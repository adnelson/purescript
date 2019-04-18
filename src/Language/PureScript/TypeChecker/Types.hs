{-# LANGUAGE NamedFieldPuns #-}

-- |
-- This module implements the type checker
--
module Language.PureScript.TypeChecker.Types
  ( BindingGroupType(..)
  , Inferred(..)
  , typesOf
  ) where

{-
  The following functions represent the corresponding type checking judgements:

    infer
      Synthesize a type for a value

    check
      Check a value has a given type

    checkProperties
      Check an object with a given type contains specified properties

    checkFunctionApplication
      Check a function of a given type returns a value of another type when applied to its arguments
-}

import Prelude.Compat
import Protolude (ordNub)

import Control.Arrow (first, second)
import Control.Monad
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State.Class (MonadState(..), gets)
import Control.Monad.Supply.Class (MonadSupply)
import Control.Monad.Writer.Class (MonadWriter(..))

import Data.Bifunctor (bimap)
import Data.Either (partitionEithers)
import Data.Functor (($>))
import Data.List (transpose, (\\), partition, delete)
import Data.Maybe (fromMaybe)
import Data.Traversable (for)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as M
import qualified Data.Set as S

import Language.PureScript.AST
import Language.PureScript.Crash
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.Traversals
import Language.PureScript.TypeChecker.Entailment
import Language.PureScript.TypeChecker.Kinds
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.TypeChecker.Skolems
import Language.PureScript.TypeChecker.Subsumption
import Language.PureScript.TypeChecker.Synonyms
import Language.PureScript.TypeChecker.TypeSearch
import Language.PureScript.TypeChecker.Unify
import Language.PureScript.Types
import Language.PureScript.Label (Label(..))
import Language.PureScript.PSString (PSString)

data BindingGroupType
  = RecursiveBindingGroup
  | NonRecursiveBindingGroup
  deriving (Show, Eq, Ord)

-- | Infer the types of multiple mutually-recursive values, and return elaborated values including
-- type class dictionaries and type annotations.
typesOf
  :: (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => BindingGroupType
  -> ModuleName
  -> [((SourceAnn, Ident), Expr)]
  -> m [((SourceAnn, Ident), Inferred)]
typesOf bindingGroupType moduleName vals = withFreshSubstitution $ do
    -- Apply the substitution that was returned from runUnify to both types and (type-annotated) values
    let
      tidyUp :: forall d1 d2 d3 d4.
             ([(d1, ((d2, Inferred), d3))], d4)
             -> Substitution -> ([(d1, ((d2, Inferred), d3))], d4)
      tidyUp ts sub = first (map (second (first (second (substituteInferred sub))))) ts

    (tys, wInfer) <- capturingSubstitution tidyUp $ do
      (SplitBindingGroup untyped typed dict, w) <- withoutWarnings $ typeDictionaryForBindingGroup (Just moduleName) vals
      ds1 <- parU typed $ \e -> withoutWarnings $ checkTypedBindingGroupElement moduleName e dict
      ds2 <- forM untyped $ \e -> withoutWarnings $ typeForBindingGroupElement e dict
      return (map (False, ) ds1 ++ map (True, ) ds2, w)


    inferred <- forM tys $ \(shouldGeneralize, ((sai@((ss, _), ident), (Inferred val ty)), _)) -> do
      -- Replace type class dictionary placeholders with actual dictionaries
      (val', unsolved) <- replaceTypeClassDictionaries shouldGeneralize val
      -- Generalize and constrain the type
      currentSubst <- gets checkSubstitution
      let ty' = substituteType currentSubst ty
          unsolvedTypeVars = ordNub $ unknownsInType ty'
          generalized = generalize unsolved ty'

      when shouldGeneralize $ do
        -- Show the inferred type in a warning
        tell
          . errorMessage' ss
          $ MissingTypeDeclaration ident generalized
        -- For non-recursive binding groups, can generalize over constraints.
        -- For recursive binding groups, we throw an error here for now.
        when (bindingGroupType == RecursiveBindingGroup && not (null unsolved))
          . throwError
          . errorMessage' ss
          $ CannotGeneralizeRecursiveFunction ident generalized
        -- Make sure any unsolved type constraints only use type variables which appear
        -- unknown in the inferred type.
        forM_ unsolved $ \(_, _, con) -> do
          -- We need information about functional dependencies, since we allow
          -- ambiguous types to be inferred if they can be solved by some functional
          -- dependency.
          let findClass = fromMaybe (internalError "entails: type class not found in environment") . M.lookup (constraintClass con)
          TypeClassData{ typeClassDependencies } <- gets (findClass . typeClasses . checkEnv)
          let solved = foldMap (S.fromList . fdDetermined) typeClassDependencies
          let constraintTypeVars = ordNub . foldMap (unknownsInType . fst) . filter ((`notElem` solved) . snd) $ zip (constraintArgs con) [0..]
          when (any (`notElem` unsolvedTypeVars) constraintTypeVars) .
            throwError
              . onErrorMessages (replaceTypes currentSubst)
              . errorMessage' ss
              $ AmbiguousTypeVariables generalized con

      -- Check skolem variables did not escape their scope
      skolemEscapeCheck val'
      return ((sai, (foldr (Abs . VarBinder nullSourceSpan . (\(x, _, _) -> x)) val' unsolved, generalized)), unsolved)

    -- Show warnings here, since types in wildcards might have been solved during
    -- instance resolution (by functional dependencies).
    finalState <- get
    let replaceTypes' = replaceTypes (checkSubstitution finalState)
        runTypeSearch' gen = runTypeSearch (guard gen $> foldMap snd inferred) finalState
        raisePreviousWarnings gen = (escalateWarningWhen isHoleError . tell . onErrorMessages (runTypeSearch' gen . replaceTypes'))

    raisePreviousWarnings False wInfer
    forM_ tys $ \(shouldGeneralize, (_, w)) ->
      raisePreviousWarnings shouldGeneralize w

    return (map (\((si, (e, t)), _) -> (si, Inferred e t)) inferred)
  where
    replaceTypes
      :: Substitution
      -> ErrorMessage
      -> ErrorMessage
    replaceTypes subst = onTypesInErrorMessage (substituteType subst)

    -- | Run type search to complete any typed hole error messages
    runTypeSearch
      :: Maybe [(Ident, InstanceContext, SourceConstraint)]
      -- ^ Any unsolved constraints which we need to continue to satisfy
      -> CheckState
      -- ^ The final type checker state
      -> ErrorMessage
      -> ErrorMessage
    runTypeSearch cons st = \case
      ErrorMessage hints (HoleInferredType x ty y (Just (TSBefore env))) ->
        let subst = checkSubstitution st
            searchResult = onTypeSearchTypes
              (substituteType subst)
              (uncurry TSAfter (typeSearch cons env st (substituteType subst ty)))
        in ErrorMessage hints (HoleInferredType x ty y (Just searchResult))
      other -> other

    -- | Generalize type vars using forall and add inferred constraints
    generalize unsolved = varIfUnknown . constrain unsolved

    -- | Add any unsolved constraints
    constrain cs ty = foldr srcConstrainedType ty (map (\(_, _, x) -> x) cs)

    isHoleError :: ErrorMessage -> Bool
    isHoleError (ErrorMessage _ HoleInferredType{}) = True
    isHoleError _ = False

-- | A binding group contains multiple value definitions, some of which are typed
-- and some which are not.
--
-- This structure breaks down a binding group into typed and untyped parts.
data SplitBindingGroup = SplitBindingGroup
  { _splitBindingGroupUntyped :: [((SourceAnn, Ident), Inferred)]
  -- ^ The untyped expressions
  , _splitBindingGroupTyped :: [((SourceAnn, Ident), (Expr, SourceType, Bool))]
  -- ^ The typed expressions, along with their type annotations
  , _splitBindingGroupNames :: M.Map (Qualified Ident) (SourceType, NameKind, NameVisibility)
  -- ^ A map containing all expressions and their assigned types (which might be
  -- fresh unification variables). These will be added to the 'Environment' after
  -- the binding group is checked, so the value type of the 'Map' is chosen to be
  -- compatible with the type of 'bindNames'.
  }

-- | This function breaks a binding group down into two sets of declarations:
-- those which contain type annotations, and those which don't.
-- This function also generates fresh unification variables for the types of
-- declarations without type annotations, returned in the 'UntypedData' structure.
typeDictionaryForBindingGroup
  :: (MonadState CheckState m, MonadWriter MultipleErrors m)
  => Maybe ModuleName
  -> [((SourceAnn, Ident), Expr)]
  -> m SplitBindingGroup
typeDictionaryForBindingGroup moduleName vals = do
    -- Filter the typed and untyped declarations and make a map of names to typed declarations.
    -- Replace type wildcards here so that the resulting dictionary of types contains the
    -- fully expanded types.
    let (untyped, typed) = partitionEithers (map splitTypeAnnotation vals)
    (typedDict, typed') <- fmap unzip . for typed $ \(sai, (expr, ty, checkType)) -> do
      ty' <- replaceTypeWildcards ty
      return ((sai, ty'), (sai, (expr, ty', checkType)))
    -- Create fresh unification variables for the types of untyped declarations
    (untypedDict, untyped') <- fmap unzip . for untyped $ \(sai, expr) -> do
      ty <- freshType
      return ((sai, ty), (sai, Inferred expr ty))
    -- Create the dictionary of all name/type pairs, which will be added to the
    -- environment during type checking
    let dict = M.fromList [ (Qualified moduleName ident, (ty, Private, Undefined))
                          | ((_, ident), ty) <- typedDict <> untypedDict
                          ]
    return (SplitBindingGroup untyped' typed' dict)
  where
    -- | Check if a value contains a type annotation, and if so, separate it
    -- from the value itself.
    splitTypeAnnotation :: (a, Expr) -> Either (a, Expr) (a, (Expr, SourceType, Bool))
    splitTypeAnnotation (a, TypedValue checkType value ty) = Right (a, (value, ty, checkType))
    splitTypeAnnotation (a, PositionedValue pos c value) =
      bimap (second (PositionedValue pos c))
            (second (\(e, t, b) -> (PositionedValue pos c e, t, b)))
            (splitTypeAnnotation (a, value))
    splitTypeAnnotation (a, value) = Left (a, value)

-- | Check the type annotation of a typed value in a binding group.
checkTypedBindingGroupElement
  :: (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => ModuleName
  -> ((SourceAnn, Ident), (Expr, SourceType, Bool))
  -- ^ The identifier we are trying to define, along with the expression and its type annotation
  -> M.Map (Qualified Ident) (SourceType, NameKind, NameVisibility)
  -- ^ Names brought into scope in this binding group
  -> m ((SourceAnn, Ident), Inferred)
checkTypedBindingGroupElement mn (ident, (val, ty, checkType)) dict = do
  -- Kind check
  (kind, args) <- kindOfWithScopedVars ty
  checkTypeKind ty kind
  -- We replace type synonyms _after_ kind-checking, since we don't want type
  -- synonym expansion to bring type variables into scope. See #2542.
  ty' <- introduceSkolemScope <=< replaceAllTypeSynonyms $ ty
  -- Check the type with the new names in scope
  inf <- if checkType
            then withScopedTypeVars mn args $ bindNames dict $ check val ty'
            else return (Inferred val ty')
  return (ident, inf)

-- | Infer a type for a value in a binding group which lacks an annotation.
typeForBindingGroupElement
  :: (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => ((SourceAnn, Ident), Inferred)
  -- ^ The identifier we are trying to define, along with the expression and its assigned type
  -- (at this point, this should be a unification variable)
  -> M.Map (Qualified Ident) (SourceType, NameKind, NameVisibility)
  -- ^ Names brought into scope in this binding group
  -> m ((SourceAnn, Ident), Inferred)
typeForBindingGroupElement (ident, Inferred val ty) dict = do
  -- Infer the type with the new names in scope
  Inferred val' ty' <- bindNames dict $ infer val
  -- Unify the type with the unification variable we chose for this definition
  unifyTypes ty ty'
  return (ident, Inferred val' ty')

-- | Check the kind of a type, failing if it is not of kind *.
checkTypeKind
  :: MonadError MultipleErrors m
  => SourceType
  -> SourceKind
  -> m ()
checkTypeKind ty kind = guardWith (errorMessage (ExpectedType ty kind)) $ isKindType kind

-- | Remove any ForAlls and ConstrainedType constructors in a type by introducing new unknowns
-- or TypeClassDictionary values.
--
-- This is necessary during type checking to avoid unifying a polymorphic type with a
-- unification variable.
instantiatePolyTypeWithUnknowns
  :: (MonadState CheckState m, MonadError MultipleErrors m)
  => Inferred -> m Inferred
instantiatePolyTypeWithUnknowns (Inferred val (ForAll _ ident ty _)) = do
  ty' <- replaceVarWithUnknown ident ty
  instantiatePolyTypeWithUnknowns (Inferred val ty')
instantiatePolyTypeWithUnknowns (Inferred val (ConstrainedType _ con ty)) = do
   dicts <- getTypeClassDictionaries
   hints <- getHints
   let inf = Inferred (App val (TypeClassDictionary con dicts hints)) ty
   instantiatePolyTypeWithUnknowns inf
instantiatePolyTypeWithUnknowns inferred = pure inferred

-- | Infer a type for a value, rethrowing any error to provide a more useful error message
infer
  :: (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => Expr
  -> m Inferred
infer val = withErrorMessageHint (ErrorInferringType val) (infer' val)

data Inferred = Inferred {infExpr :: Expr, infType :: SourceType}

inferredToTuple :: Inferred -> (Expr, SourceType)
inferredToTuple (Inferred e t) = (e, t)

inferredToExpr :: Inferred -> Expr
inferredToExpr (Inferred e t) = TypedValue True e t

substituteInferred :: Substitution -> Inferred -> Inferred
substituteInferred sub (Inferred e t) = Inferred (overTypes doSub e) (doSub t)
  where doSub = substituteType sub

-- | Infer a type for a value
infer'
  :: forall m
   . (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => Expr
  -> m Inferred
infer' v@(Literal _ (NumericLiteral (Left _))) = pure $ Inferred v tyInt
infer' v@(Literal _ (NumericLiteral (Right _))) = pure $ Inferred v tyNumber
infer' v@(Literal _ (StringLiteral _)) = pure $ Inferred v tyString
infer' v@(Literal _ (CharLiteral _)) = pure $ Inferred v tyChar
infer' v@(Literal _ (BooleanLiteral _)) = pure $ Inferred v tyBoolean
infer' (Literal ss (ArrayLiteral vals)) = do
  ts <- traverse infer vals
  els <- freshType
  ts' <- forM ts $ \inf -> do
    inf' <- instantiatePolyTypeWithUnknowns inf
    unifyTypes els (infType inf')
    return inf'
  let arrayLit = ArrayLiteral (map inferredToExpr ts')
  pure $ Inferred (Literal ss arrayLit) (srcTypeApp tyArray els)
infer' (Literal ss (ObjectLiteral ps)) = do
  ensureNoDuplicateProperties ps
  -- We make a special case for Vars in record labels, since these are the
  -- only types of expressions for which 'infer' can return a polymorphic type.
  -- They need to be instantiated here.
  let shouldInstantiate :: Expr -> Bool
      shouldInstantiate Var{} = True
      shouldInstantiate (PositionedValue _ _ e) = shouldInstantiate e
      shouldInstantiate _ = False

      inferProperty :: (PSString, Expr) -> m (PSString, Inferred)
      inferProperty (name, val) = do
        inf <- infer val
        inf' <- case shouldInstantiate val of
          True -> instantiatePolyTypeWithUnknowns inf
          False -> pure inf
        pure (name, inf')

      toRowListItem (lbl, (Inferred _ ty)) = srcRowListItem (Label lbl) ty

  fields <- forM ps inferProperty
  let ty = srcTypeApp tyRecord $ rowFromList (map toRowListItem fields, srcREmpty)
  pure $ Inferred (Literal ss (ObjectLiteral (map (fmap inferredToExpr) fields))) ty
infer' (ObjectUpdate o ps) = do
  ensureNoDuplicateProperties ps
  row <- freshType
  newVals' <- zipWith (\(name, _) t -> (name, t)) ps <$> traverse (infer . snd) ps
  let newVals = map (inferredToExpr <$>) newVals'
  let toRowListItem = uncurry srcRowListItem
  let newTys = map (\(name, Inferred _ ty) -> (Label name, ty)) newVals'
  oldTys <- zip (map (Label . fst) ps) <$> replicateM (length ps) freshType
  let oldTy = srcTypeApp tyRecord $ rowFromList (toRowListItem <$> oldTys, row)
  o' <- inferredToExpr <$> check o oldTy
  pure $ Inferred (ObjectUpdate o' newVals) $ srcTypeApp tyRecord $ rowFromList (toRowListItem <$> newTys, row)
infer' (Accessor prop val) = withErrorMessageHint (ErrorCheckingAccessor val prop) $ do
  field <- freshType
  rest <- freshType
  typed <- inferredToExpr <$> check val (srcTypeApp tyRecord (srcRCons (Label prop) field rest))
  pure $ Inferred (Accessor prop typed) field
infer' (Abs binder ret)
  | VarBinder ss arg <- binder = do
      ty <- freshType
      withBindingGroupVisible $ bindLocalVariables [(arg, ty, Defined)] $ do
        body <- infer' ret
        Inferred body' bodyTy' <- instantiatePolyTypeWithUnknowns body
        pure $ Inferred (Abs (VarBinder ss arg) body') (function ty bodyTy')
  | otherwise = internalError "Binder was not desugared"
infer' (App f arg) = do
  f'@(Inferred _ ft) <- infer f
  checkFunctionApplication (inferredToExpr f') ft arg
infer' (Var ss var) = do
  checkVisibility var
  ty <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards <=< lookupVariable $ var
  case ty of
    ConstrainedType _ con ty' -> do
      dicts <- getTypeClassDictionaries
      hints <- getHints
      pure $ Inferred (App (Var ss var) (TypeClassDictionary con dicts hints)) ty'
    _ -> pure $ Inferred (Var ss var) ty
infer' v@(Constructor _ c) = do
  env <- getEnv
  case M.lookup c (dataConstructors env) of
    Nothing -> throwError . errorMessage . UnknownName . fmap DctorName $ c
    Just (_, _, ty, _) -> do
      let instantiate t = inferredToTuple <$> instantiatePolyTypeWithUnknowns (Inferred v t)
      (v', ty') <- sndM (introduceSkolemScope <=< replaceAllTypeSynonyms) <=< instantiate $ ty
      pure $ Inferred v' ty'
infer' (Case vals binders) = do
  (vals', ts) <- instantiateForBinders vals binders
  ret <- freshType
  binders' <- checkBinders ts ret binders
  pure $ Inferred (Case vals' binders') ret
infer' (IfThenElse cond th el) = do
  cond' <- inferredToExpr <$> check cond tyBoolean
  th' <- infer th
  el' <- infer el
  Inferred th'' thTy' <- instantiatePolyTypeWithUnknowns th'
  Inferred el'' elTy' <- instantiatePolyTypeWithUnknowns el'
  unifyTypes thTy' elTy'
  pure $ Inferred (IfThenElse cond' th'' el'') thTy'
infer' (Let w ds val) = do
  (ds', val'@(Inferred _ valTy)) <- inferLetBinding [] ds val infer
  pure $ Inferred (Let w ds' (inferredToExpr val')) valTy
infer' (DeferredDictionary className tys) = do
  dicts <- getTypeClassDictionaries
  hints <- getHints
  return $ Inferred
             (TypeClassDictionary (srcConstraint className tys Nothing) dicts hints)
             (foldl srcTypeApp (srcTypeConstructor (fmap coerceProperName className)) tys)
infer' (TypedValue checkType val ty) = do
  moduleName <- unsafeGetCurrentModule
  (kind, args) <- kindOfWithScopedVars ty
  checkTypeKind ty kind
  ty' <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards $ ty
  case checkType of
    False -> pure $ Inferred val ty'
    True -> withScopedTypeVars moduleName args (check val ty')
infer' (Hole name) = do
  ty <- freshType
  ctx <- getLocalContext
  env <- getEnv
  tell . errorMessage $ HoleInferredType name ty ctx . Just $ TSBefore env
  pure $ Inferred (Hole name) ty
infer' (PositionedValue pos c val) = warnAndRethrowWithPositionTC pos $ do
  Inferred v ty <- infer' val
  return $ Inferred (PositionedValue pos c v) ty
infer' v = internalError $ "Invalid argument to infer: " ++ show v

inferLetBinding
  :: (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => [Declaration]
  -> [Declaration]
  -> Expr
  -> (Expr -> m Inferred)
  -> m ([Declaration], Inferred)
inferLetBinding seen [] ret j = (,) seen <$> withBindingGroupVisible (j ret)
inferLetBinding seen (ValueDecl sa@(ss, _) ident nameKind [] [MkUnguarded tv@(TypedValue checkType val ty)] : rest) ret j = do
  moduleName <- unsafeGetCurrentModule
  Inferred val' ty'' <- warnAndRethrowWithPositionTC ss $ do
    (kind, args) <- kindOfWithScopedVars ty
    checkTypeKind ty kind
    let dict = M.singleton (Qualified Nothing ident) (ty, nameKind, Undefined)
    ty' <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards $ ty
    case checkType of
      True -> withScopedTypeVars moduleName args (bindNames dict (check val ty'))
      _ -> pure $ Inferred tv ty'
  bindNames (M.singleton (Qualified Nothing ident) (ty'', nameKind, Defined))
    $ inferLetBinding (seen ++ [ValueDecl sa ident nameKind [] [MkUnguarded (TypedValue checkType val' ty'')]]) rest ret j
inferLetBinding seen (ValueDecl sa@(ss, _) ident nameKind [] [MkUnguarded val] : rest) ret j = do
  valTy <- freshType
  Inferred val' valTy' <- warnAndRethrowWithPositionTC ss $ do
    let dict = M.singleton (Qualified Nothing ident) (valTy, nameKind, Undefined)
    bindNames dict $ infer val
  warnAndRethrowWithPositionTC ss $ unifyTypes valTy valTy'
  bindNames (M.singleton (Qualified Nothing ident) (valTy', nameKind, Defined))
    $ inferLetBinding (seen ++ [ValueDecl sa ident nameKind [] [MkUnguarded val']]) rest ret j
inferLetBinding seen (BindingGroupDeclaration ds : rest) ret j = do
  moduleName <- unsafeGetCurrentModule
  SplitBindingGroup untyped typed dict <- typeDictionaryForBindingGroup Nothing . NEL.toList $ fmap (\(i, _, v) -> (i, v)) ds
  ds1' <- parU typed $ \e -> checkTypedBindingGroupElement moduleName e dict
  ds2' <- forM untyped $ \e -> typeForBindingGroupElement e dict
  let ds' = NEL.fromList [(ident, Private, val') | (ident, (Inferred val' _)) <- ds1' ++ ds2']
  bindNames dict $ do
    makeBindingGroupVisible
    inferLetBinding (seen ++ [BindingGroupDeclaration ds']) rest ret j
inferLetBinding _ _ _ _ = internalError "Invalid argument to inferLetBinding"

-- | Infer the types of variables brought into scope by a binder
inferBinder
  :: forall m
   . (MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => SourceType
  -> Binder
  -> m (M.Map Ident SourceType)
inferBinder _ NullBinder = return M.empty
inferBinder val (LiteralBinder _ (StringLiteral _)) = unifyTypes val tyString >> return M.empty
inferBinder val (LiteralBinder _ (CharLiteral _)) = unifyTypes val tyChar >> return M.empty
inferBinder val (LiteralBinder _ (NumericLiteral (Left _))) = unifyTypes val tyInt >> return M.empty
inferBinder val (LiteralBinder _ (NumericLiteral (Right _))) = unifyTypes val tyNumber >> return M.empty
inferBinder val (LiteralBinder _ (BooleanLiteral _)) = unifyTypes val tyBoolean >> return M.empty
inferBinder val (VarBinder _ name) = return $ M.singleton name val
inferBinder val (ConstructorBinder ss ctor binders) = do
  env <- getEnv
  case M.lookup ctor (dataConstructors env) of
    Just (_, _, ty, _) -> do
      Inferred _ fn <- instantiatePolyTypeWithUnknowns $
        Inferred (internalError "Data constructor types cannot contain constraints") ty
      fn' <- introduceSkolemScope <=< replaceAllTypeSynonyms $ fn
      let (args, ret) = peelArgs fn'
          expected = length args
          actual = length binders
      unless (expected == actual) . throwError . errorMessage' ss $ IncorrectConstructorArity ctor expected actual
      unifyTypes ret val
      M.unions <$> zipWithM inferBinder (reverse args) binders
    _ -> throwError . errorMessage' ss . UnknownName . fmap DctorName $ ctor
  where
  peelArgs :: Type a -> ([Type a], Type a)
  peelArgs = go []
    where
    go args (TypeApp _ (TypeApp _ fn arg) ret) | eqType fn tyFunction = go (arg : args) ret
    go args ret = (args, ret)
inferBinder val (LiteralBinder _ (ObjectLiteral props)) = do
  row <- freshType
  rest <- freshType
  m1 <- inferRowProperties row rest props
  unifyTypes val (srcTypeApp tyRecord row)
  return m1
  where
  inferRowProperties :: SourceType -> SourceType -> [(PSString, Binder)] -> m (M.Map Ident SourceType)
  inferRowProperties nrow row [] = unifyTypes nrow row >> return M.empty
  inferRowProperties nrow row ((name, binder):binders) = do
    propTy <- freshType
    m1 <- inferBinder propTy binder
    m2 <- inferRowProperties nrow (srcRCons (Label name) propTy row) binders
    return $ m1 `M.union` m2
inferBinder val (LiteralBinder _ (ArrayLiteral binders)) = do
  el <- freshType
  m1 <- M.unions <$> traverse (inferBinder el) binders
  unifyTypes val (srcTypeApp tyArray el)
  return m1
inferBinder val (NamedBinder ss name binder) =
  warnAndRethrowWithPositionTC ss $ do
    m <- inferBinder val binder
    return $ M.insert name val m
inferBinder val (PositionedBinder pos _ binder) =
  warnAndRethrowWithPositionTC pos $ inferBinder val binder
inferBinder val (TypedBinder ty binder) = do
  kind <- kindOf ty
  checkTypeKind ty kind
  ty1 <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards $ ty
  unifyTypes val ty1
  inferBinder ty1 binder
inferBinder _ OpBinder{} =
  internalError "OpBinder should have been desugared before inferBinder"
inferBinder _ BinaryNoParensBinder{} =
  internalError "BinaryNoParensBinder should have been desugared before inferBinder"
inferBinder _ ParensInBinder{} =
  internalError "ParensInBinder should have been desugared before inferBinder"

-- | Returns true if a binder requires its argument type to be a monotype.
-- | If this is the case, we need to instantiate any polymorphic types before checking binders.
binderRequiresMonotype :: Binder -> Bool
binderRequiresMonotype NullBinder = False
binderRequiresMonotype (VarBinder _ _) = False
binderRequiresMonotype (NamedBinder _ _ b) = binderRequiresMonotype b
binderRequiresMonotype (PositionedBinder _ _ b) = binderRequiresMonotype b
binderRequiresMonotype (TypedBinder ty b) = isMonoType ty || binderRequiresMonotype b
binderRequiresMonotype _ = True

-- | Instantiate polytypes only when necessitated by a binder.
instantiateForBinders
  :: (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => [Expr]
  -> [CaseAlternative]
  -> m ([Expr], [SourceType])
instantiateForBinders vals cases = unzip <$> zipWithM go vals shouldInstantiates
  where
  go val shouldInstantiate = do
    inf@(Inferred val' ty) <- infer val
    case shouldInstantiate of
      False -> pure (val', ty)
      True -> inferredToTuple <$> instantiatePolyTypeWithUnknowns inf

  shouldInstantiates :: [Bool]
  shouldInstantiates = map (any binderRequiresMonotype) . transpose . map caseAlternativeBinders $ cases

-- |
-- Check the types of the return values in a set of binders in a case statement
--
checkBinders
  :: (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => [SourceType]
  -> SourceType
  -> [CaseAlternative]
  -> m [CaseAlternative]
checkBinders _ _ [] = return []
checkBinders nvals ret (CaseAlternative binders result : bs) = do
  guardWith (errorMessage $ OverlappingArgNames Nothing) $
    let ns = concatMap binderNames binders in length (ordNub ns) == length ns
  m1 <- M.unions <$> zipWithM inferBinder nvals binders
  r <- bindLocalVariables [ (name, ty, Defined) | (name, ty) <- M.toList m1 ] $
       CaseAlternative binders <$> forM result (\ge -> checkGuardedRhs ge ret)
  rs <- checkBinders nvals ret bs
  return $ r : rs

checkGuardedRhs
  :: (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => GuardedExpr
  -> SourceType
  -> m GuardedExpr
checkGuardedRhs (GuardedExpr [] rhs) ret = do
  GuardedExpr [] . inferredToExpr <$> check rhs ret
checkGuardedRhs (GuardedExpr (ConditionGuard cond : guards) rhs) ret = do
  cond' <- inferredToExpr <$> withErrorMessageHint ErrorCheckingGuard (check cond tyBoolean)
  GuardedExpr guards' rhs' <- checkGuardedRhs (GuardedExpr guards rhs) ret
  return $ GuardedExpr (ConditionGuard cond' : guards') rhs'
checkGuardedRhs (GuardedExpr (PatternGuard binder expr : guards) rhs) ret = do
  inf@(Inferred _ ty) <- infer expr
  variables <- inferBinder ty binder
  GuardedExpr guards' rhs' <- bindLocalVariables [ (name, bty, Defined)
                                                 | (name, bty) <- M.toList variables
                                                 ] $
    checkGuardedRhs (GuardedExpr guards rhs) ret
  return $ GuardedExpr (PatternGuard binder (inferredToExpr inf) : guards') rhs'

-- |
-- Check the type of a value, rethrowing errors to provide a better error message
--
check
  :: (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => Expr
  -> SourceType
  -> m Inferred
check val ty = withErrorMessageHint (ErrorCheckingType val ty) $ check' val ty

-- |
-- Check the type of a value
--
check'
  :: forall m
   . (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => Expr
  -> SourceType
  -> m Inferred
check' val (ForAll ann ident ty _) = do
  scope <- newSkolemScope
  sko <- newSkolemConstant
  let ss = case val of
             PositionedValue pos c _ -> (pos, c)
             _ -> NullSourceAnn
      sk = skolemize ss ident sko scope ty
      skVal = skolemizeTypesInValue ss ident sko scope val
  val' <- inferredToExpr <$> check skVal sk
  pure $ Inferred val' (ForAll ann ident ty (Just scope))
check' val t@(ConstrainedType _ con@(Constraint _ (Qualified _ (ProperName className)) _ _) ty) = do
  dictName <- freshIdent ("dict" <> className)
  dicts <- newDictionaries [] (Qualified Nothing dictName) con
  val' <- withBindingGroupVisible $ withTypeClassDictionaries dicts $ check val ty
  pure $ Inferred (Abs (VarBinder nullSourceSpan dictName) (inferredToExpr val')) t
check' val u@(TUnknown _ _) = do
  -- Don't unify an unknown with an inferred polytype
  Inferred val'' ty' <- instantiatePolyTypeWithUnknowns =<< infer val
  unifyTypes ty' u
  pure (Inferred val'' ty')
check' v@(Literal _ (NumericLiteral (Left _))) t | t == tyInt =
  pure $ Inferred v t
check' v@(Literal _ (NumericLiteral (Right _))) t | t == tyNumber =
  pure $ Inferred v t
check' v@(Literal _ (StringLiteral _)) t | t == tyString =
  pure $ Inferred v t
check' v@(Literal _ (CharLiteral _)) t | t == tyChar =
  pure $ Inferred v t
check' v@(Literal _ (BooleanLiteral _)) t | t == tyBoolean =
  pure $ Inferred v t
check' (Literal ss (ArrayLiteral vals)) t@(TypeApp _ a ty) = do
  unifyTypes a tyArray
  array <- Literal ss . ArrayLiteral <$> forM vals (\e -> inferredToExpr <$> check e ty)
  pure $ Inferred array t
check' (Abs binder ret) ty@(TypeApp _ (TypeApp _ t argTy) retTy)
  | VarBinder ss arg <- binder = do
      unifyTypes t tyFunction
      ret' <- withBindingGroupVisible $ bindLocalVariables [(arg, argTy, Defined)] $ check ret retTy
      pure $ Inferred (Abs (VarBinder ss arg) (inferredToExpr ret')) ty
  | otherwise = internalError "Binder was not desugared"
check' (App f arg) ret = do
  f'@(Inferred _ ft) <- infer f
  Inferred app retTy <- checkFunctionApplication (inferredToExpr f') ft arg
  elaborate <- subsumes retTy ret
  pure $ Inferred (elaborate app) ret
check' v@(Var _ var) ty = do
  checkVisibility var
  repl <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< lookupVariable $ var
  ty' <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards $ ty
  elaborate <- subsumes repl ty'
  pure $ Inferred (elaborate v) ty'
check' (DeferredDictionary className tys) ty = do
  {-
  -- Here, we replace a placeholder for a superclass dictionary with a regular
  -- TypeClassDictionary placeholder. The reason we do this is that it is necessary to have the
  -- correct super instance dictionaries in scope, and these are not available when the type class
  -- declaration gets desugared.
  -}
  dicts <- getTypeClassDictionaries
  hints <- getHints
  return $ Inferred
             (TypeClassDictionary (srcConstraint className tys Nothing) dicts hints)
             ty
check' (TypedValue checkType val ty1) ty2 = do
  kind <- kindOf ty1
  checkTypeKind ty1 kind
  ty1' <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards $ ty1
  ty2' <- introduceSkolemScope <=< replaceAllTypeSynonyms <=< replaceTypeWildcards $ ty2
  elaborate <- subsumes ty1' ty2'
  val' <- if checkType
            then inferredToExpr <$> check val ty1'
            else pure val
  pure $ Inferred (TypedValue checkType (elaborate val') ty1') ty2'
check' (Case vals binders) ret = do
  (vals', ts) <- instantiateForBinders vals binders
  binders' <- checkBinders ts ret binders
  pure $ Inferred (Case vals' binders') ret
check' (IfThenElse cond th el) ty = do
  cond' <- inferredToExpr <$> check cond tyBoolean
  th' <- inferredToExpr <$> check th ty
  el' <- inferredToExpr <$> check el ty
  pure $ Inferred (IfThenElse cond' th' el') ty
check' e@(Literal ss (ObjectLiteral ps)) t@(TypeApp _ obj row) | obj == tyRecord = do
  ensureNoDuplicateProperties ps
  ps' <- checkProperties e ps row False
  pure $ Inferred (Literal ss (ObjectLiteral ps')) t
check' (TypeClassDictionaryConstructorApp name ps) t = do
  ps' <- inferredToExpr <$> check' ps t
  pure $ Inferred (TypeClassDictionaryConstructorApp name ps') t
check' e@(ObjectUpdate obj ps) t@(TypeApp _ o row) | o == tyRecord = do
  ensureNoDuplicateProperties ps
  -- We need to be careful to avoid duplicate labels here.
  -- We check _obj_ against the type _t_ with the types in _ps_ replaced with unknowns.
  let (propsToCheck, rest) = rowToList row
      (removedProps, remainingProps) = partition (\(RowListItem _ p _) -> p `elem` map (Label . fst) ps) propsToCheck
  us <- zipWith srcRowListItem (map rowListLabel removedProps) <$> replicateM (length ps) freshType
  obj' <- inferredToExpr <$> check obj (srcTypeApp tyRecord (rowFromList (us ++ remainingProps, rest)))
  ps' <- checkProperties e ps row True
  pure $ Inferred (ObjectUpdate obj' ps') t
check' (Accessor prop val) ty = withErrorMessageHint (ErrorCheckingAccessor val prop) $ do
  rest <- freshType
  val' <- inferredToExpr <$> check val (srcTypeApp tyRecord (srcRCons (Label prop) ty rest))
  pure $ Inferred (Accessor prop val') ty
check' v@(Constructor _ c) ty = do
  env <- getEnv
  case M.lookup c (dataConstructors env) of
    Nothing -> throwError . errorMessage . UnknownName . fmap DctorName $ c
    Just (_, _, ty1, _) -> do
      repl <- introduceSkolemScope <=< replaceAllTypeSynonyms $ ty1
      ty' <- introduceSkolemScope ty
      elaborate <- subsumes repl ty'
      pure $ Inferred (elaborate v) ty'
check' (Let w ds val) ty = do
  (ds', inferred) <- inferLetBinding [] ds val (`check` ty)
  pure $ Inferred (Let w ds' (inferredToExpr inferred)) ty
check' val kt@(KindedType _ ty kind) = do
  checkTypeKind ty kind
  val' <- check' val ty
  pure $ Inferred (inferredToExpr val') kt
check' (PositionedValue pos c val) ty = warnAndRethrowWithPositionTC pos $ do
  Inferred v ty' <- check' val ty
  return $ Inferred (PositionedValue pos c v) ty'
check' val ty = do
  Inferred val' ty' <- infer val
  elaborate <- subsumes ty' ty
  pure $ Inferred (elaborate val') ty

-- |
-- Check the type of a collection of named record fields
--
-- The @lax@ parameter controls whether or not every record member has to be provided. For object updates, this is not the case.
--
checkProperties
  :: (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => Expr
  -> [(PSString, Expr)]
  -> SourceType
  -> Bool
  -> m [(PSString, Expr)]
checkProperties expr ps row lax = let (ts, r') = rowToList row in go ps (toRowPair <$> ts) r' where
  toRowPair (RowListItem _ lbl ty) = (lbl, ty)
  go [] [] (REmpty _) = return []
  go [] [] u@(TUnknown _ _)
    | lax = return []
    | otherwise = do unifyTypes u srcREmpty
                     return []
  go [] [] Skolem{} | lax = return []
  go [] ((p, _): _) _ | lax = return []
                      | otherwise = throwError . errorMessage $ PropertyIsMissing p
  go ((p,_):_) [] (REmpty _) = throwError . errorMessage $ AdditionalProperty $ Label p
  go ((p,v):ps') ts r =
    case lookup (Label p) ts of
      Nothing -> do
        inf@(Inferred _ ty) <- infer v
        rest <- freshType
        unifyTypes r (srcRCons (Label p) ty rest)
        ps'' <- go ps' ts rest
        return $ (p, inferredToExpr inf) : ps''
      Just ty -> do
        v' <- inferredToExpr <$> check v ty
        ps'' <- go ps' (delete (Label p, ty) ts) r
        return $ (p, v') : ps''
  go _ _ _ = throwError . errorMessage $ ExprDoesNotHaveType expr (srcTypeApp tyRecord row)

-- | Check the type of a function application, rethrowing errors to provide a better error message.
--
-- This judgment takes three inputs:
--
-- * The expression of the function we are applying
-- * The type of that function
-- * The expression we are applying it to
--
-- and synthesizes two outputs:
--
-- * The return type
-- * The elaborated expression for the function application (since we might need to
--   insert type class dictionaries, etc.)
checkFunctionApplication
  :: (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => Expr
  -- ^ The function expression
  -> SourceType
  -- ^ The type of the function
  -> Expr
  -- ^ The argument expression
  -> m Inferred
  -- ^ The result type, and the elaborated term
checkFunctionApplication fn fnTy arg = withErrorMessageHint (ErrorInApplication fn fnTy arg) $ do
  subst <- gets checkSubstitution
  checkFunctionApplication' fn (substituteType subst fnTy) arg

-- | Check the type of a function application
checkFunctionApplication'
  :: (MonadSupply m, MonadState CheckState m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => Expr
  -> SourceType
  -> Expr
  -> m Inferred
checkFunctionApplication' fn (TypeApp _ (TypeApp _ tyFunction' argTy) retTy) arg = do
  unifyTypes tyFunction' tyFunction
  arg' <- inferredToExpr <$> check arg argTy
  pure $ Inferred (App fn arg') retTy
checkFunctionApplication' fn (ForAll _ ident ty _) arg = do
  replaced <- replaceVarWithUnknown ident ty
  checkFunctionApplication fn replaced arg
checkFunctionApplication' fn (KindedType _ ty _) arg =
  checkFunctionApplication fn ty arg
checkFunctionApplication' fn (ConstrainedType _ con fnTy) arg = do
  dicts <- getTypeClassDictionaries
  hints <- getHints
  checkFunctionApplication' (App fn (TypeClassDictionary con dicts hints)) fnTy arg
checkFunctionApplication' fn fnTy dict@TypeClassDictionary{} =
  pure $ Inferred (App fn dict) fnTy
checkFunctionApplication' fn u arg = do
  inf <- instantiatePolyTypeWithUnknowns =<< infer arg
  returnType <- freshType
  unifyTypes u (function (infType inf) returnType)
  pure $ Inferred (App fn (inferredToExpr inf)) returnType

  -- arg'@(_, ty) <- do
  --   Inferred arg' t <- infer arg
  --   instantiatePolyTypeWithUnknowns arg' t
  -- ret <- freshType
  -- unifyTypes u (function ty ret)
  -- pure $ Inferred (App fn (inferredToExpr arg')) ret

-- |
-- Ensure a set of property names and value does not contain duplicate labels
--
ensureNoDuplicateProperties :: (MonadError MultipleErrors m) => [(PSString, Expr)] -> m ()
ensureNoDuplicateProperties ps =
  let ls = map fst ps in
  case ls \\ ordNub ls of
    l : _ -> throwError . errorMessage $ DuplicateLabel (Label l) Nothing
    _ -> return ()
