module Language.PureScript.CoreFn.Desugar (moduleToCoreFn, moduleToFullyAnnotated) where

import Prelude.Compat
import Protolude (ordNub)

import Control.Arrow (second)
import Control.Monad (forM)

import Data.Function (on)
import Data.List (sort, sortBy)
import Data.Maybe (mapMaybe)
import Data.Tuple (swap)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as M
import Debug.Trace (traceM)

import Language.PureScript.AST.Literals
import Language.PureScript.AST.SourcePos
import Language.PureScript.AST.Traversals
import Language.PureScript.Comments
import Language.PureScript.CoreFn.Ann
import Language.PureScript.CoreFn.Binders
import Language.PureScript.CoreFn.Expr
import Language.PureScript.CoreFn.Meta
import Language.PureScript.CoreFn.Module
import Language.PureScript.Crash
import Language.PureScript.Environment
import Language.PureScript.Names
import Language.PureScript.Sugar.TypeClasses (typeClassMemberName, superClassDictionaryNames)
import Language.PureScript.Types
import Language.PureScript.PSString (mkString)
import qualified Language.PureScript.AST as A

data Ann' = Ann' SourceSpan [Comment] (Maybe Meta) SourceType
  deriving (Show, Eq)

toFullyAnnotated :: Ann -> Maybe Ann'
toFullyAnnotated (ss, cs, st, meta) = do
  traceM "hey!"
  Ann' ss cs meta <$> st

exprToFullyAnnotated :: Expr Ann -> Maybe (Expr Ann')
exprToFullyAnnotated e = case exprToFullyAnnotated' e of
  Nothing -> do
    traceM ("expression " ++ show e ++ " has no annotation")
    Nothing
  Just e' -> pure e'


exprToFullyAnnotated' :: Expr Ann -> Maybe (Expr Ann')
exprToFullyAnnotated' = \case
  Literal a lit -> do
    Literal <$> toFullyAnnotated a <*> mapM exprToFullyAnnotated lit
  Constructor a tname cname idents -> do
    a' <- toFullyAnnotated a
    pure $ Constructor a' tname cname idents
  Accessor a field e -> do
    a' <- toFullyAnnotated a
    e' <- exprToFullyAnnotated e
    pure $ Accessor a' field e'
  ObjectUpdate a e updates -> do
    a' <- toFullyAnnotated a
    e' <- exprToFullyAnnotated e
    updates' <- forM updates $ \(f, e'') -> (f,) <$> exprToFullyAnnotated e''
    pure $ ObjectUpdate a' e' updates'
  Abs a ident e -> do
    a' <- toFullyAnnotated a
    e' <- exprToFullyAnnotated e
    pure $ Abs a' ident e'
  App a e e' -> App <$> toFullyAnnotated a <*> exprToFullyAnnotated e <*> exprToFullyAnnotated e'
  Var a ident -> flip Var ident <$> toFullyAnnotated a
  Case a exprs alts -> do
    a' <- toFullyAnnotated a
    exprs' <- mapM exprToFullyAnnotated exprs
    alts' <- forM alts $ \(CaseAlternative binders result) -> do
      binders' <- mapM (traverse toFullyAnnotated) binders
      result' <- case result of
        Left guardsAndExprs -> do
          fmap Left $ forM guardsAndExprs $ \(g, e) -> do
            (,) <$> exprToFullyAnnotated g <*> exprToFullyAnnotated e
        Right expr -> Right <$> exprToFullyAnnotated expr
      pure $ CaseAlternative binders' result'
    pure $ Case a' exprs' alts'
  Let a binds expr -> do
    a' <- toFullyAnnotated a
    binds' <- forM binds bindToFullyAnnotated
    expr' <- exprToFullyAnnotated expr
    pure $ Let a' binds' expr'

bindToFullyAnnotated :: Bind Ann -> Maybe (Bind Ann')
bindToFullyAnnotated = \case
  NonRec a ident e -> do
    flip NonRec ident <$> toFullyAnnotated a <*> exprToFullyAnnotated e
  Rec group -> fmap Rec $ forM group $ \((a, ident), e) -> do
    a' <- toFullyAnnotated a
    e' <- exprToFullyAnnotated e
    pure ((a', ident), e')

moduleToFullyAnnotated :: Module Ann -> Maybe (Module Ann')
moduleToFullyAnnotated mod = do
  decls' <- forM (moduleDecls mod) bindToFullyAnnotated
  imports' <- forM (moduleImports mod) $ \(a, name) ->
    (, name) <$> toFullyAnnotated a
  pure mod { moduleDecls = decls', moduleImports = imports' }

-- | Desugars a module from AST to CoreFn representation.
moduleToCoreFn :: Environment -> A.Module -> Module Ann
moduleToCoreFn _ (A.Module _ _ _ _ Nothing) =
  internalError "Module exports were not elaborated before moduleToCoreFn"
moduleToCoreFn env (A.Module modSS coms mn decls (Just exps)) =
  let imports = mapMaybe importToCoreFn decls ++ fmap (ssAnn modSS,) (findQualModules decls)
      imports' = dedupeImports imports
      exps' = ordNub $ concatMap exportToCoreFn exps
      externs = ordNub $ mapMaybe externToCoreFn decls
      decls' = concatMap declToCoreFn decls
  in Module modSS coms mn (spanName modSS) imports' exps' externs decls'

  where

  -- | Remove duplicate imports
  dedupeImports :: [(Ann, ModuleName)] -> [(Ann, ModuleName)]
  dedupeImports = fmap swap . M.toList . M.fromListWith const . fmap swap

  ssA :: SourceSpan -> Ann
  ssA ss = (ss, [], Nothing, Nothing)

  -- | Desugars member declarations from AST to CoreFn representation.
  declToCoreFn :: A.Declaration -> [Bind Ann]
  declToCoreFn (A.DataDeclaration (ss, com) Newtype _ _ [(ctor, _)]) =
    [NonRec (ssA ss) (properToIdent ctor) $
      Abs (ss, com, Nothing, Just IsNewtype) (Ident "x") (Var (ssAnn ss) $ Qualified Nothing (Ident "x"))]
  declToCoreFn d@(A.DataDeclaration _ Newtype _ _ _) =
    error $ "Found newtype with multiple constructors: " ++ show d
  declToCoreFn (A.DataDeclaration (ss, com) Data tyName _ ctors) =
    flip fmap ctors $ \(ctor, _) ->
      let (_, _, _, fields) = lookupConstructor env (Qualified (Just mn) ctor)
      in NonRec (ssA ss) (properToIdent ctor) $ Constructor (ss, com, Nothing, Nothing) tyName ctor fields
  declToCoreFn (A.DataBindingGroupDeclaration ds) =
    concatMap declToCoreFn ds
  declToCoreFn (A.ValueDecl (ss, com) name _ _ [A.MkUnguarded e]) =
    [NonRec (ssA ss) name (exprToCoreFn ss com Nothing e)]
  declToCoreFn (A.BindingGroupDeclaration ds) =
    [Rec . NEL.toList $ fmap (\(((ss, com), name), _, e) -> ((ssA ss, name), exprToCoreFn ss com Nothing e)) ds]
  declToCoreFn (A.TypeClassDeclaration sa@(ss, _) name _ supers _ members) =
    [NonRec (ssA ss) (properToIdent name) $ mkTypeClassConstructor sa supers members]
  declToCoreFn _ = []

  -- | Desugars expressions from AST to CoreFn representation.
  exprToCoreFn :: SourceSpan -> [Comment] -> Maybe SourceType -> A.Expr -> Expr Ann
  exprToCoreFn _ com ty (A.Literal ss lit) =
    Literal (ss, com, ty, Nothing) (fmap (exprToCoreFn ss com Nothing) lit)
  exprToCoreFn ss com ty (A.Accessor name v) =
    Accessor (ss, com, ty, Nothing) name (exprToCoreFn ss [] Nothing v)
  exprToCoreFn ss com ty (A.ObjectUpdate obj vs) =
    ObjectUpdate (ss, com, ty, Nothing) (exprToCoreFn ss [] Nothing obj) $ fmap (second (exprToCoreFn ss [] Nothing)) vs
  exprToCoreFn ss com ty e@(A.Abs (A.VarBinder _ name) v) = do
    Abs (ss, com, ty, Nothing) name (exprToCoreFn ss [] Nothing v)
  exprToCoreFn ss com ty (A.Abs (A.VarBinder _ name) v) =
    Abs (ss, com, ty, Nothing) name (exprToCoreFn ss [] Nothing v)
  exprToCoreFn _ _ _ (A.Abs _ _) =
    internalError "Abs with Binder argument was not desugared before exprToCoreFn mn"
  exprToCoreFn ss com ty (A.App v1 v2) =
    App (ss, com, ty, Nothing) (exprToCoreFn ss [] Nothing v1) (exprToCoreFn ss [] Nothing v2)
  exprToCoreFn _ com ty (A.Var ss ident) =
    Var (ss, com, ty, getValueMeta ident) ident
  exprToCoreFn ss com ty (A.IfThenElse v1 v2 v3) =
    Case (ss, com, ty, Nothing) [exprToCoreFn ss [] Nothing v1]
      [ CaseAlternative [LiteralBinder (ssAnn ss) $ BooleanLiteral True]
                        (Right $ exprToCoreFn ss [] Nothing v2)
      , CaseAlternative [NullBinder (ssAnn ss)]
                        (Right $ exprToCoreFn ss [] Nothing v3) ]
  exprToCoreFn _ com ty (A.Constructor ss name) =
    Var (ss, com, ty, Just $ getConstructorMeta name) $ fmap properToIdent name
  exprToCoreFn ss com ty (A.Case vs alts) =
    Case (ss, com, ty, Nothing) (fmap (exprToCoreFn ss [] Nothing) vs) (fmap (altToCoreFn ss) alts)
  exprToCoreFn ss com _ (A.TypedValue _ v ty) =
    exprToCoreFn ss com (Just ty) v
  exprToCoreFn ss com ty (A.Let w ds v) =
    Let (ss, com, ty, getLetMeta w) (concatMap declToCoreFn ds) (exprToCoreFn ss [] Nothing v)
  exprToCoreFn ss com ty (A.TypeClassDictionaryConstructorApp name (A.TypedValue _ lit@(A.Literal _ (A.ObjectLiteral _)) _)) =
    exprToCoreFn ss com ty (A.TypeClassDictionaryConstructorApp name lit)
  exprToCoreFn ss com _ (A.TypeClassDictionaryConstructorApp name (A.Literal _ (A.ObjectLiteral vs))) =
    let args = fmap (exprToCoreFn ss [] Nothing . snd) $ sortBy (compare `on` fst) vs
        ctor = Var (ss, [], Nothing, Just IsTypeClassConstructor) (fmap properToIdent name)
    in foldl (App (ss, com, Nothing, Nothing)) ctor args
  exprToCoreFn ss com ty  (A.TypeClassDictionaryAccessor _ ident) =
    Abs (ss, com, ty, Nothing) (Ident "dict")
      (Accessor (ssAnn ss) (mkString $ runIdent ident) (Var (ssAnn ss) $ Qualified Nothing (Ident "dict")))
  exprToCoreFn _ com ty (A.PositionedValue ss com1 v) =
    exprToCoreFn ss (com ++ com1) ty v
  exprToCoreFn _ _ _ e =
    error $ "Unexpected value in exprToCoreFn mn: " ++ show e

  -- | Desugars case alternatives from AST to CoreFn representation.
  altToCoreFn :: SourceSpan -> A.CaseAlternative -> CaseAlternative Ann
  altToCoreFn ss (A.CaseAlternative bs vs) = CaseAlternative (map (binderToCoreFn ss []) bs) (go vs)
    where
    go :: [A.GuardedExpr] -> Either [(Guard Ann, Expr Ann)] (Expr Ann)
    go [A.MkUnguarded e]
      = Right (exprToCoreFn ss [] Nothing e)
    go gs
      = Left [ (exprToCoreFn ss [] Nothing cond, exprToCoreFn ss [] Nothing e)
             | A.GuardedExpr g e <- gs
             , let cond = guardToExpr g
             ]

    guardToExpr [A.ConditionGuard cond] = cond
    guardToExpr _ = internalError "Guard not correctly desugared"

  -- | Desugars case binders from AST to CoreFn representation.
  binderToCoreFn :: SourceSpan -> [Comment] -> A.Binder -> Binder Ann
  binderToCoreFn _ com (A.LiteralBinder ss lit) =
    LiteralBinder (ss, com, Nothing, Nothing) (fmap (binderToCoreFn ss com) lit)
  binderToCoreFn ss com A.NullBinder =
    NullBinder (ss, com, Nothing, Nothing)
  binderToCoreFn _ com (A.VarBinder ss name) =
    VarBinder (ss, com, Nothing, Nothing) name
  binderToCoreFn _ com (A.ConstructorBinder ss dctor@(Qualified mn' _) bs) =
    let (_, tctor, _, _) = lookupConstructor env dctor
    in ConstructorBinder (ss, com, Nothing, Just $ getConstructorMeta dctor) (Qualified mn' tctor) dctor (fmap (binderToCoreFn ss []) bs)
  binderToCoreFn _ com (A.NamedBinder ss name b) =
    NamedBinder (ss, com, Nothing, Nothing) name (binderToCoreFn ss [] b)
  binderToCoreFn _ com (A.PositionedBinder ss com1 b) =
    binderToCoreFn ss (com ++ com1) b
  binderToCoreFn ss com (A.TypedBinder _ b) =
    binderToCoreFn ss com b
  binderToCoreFn _ _ A.OpBinder{} =
    internalError "OpBinder should have been desugared before binderToCoreFn"
  binderToCoreFn _ _ A.BinaryNoParensBinder{} =
    internalError "BinaryNoParensBinder should have been desugared before binderToCoreFn"
  binderToCoreFn _ _ A.ParensInBinder{} =
    internalError "ParensInBinder should have been desugared before binderToCoreFn"

  -- | Gets metadata for let bindings.
  getLetMeta :: A.WhereProvenance -> Maybe Meta
  getLetMeta A.FromWhere = Just IsWhere
  getLetMeta A.FromLet = Nothing

  -- | Gets metadata for values.
  getValueMeta :: Qualified Ident -> Maybe Meta
  getValueMeta name =
    case lookupValue env name of
      Just (_, External, _) -> Just IsForeign
      _ -> Nothing

  -- | Gets metadata for data constructors.
  getConstructorMeta :: Qualified (ProperName 'ConstructorName) -> Meta
  getConstructorMeta ctor =
    case lookupConstructor env ctor of
      (Newtype, _, _, _) -> IsNewtype
      dc@(Data, _, _, fields) ->
        let constructorType = if numConstructors (ctor, dc) == 1 then ProductType else SumType
        in IsConstructor constructorType fields
    where

    numConstructors
      :: (Qualified (ProperName 'ConstructorName), (DataDeclType, ProperName 'TypeName, SourceType, [Ident]))
      -> Int
    numConstructors ty = length $ filter (((==) `on` typeConstructor) ty) $ M.toList $ dataConstructors env

    typeConstructor
      :: (Qualified (ProperName 'ConstructorName), (DataDeclType, ProperName 'TypeName, SourceType, [Ident]))
      -> (ModuleName, ProperName 'TypeName)
    typeConstructor (Qualified (Just mn') _, (_, tyCtor, _, _)) = (mn', tyCtor)
    typeConstructor _ = internalError "Invalid argument to typeConstructor"

-- | Find module names from qualified references to values. This is used to
-- ensure instances are imported from any module that is referenced by the
-- current module, not just from those that are imported explicitly (#667).
findQualModules :: [A.Declaration] -> [ModuleName]
findQualModules decls =
  let (f, _, _, _, _) = everythingOnValues (++) fqDecls fqValues fqBinders (const []) (const [])
  in f `concatMap` decls
  where
  fqDecls :: A.Declaration -> [ModuleName]
  fqDecls (A.TypeInstanceDeclaration _ _ _ _ _ q _ _) = getQual' q
  fqDecls (A.ValueFixityDeclaration _ _ q _) = getQual' q
  fqDecls (A.TypeFixityDeclaration _ _ q _) = getQual' q
  fqDecls _ = []

  fqValues :: A.Expr -> [ModuleName]
  fqValues (A.Var _ q) = getQual' q
  fqValues (A.Constructor _ q) = getQual' q
  -- Some instances are automatically solved and have their class dictionaries
  -- built inline instead of having a named instance defined and imported.
  -- We therefore need to import these constructors if they aren't already.
  fqValues (A.TypeClassDictionaryConstructorApp c _) = getQual' c
  fqValues _ = []

  fqBinders :: A.Binder -> [ModuleName]
  fqBinders (A.ConstructorBinder _ q _) = getQual' q
  fqBinders _ = []

  getQual' :: Qualified a -> [ModuleName]
  getQual' = maybe [] return . getQual

-- | Desugars import declarations from AST to CoreFn representation.
importToCoreFn :: A.Declaration -> Maybe (Ann, ModuleName)
importToCoreFn (A.ImportDeclaration (ss, com) name _ _) = Just ((ss, com, Nothing, Nothing), name)
importToCoreFn _ = Nothing

-- | Desugars foreign declarations from AST to CoreFn representation.
externToCoreFn :: A.Declaration -> Maybe Ident
externToCoreFn (A.ExternDeclaration _ name _) = Just name
externToCoreFn _ = Nothing

-- | Desugars export declarations references from AST to CoreFn representation.
-- CoreFn modules only export values, so all data constructors, class
-- constructor, instances and values are flattened into one list.
exportToCoreFn :: A.DeclarationRef -> [Ident]
exportToCoreFn (A.TypeRef _ _ (Just dctors)) = fmap properToIdent dctors
exportToCoreFn (A.ValueRef _ name) = [name]
exportToCoreFn (A.TypeClassRef _ name) = [properToIdent name]
exportToCoreFn (A.TypeInstanceRef _ name) = [name]
exportToCoreFn _ = []

-- | Makes a typeclass dictionary constructor function. The returned expression
-- is a function that accepts the superclass instances and member
-- implementations and returns a record for the instance dictionary.
mkTypeClassConstructor :: SourceAnn -> [SourceConstraint] -> [A.Declaration] -> Expr Ann
mkTypeClassConstructor (ss, com) [] [] = Literal (ss, com, Nothing, Just IsTypeClassConstructor) (ObjectLiteral [])
mkTypeClassConstructor (ss, com) supers members =
  let args@(a:as) = sort $ fmap typeClassMemberName members ++ superClassDictionaryNames supers
      props = [ (mkString arg, Var (ssAnn ss) $ Qualified Nothing (Ident arg)) | arg <- args ]
      dict = Literal (ssAnn ss) (ObjectLiteral props)
  in Abs (ss, com, Nothing, Just IsTypeClassConstructor)
         (Ident a)
         (foldr (Abs (ssAnn ss) . Ident) dict as)

-- | Converts a ProperName to an Ident.
properToIdent :: ProperName a -> Ident
properToIdent = Ident . runProperName
