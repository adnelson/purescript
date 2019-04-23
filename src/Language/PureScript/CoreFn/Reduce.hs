-- | Partial evaluation of PureScript Core.
module Language.PureScript.CoreFn.Reduce (reduce) where

import Prelude.Compat
import qualified Data.Set as S
import qualified Data.Map as M

import Control.Arrow ((***))

import Language.PureScript.AST.Literals
import Language.PureScript.CoreFn.Binders
import Language.PureScript.CoreFn.Expr
import Language.PureScript.Names
import Language.PureScript.PSString (PSString)

substituteVariable :: forall a. Ident -> Expr a -> Expr a -> Expr a
substituteVariable ident with e = do
  let recur = substituteVariable ident with
  case e of
    -- This is the main point of the function
    Var _ (Qualified Nothing ident') | ident == ident' -> with
    -- Don't replace qualified any variables
    Var _ _ -> e

    -- Everything else is a recursive call
    Literal a (ArrayLiteral es) -> Literal a (ArrayLiteral $ map recur es)
    Accessor a field e' -> Accessor a field (recur e')
    ObjectUpdate a expr updates -> ObjectUpdate a (recur expr) $ map (fmap recur) updates
    -- Make sure we don't replace a value shadowed in a lambda
    Abs _ arg _ | arg == ident -> e
    -- If it's *not* shadowed in the lambda, it must be replaced in its body
    Abs a arg body -> Abs a arg (recur body)

    App a left right -> App a (recur left) (recur right)
    Case a exprs alternatives -> do
      let
        alternatives' = flip map alternatives $
          \(CaseAlternative binders result) ->
            let
              idents = foldr S.union S.empty $ map binderIdents binders
              result' = case S.member ident idents of
                  -- The variable is shadowed by the case alternative, so don't recur
                  True -> result
                  -- Otherwise we need to recur and replace the variable
                  False -> case result of
                    Right expr -> Right (recur expr)
                    Left guardsAndExprs -> Left (map (recur *** recur) guardsAndExprs)
            in CaseAlternative binders result'
      Case a (map recur exprs) alternatives'
    Let a binds inner -> do
      let
        bindsAndShadowed = flip map binds $ \b -> case b of
          NonRec a' ident' expr
            | ident == ident' -> (b, True) -- shadowed
            | otherwise -> (NonRec a' ident' (recur expr), False)
          Rec innerBindings -> do
            let bindsAndShadowed' = flip map innerBindings $ \b' ->
                  case b' of
                    ((_, ident'), _) | ident == ident' -> (b', True) --shadowed
                    ((a', ident'), e') -> (((a', ident'), recur e'), False)
                rec' = Rec (map fst bindsAndShadowed')
                isShadowed = any snd bindsAndShadowed'
            (rec', isShadowed)
      Let a (map fst bindsAndShadowed) $ case any snd bindsAndShadowed of
        True -> inner
        False -> recur inner
    _ -> e

-- Essentially a flag used to distinguish between reduced and
-- unreduced expressions and guarantee they're not reduced twice.
newtype Reduced a = R {runReduced :: a}
instance Show a => Show (Reduced a) where
  show (R a) = show a

-- | Inner reduction function. Hides the 'Reduced' type at the end.
reduce :: forall a. M.Map Ident (Expr (Reduced a)) -> Expr a -> Expr a
reduce scope e = runReduced <$> reduce' scope e

-- | Inner reduction function. Uses the 'Reduced' type
reduce' :: forall a. M.Map Ident (Expr (Reduced a)) -> Expr a -> Expr (Reduced a)
reduce' scope e = case e of
  -- Object literals and array literals are the ones we need to recur on
  Literal a (ObjectLiteral fields) ->
    Literal (R a) $ ObjectLiteral $ map (reduce' scope <$>) fields

  Literal a (ArrayLiteral exprs) ->
    Literal (R a) $ ArrayLiteral $ map (reduce' scope) exprs

  Accessor a field expr -> case reduce' scope expr of
    o@(Literal _ (ObjectLiteral fields)) -> case lookup field fields of
      Nothing -> o
      Just e' -> e'
    expr' -> Accessor (R a) field expr'

  ObjectUpdate a expr fields -> do
    let fields' :: [(PSString, Expr (Reduced a))] = map (reduce' scope <$>) fields
    case reduce' scope expr of
      -- TODO ensure fields are unique?
      Literal a' (ObjectLiteral fields'') -> do
        let allFields :: [(PSString, Expr (Reduced a))] = fields'' <> fields'
        Literal a' (ObjectLiteral allFields)
      expr' -> ObjectUpdate (R a) expr' fields'

  Abs a argName body -> do
    Abs (R a) argName $ reduce' (M.delete argName scope) body

  App a fn arg -> let arg' = reduce' scope arg in case reduce' scope fn of
    Abs _ argName body' -> substituteVariable argName arg' body'
    fn' -> App (R a) fn' arg'

  -- The scope holds reduced variables a variable, we'll store it here
  v@(Var _ (Qualified Nothing ident)) -> case M.lookup ident scope of
    Just reducedExpr -> reducedExpr
    _ -> R <$> v

  -- TODO we should be able to inline imported expressions as well...

  -- TODO not sure how to reduce case expressions

  Let _ binds returns -> do
    let
      newScope :: M.Map Ident (Expr (Reduced a))
      newScope = foldr step scope binds where
        step bind m = case bind of
          NonRec _ ident expr -> do
            M.insert ident (reduce' scope expr) m
          -- TODO order independence on this is tricky....
          Rec bindings -> do
            -- NOTE: this is self-referential and would infinite loop
            -- if definitions form a cycle.
            let step' ((_, ident), expr) = M.insert ident (reduce' scope' expr)
                scope' = foldr step' scope bindings
            scope'

    reduce' newScope returns

  _ -> R <$> e
