-- | Partial evaluation of PureScript Core.
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, NamedFieldPuns #-}
module Language.PureScript.CoreFn.Reduce (reduce, reduceBind, Reduced(..), Scope, encScope) where

import Prelude.Compat
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace (trace)
import Data.Aeson (encode, ToJSON)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.List (intercalate)
import Data.Text (Text)
import Control.Monad.State.Strict

import Language.PureScript.AST.Literals
import Language.PureScript.CoreFn.Expr
import Language.PureScript.Names
import Language.PureScript.PSString (PSString)

--- | TEMP
enc :: ToJSON a => a -> String
enc = BL8.unpack . encode

c :: Functor f => f a -> f ()
c = fmap (const ())

-- Essentially a flag used to distinguish between reduced and
-- unreduced expressions and guarantee they're not reduced twice.
newtype Reduced a = R {runReduced :: a} deriving (Functor, ToJSON)
instance Show a => Show (Reduced a) where
  show (R a) = show a

-- | A variable in scope during reduction.
data ReducerExpr a
  = Free -- ^ A free variable, e.g. a function argument. Can't be reduced further.
  | Bound (Expr (Reduced a))
  deriving (Functor, Generic)

instance ToJSON a => ToJSON (ReducerExpr a)

type Scope a = M.Map (Qualified Ident) (ReducerExpr a)

-- | Inner reduction function. Hides the 'Reduced' type at the end.
reduce :: forall a. (Show a, ToJSON a) => Scope a -> Expr a -> Expr a
reduce scope e = runReduced <$> reduce' ["REDUCE"] scope e

encScope :: forall a. (Show a, ToJSON a) => Scope a -> String
encScope scope = enc $ M.keys scope

data ReductionState a = ReductionState {
  rsScope :: [Scope a],
  rsTrace :: [String]
  }
type Reduction a t = State (ReductionState a) t

runReduction :: [String] -> Scope a -> Reduction a t -> (t, ReductionState a)
runReduction trace scope red = runState red ReductionState {
  rsScope = [scope],
  rsTrace = trace
  }

withScope :: Scope a -> Reduction a t -> Reduction a t
withScope newScope = withState $ \s -> s {
  rsScope = newScope : rsScope s
  }

withTrace :: String -> Reduction a t -> Reduction a t
withTrace t = withState $ \s -> s { rsTrace = t : rsTrace s }

reduce_ :: Expr a -> Reduction a (Expr (Reduced a))
reduce_ e = do
  ReductionState { rsTrace, rsScope } <- get
  pure $ reduce' rsTrace (mconcat rsScope) e

reduceBind
  :: forall a. (Show a, ToJSON a)
  => Bind a
  -> Reduction a (Bind (Reduced a))
  -- ^ Scope created by this bind, composite scope, reduced binding
reduceBind bind = do
  traceM ("BIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIINDINGINGINGINGINGING " ++ show (const () <$> bind))
  case bind of
    NonRec a ident expr -> NonRec (R a) ident <$> do
      -- Evaluate the inner expression with the same scope as outer.
      withTrace ("nonrec binding of " ++ show ident) $ reduce_ expr
    Rec bindings -> withNewScope $ do
      -- TODO this will fail if not defined in dependency order.
      bindings' <- fmap M.fromList $ forM_ bindings $ \((ann, ident), expr) -> do
        reduced <- reduce_ expr
        pure (ident, reduced)
        -- add the new ident to the current scope
        addToScope ident expr





      let go [] reducedBindings = pure $ Rec $ reducedBindings
          go (((ann, ident'), expr):bs) reducedBindings = do
            reduced <- reduceBind b



      -- NOTE: this is self-referential and would infinite loop
      -- if definitions form a cycle.
      let
        step' ((ann, ident), expr) (s, bs, newScope) = do
          let reduced = reduce' (("!rec " ++ show ident) : labels) scope' expr
          let s' = M.insert (Qualified Nothing ident) (Bound reduced) s
          let newScope' = M.insert (Qualified Nothing ident) (Bound reduced) newScope
          let bs' = (((R ann, ident), reduced) : bs)
          (s', bs', newScope')
        (scope', bindings', newScope) = foldr step' (scope, [], mempty) bindings
      (scope', newScope, Rec bindings')

-- | Inner reduction function. Uses the 'Reduced' type
reduce' :: forall a. (Show a, ToJSON a) => [String] -> Scope a -> Expr a -> Expr (Reduced a)
reduce' labels scope e = let lbl = case labels of { [] -> "TOPLEVEL"; _ -> intercalate " -> " (reverse labels)} in trace ("reducing " ++ lbl) $ case e of
  -- Object literals and array literals are the ones we need to recur on
  Literal a (ObjectLiteral fields) ->
    Literal (R a) $ ObjectLiteral $ map (reduce' ("obj literal":labels) scope <$>) fields

  Literal a (ArrayLiteral exprs) ->
    Literal (R a) $ ArrayLiteral $ map (reduce' ("array literal":labels) scope) exprs

  Accessor a field expr -> case reduce' ("accessor":labels) scope expr of
    o@(Literal _ (ObjectLiteral fields)) -> case lookup field fields of
      Nothing -> o
      Just e' -> e'
    expr' -> Accessor (R a) field expr'

  ObjectUpdate a expr fields -> do
    let fields' :: [(PSString, Expr (Reduced a))] = map (reduce' ("updateFields":labels) scope <$>) fields
    case reduce' ("updateExpr":labels) scope expr of
      -- TODO ensure fields are unique?
      Literal a' (ObjectLiteral fields'') -> do
        let allFields :: [(PSString, Expr (Reduced a))] = fields'' <> fields'
        Literal a' (ObjectLiteral allFields)
      expr' -> ObjectUpdate (R a) expr' fields'

  Abs a argName body -> do
    let argName' = Qualified Nothing argName
    Abs (R a) argName $ reduce' ("abs":labels) (M.insert argName' Free scope) body

  App a fn arg -> do
    let arg' = reduce' ("app arg":labels) scope arg
    case reduce' ("app fn":labels) scope fn of
      Abs _ argName body' -> do
        let argName' = Qualified Nothing argName
        reduce' ("function body":labels) (M.insert argName' (Bound arg') scope) (runReduced <$> body')
      fn' -> App (R a) fn' arg'

  -- The scope holds reduced variables a variable, we'll store it here
  v@(Var _ ident) -> do
    let trace' = case ident of { Qualified Nothing (Ident "as") -> trace; _ -> \_ -> id }
    trace' ("Looking up " ++ show ident ++ " in scope " ++ enc (fmap (const () <$>) scope)) $
      case M.lookup ident scope of
        Just (Bound reducedExpr) -> trace' (show ident ++ " was found") reducedExpr
        _ -> case ident of
          Qualified Nothing (Ident _) -> error (lbl ++ ": " ++ show ident ++ " was NOT found in " ++ enc (fmap (const () <$>) scope))
          _ -> (R <$> v)

  -- TODO we should be able to inline imported expressions as well...

  -- TODO not sure how to reduce case expressions

  Let _ binds returns -> do
    if False -- Ident "as" `S.member` bindListIdents binds
      then error $ "FUCK"
      else go binds scope mempty where
        go binds' newBinds newScope = case binds of
          [] -> reduce' (("let inner, new binds " ++ enc (c <$> newBinds)) : labels)
                  newScope returns
          bind : binds'' -> do
            let ReductionState { rsLastAddedScope = newBinds', rsScope = newScope' } =
                  runReduction labels scope (reduceBind bind)
            go binds'' (newBinds' <> newBinds) (newScope' <> newScope)
        --   step :: Bind a -> Scope a -> Scope a
        --   step b s = trace ("BEFORE REDUCING " ++ encScope s) $ do
        --     let (scope', _) = reduceBind s labels b
        --     trace ("AFTER REDUCTION " ++ encScope scope') scope'
        --   newScope = foldr step scope (reverse binds)
        -- reduce' (("let inner " ++ show (M.keys newScope)) : labels) newScope returns

  _ -> R <$> e
