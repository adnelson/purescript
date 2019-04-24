-- |
-- The core functional representation for binders
--
{-# LANGUAGE DeriveGeneric #-}
module Language.PureScript.CoreFn.Binders where

import Prelude.Compat
import qualified Data.Set as S
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Language.PureScript.AST.Literals
import Language.PureScript.Names

-- |
-- Data type for binders
--
data Binder a
  -- |
  -- Wildcard binder
  --
  = NullBinder a
  -- |
  -- A binder which matches a literal value
  --
  | LiteralBinder a (Literal (Binder a))
  -- |
  -- A binder which binds an identifier
  --
  | VarBinder a Ident
  -- |
  -- A binder which matches a data constructor
  --
  | ConstructorBinder a (Qualified (ProperName 'TypeName)) (Qualified (ProperName 'ConstructorName)) [Binder a]
  -- |
  -- A binder which binds its input to an identifier
  --
  | NamedBinder a Ident (Binder a) deriving (Show, Functor, Generic)

instance NFData a => NFData (Binder a)
instance ToJSON a => ToJSON (Binder a)
instance FromJSON a => FromJSON (Binder a)


extractBinderAnn :: Binder a -> a
extractBinderAnn (NullBinder a) = a
extractBinderAnn (LiteralBinder a _) = a
extractBinderAnn (VarBinder a _) = a
extractBinderAnn (ConstructorBinder a _ _ _) = a
extractBinderAnn (NamedBinder a _ _) = a

binderIdents :: Binder a -> S.Set Ident
binderIdents = \case
  NullBinder _ -> S.empty
  LiteralBinder _ _ -> S.empty
  VarBinder _ ident -> S.singleton ident
  ConstructorBinder _ _ _ binders -> foldr S.union S.empty $ map binderIdents binders
  NamedBinder _ ident binder -> S.insert ident (binderIdents binder)
