-- |
-- The core functional representation for literal values.
--
{-# LANGUAGE DeriveGeneric #-}
module Language.PureScript.AST.Literals where
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Prelude.Compat
import Language.PureScript.PSString (PSString)

-- |
-- Data type for literal values. Parameterised so it can be used for Exprs and
-- Binders.
--
data Literal a
  -- |
  -- A numeric literal
  --
  = NumericLiteral (Either Integer Double)
  -- |
  -- A string literal
  --
  | StringLiteral PSString
  -- |
  -- A character literal
  --
  | CharLiteral Char
  -- |
  -- A boolean literal
  --
  | BooleanLiteral Bool
  -- |
  -- An array literal
  --
  | ArrayLiteral [a]
  -- |
  -- An object literal
  --
  | ObjectLiteral [(PSString, a)]
  deriving (Eq, Ord, Show, Functor, Generic)

instance NFData a => NFData (Literal a)
instance ToJSON a => ToJSON (Literal a)
instance FromJSON a => FromJSON (Literal a)
