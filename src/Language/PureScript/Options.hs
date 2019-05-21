-- | The data type of compiler options
{-# LANGUAGE DeriveGeneric #-}
module Language.PureScript.Options where

import Prelude.Compat
import qualified Data.Set as S
import Data.Map (Map)
import GHC.Generics (Generic)
import qualified Data.Map as Map
import qualified Data.Aeson as A -- (FromJSON(..), Options(..), defaultOptions)

-- | The data type of compiler options
data Options = Options
  { optionsVerboseErrors :: Bool
  -- ^ Verbose error message
  , optionsNoComments :: Bool
  -- ^ Remove the comments from the generated js
  , optionsCodegenTargets :: S.Set CodegenTarget
  -- ^ Codegen targets (JS, CoreFn, etc.)
  , optionsSourceMaps :: Bool
  -- ^ Generate sourcemaps
  , optionsAddPrefix :: Bool
  -- ^ Add a "Generated purs" comment to generated code
  } deriving (Show, Generic)

instance A.FromJSON Options where
  parseJSON = A.genericParseJSON A.defaultOptions

-- Default make options
defaultOptions :: Options
defaultOptions = Options False False (S.singleton JS) False True

data CodegenTarget = JS | CoreFn
  deriving (Eq, Ord, Show, Generic)

instance A.FromJSON CodegenTarget

codegenTargets :: Map String CodegenTarget
codegenTargets = Map.fromList
  [ ("js", JS)
  , ("corefn", CoreFn)
  ]
