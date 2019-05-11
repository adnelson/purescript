{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.PureScript.Build.Types where

import Prelude.Compat

import Control.Concurrent.MVar.Lifted
import Control.Concurrent.Async.Lifted
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import qualified Data.ByteString.Char8 as B8
import Data.Map (Map)
import Data.Set (Set)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Writer.Class (MonadWriter(..))
import Control.Monad.Reader.Class (MonadReader(..), asks)
import qualified Crypto.Hash.MD5 as MD5

import Language.PureScript.Names
import Language.PureScript.Errors
import Language.PureScript.Externs

data PackageRef
  = LocalPackage
  | DepPackage !PackageName
  deriving (Show, Eq, Ord)

instance ToRow PackageRef where
  toRow = toRow . Only

instance ToField PackageRef where
  toField = \case
    LocalPackage -> toField ("" :: String)
    DepPackage pname -> toField pname

instance FromField PackageRef where
  fromField f = fromField f >>= \case
    "" -> pure LocalPackage
    pname -> pure $ DepPackage $ PackageName pname

data CachedBuild = CachedBuild {
  cbExterns :: !ExternsFile,
  cbHash :: !ModuleHash
  } deriving (Show)

-- | Distinct from the normal moduleref type, which can refer to either a
-- module within a particular package, or just "some module with this
-- name". This one always refers to one or the other category of
-- module. It will only be instantiated if the module was discovered on disk.
data ResolvedModuleRef = ResolvedModuleRef {
  rmrPackageRef :: !PackageRef,
  rmrModuleName :: !ModuleName
  } deriving (Show, Eq, Ord)

instance ToRow ResolvedModuleRef where
  toRow (ResolvedModuleRef pr mname) = case pr of
    LocalPackage -> toRow ("" :: String, mname)
    DepPackage pname -> toRow (pname, mname)

-- instance ToRow ResolvedModuleRef where
--   toRow = \case
--     LocalModule mname -> toRow ("" :: String, mname)
--     DepModule pname mname -> toRow (pname, mname)

type PrecompiledRecord = (ModuleName, ExternsFile, ModuleHash)
type ModuleRecord = Map ModuleName (ExternsFile, ModuleHash)

newtype PackageHash = PackageHash {unPackageHash :: B8.ByteString}
  deriving (Show, Eq, FromField, ToField)
newtype ModuleHash = ModuleHash {unModuleHash :: B8.ByteString}
  deriving (Show, Eq, FromField, ToField)

makeModuleHash :: B8.ByteString -> [ModuleHash] -> ModuleHash
makeModuleHash source deps =
  ModuleHash $ MD5.finalize $ MD5.updates MD5.init (source : map unModuleHash deps)

data ModuleMetadata = ModuleMetadata ModuleHash (Set ResolvedModuleRef)
data PackageRecord = PackageRecord PackageHash (Map ModuleName ModuleMetadata)
newtype PackageId = PackageId Int deriving (FromField, ToField)
newtype ModuleId = ModuleId Int deriving (FromField, ToField)
