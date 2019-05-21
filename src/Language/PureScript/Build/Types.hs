{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Language.PureScript.Build.Types (
  module Language.PureScript.Build.Types,
  module Language.PureScript.Package
  ) where

import Prelude.Compat

import Data.Aeson
import Control.Concurrent.MVar.Lifted
import Control.Concurrent.Async.Lifted
import Data.List.NonEmpty (NonEmpty)
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Data.List.NonEmpty as N
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import qualified Data.ByteString.Char8 as B8
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Writer.Class (MonadWriter(..))
import Control.Monad.Reader.Class (MonadReader(..), asks)
import qualified Crypto.Hash.MD5 as MD5
import qualified System.Directory as D
import GHC.Generics

import Language.PureScript.Names
import Language.PureScript.Errors
import Language.PureScript.Externs
import Language.PureScript.Package

-- Stores a `nothing` if the build failed.  (TODO store actual error,
-- but this involves writing quite a few ToJSON/FromJSON
-- instances....)
data CachedBuild = CachedBuild !(Maybe ExternsFile) !(Stamp 'Exts)
  deriving (Show)

-- | Refers to a module which has been discovered by the package manager.
-- Can be considered "proof" that the given module exists.
data ResolvedModuleRef = ResolvedModuleRef {
  rmrModuleId :: !ModuleId, -- ^ ID in the database
  rmrPackageRef :: !PackageRef, -- ^ Package the module is in
  rmrModuleName :: !ModuleName -- ^ The name of the module
  } deriving (Show, Eq, Ord)

instance ToJSON ResolvedModuleRef where
  toJSON (ResolvedModuleRef mid LocalPackage mname) = object [
    "name" .= renderModuleName mname,
    "id" .= mid
    ]
  toJSON (ResolvedModuleRef mid (DepPackage (PackageName p)) mname) = object [
    "package" .= p,
    "name" .= renderModuleName mname,
    "id" .= mid
    ]

prettyRMRef :: ResolvedModuleRef -> String
prettyRMRef (ResolvedModuleRef _ LocalPackage mn) = renderModuleName mn
prettyRMRef (ResolvedModuleRef _ p mn) = prefix <> renderModuleName mn where
  prefix = case p of
    LocalPackage -> ""
    DepPackage (PackageName p') -> T.unpack p' <> "."

type PrecompiledRecord = (ModuleName, ExternsFile, Hash 'Mod)

data PackageMeta = PackageMeta {
  pmRoot :: !FilePath,
  pmModules :: Map ModuleName ModuleId
  } deriving (Show, Eq)

data ModuleMeta = ModuleMeta !ModuleId !FilePath !(Stamp 'Mod)
  deriving (Show, Eq)

data HasId
  = PkgHasId
  | ModHasId

type PackageId = Id 'PkgHasId
type ModuleId = Id 'ModHasId

newtype Id (a :: HasId) = Id Int
  deriving (Show, Eq, Ord, FromField, ToField, ToJSON, FromJSON)

instance ToRow (Id a) where toRow = toRow . Only
instance FromRow (Id a) where fromRow = fromOnly <$> fromRow

-- | Kinds of things that can be timestamped
data HasStamp = Pkg | Mod | Exts

newtype Hash (a :: HasStamp) = Hash { unHash :: B8.ByteString }
  deriving (Show, Eq, FromField, ToField)

type PackageHash = Hash 'Pkg

newtype Stamp (a :: HasStamp) = Stamp {tStamp :: UTCTime}
  deriving (Eq, Ord, FromField, ToField, ToJSON, FromJSON)
instance Show (Stamp a) where show (Stamp s) = show s
instance Semigroup (Stamp a) where (<>) = max

isUpToDateAgainst :: Stamp a -> Stamp b -> Bool
isUpToDateAgainst (Stamp s1) (Stamp s2) = s1 >= s2

type ModuleStamp = Stamp 'Mod
type ExternsStamp = Stamp 'Exts

readStamp :: FilePath -> IO (Stamp a)
readStamp p = Stamp <$> D.getModificationTime p

currentTime :: IO (Stamp a)
currentTime = Stamp <$> getCurrentTime

-- TODO could be a more efficient/correct way to implement this
instance Semigroup (Hash a) where
  Hash h1 <> Hash h2 = Hash $ MD5.finalize $ MD5.update (MD5.update MD5.init h1) h2

hashFile :: FilePath -> IO (B8.ByteString, Hash a)
hashFile p = do
  contents <- B8.readFile p
  pure (contents, initHash contents)

initHash :: B8.ByteString -> Hash a
initHash bs = Hash $ MD5.finalize $ MD5.update MD5.init bs

initHashWithDeps :: B8.ByteString -> [Hash a] -> Hash a
initHashWithDeps source deps = foldr (<>) (initHash source) deps

refreshStamp :: Stamp a -> FilePath -> IO (Stamp a, Maybe B8.ByteString)
refreshStamp stamp path = do
  stamp' <- readStamp path
  case stamp' > stamp of
    False -> pure (stamp, Nothing)
    True -> do
      (contents, hash) <- hashFile path
      pure (stamp', Just contents)

data ModuleRecord = ModuleRecord {
  mrPath :: FilePath,
  mrForeign :: Maybe FilePath,
  mrStamp :: !(Stamp 'Mod),
  mrDeps :: [ResolvedModuleRef]
  } deriving (Generic)

instance ToJSON ModuleRecord

data PackageRecord = PackageRecord (Stamp 'Pkg) (Map ModuleName ModuleRecord)

data ModuleTrace = ModuleTrace {
  mtUnordered :: Set ResolvedModuleRef,
  mtOrdered :: [ResolvedModuleRef]
  }

newTrace :: ModuleTrace
newTrace = ModuleTrace mempty mempty

pushTrace :: ResolvedModuleRef -> ModuleTrace -> ModuleTrace
pushTrace rmr (ModuleTrace u o) = ModuleTrace (S.insert rmr u) (rmr:o)

checkCycle :: ResolvedModuleRef -> ModuleTrace -> Maybe [ResolvedModuleRef]
checkCycle rmr (ModuleTrace u o) = case S.member rmr u of
  True -> Just o
  False -> Nothing
