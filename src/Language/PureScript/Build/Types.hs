{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.PureScript.Build.Types where

import Prelude.Compat

import Control.Concurrent.MVar.Lifted
import Control.Concurrent.Async.Lifted
import Data.List.NonEmpty (NonEmpty)
import Data.Time.Clock (UTCTime)
import qualified Data.List.NonEmpty as N
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import qualified Data.ByteString.Char8 as B8
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Writer.Class (MonadWriter(..))
import Control.Monad.Reader.Class (MonadReader(..), asks)
import qualified Crypto.Hash.MD5 as MD5
import qualified System.Directory as D

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
  cbHash :: !(Hash 'Mod)
  } deriving (Show)

-- | Distinct from the normal moduleref type, which can refer to either a
-- module within a particular package, or just "some module with this
-- name". This one always refers to one or the other category of
-- module. It will only be instantiated if the module was discovered on disk.
data ResolvedModuleRef = ResolvedModuleRef {
  rmrModuleId :: !ModuleId,
  rmrPackageRef :: !PackageRef,
  rmrModuleName :: !ModuleName
  } deriving (Show, Eq, Ord)

instance ToRow ResolvedModuleRef where
  toRow (ResolvedModuleRef _ pr mname) = case pr of
    LocalPackage -> toRow ("" :: String, mname)
    DepPackage pname -> toRow (pname, mname)

-- instance ToRow ResolvedModuleRef where
--   toRow = \case
--     LocalModule mname -> toRow ("" :: String, mname)
--     DepModule pname mname -> toRow (pname, mname)

type PrecompiledRecord = (ModuleName, ExternsFile, Hash 'Mod)

data PackageMeta = PackageMeta {
  pmRoot :: FilePath,
  pmModules :: Map ModuleName ModuleId
  } deriving (Show, Eq)

data ModuleMeta = ModuleMeta !ModuleId !FilePath !(TimedHash 'Mod)
  deriving (Show, Eq)

data HasId
  = PkgHasId
  | ModHasId
  | ModDepListHasId

type PackageId = CanonId 'PkgHasId
type ModuleId = CanonId 'ModHasId
type ModuleDepListId = CanonId 'ModDepListHasId

newtype CanonId (a :: HasId) =CanonId {cId :: Int}
  deriving (Show, Eq, Ord, FromField, ToField)

instance ToRow (CanonId a) where toRow = toRow . Only
instance FromRow (CanonId a) where fromRow = fromOnly <$> fromRow

-- | Kinds of things that can be hashed/timestamped/etc
data ObjType = Pkg | Mod

newtype Hash (a :: ObjType) = Hash { unHash :: B8.ByteString }
  deriving (Show, Eq, FromField, ToField)

type PackageHash = Hash 'Pkg
type ModuleHash = Hash 'Mod
type ModuleStamp = Stamp 'Mod

newtype Stamp (a :: ObjType) = Stamp {tStamp :: UTCTime}
  deriving (Show, Eq, Ord, FromField, ToField)

readStamp :: FilePath -> IO (Stamp a)
readStamp p = Stamp <$> D.getModificationTime p

data TimedHash a = TimedHash { thStamp :: !(Stamp a), thHash :: !(Hash a) }
  deriving (Show, Eq)
instance Semigroup (TimedHash a) where
  TimedHash s1 h1 <> TimedHash s2 h2 = TimedHash (max s1 s2) (h1 <> h2)
instance FromRow (TimedHash a) where
  fromRow = fromRow >>= \(hash, stamp) -> pure (TimedHash hash stamp)
instance ToRow (TimedHash a) where toRow (TimedHash h s) = toRow (h, s)

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

refreshTimedHash :: TimedHash a -> FilePath -> IO (TimedHash a, Maybe B8.ByteString)
refreshTimedHash th@(TimedHash stamp _) path = do
  stamp' <- readStamp path
  case stamp' > stamp of
    False -> pure (th, Nothing)
    True -> do
      (contents, hash) <- hashFile path
      pure (TimedHash stamp' hash, Just contents)

data ModuleRecord = ModuleRecord {
  mrHash :: !(TimedHash 'Mod),
  mrDeps :: [ResolvedModuleRef]
  }

data PackageRecord = PackageRecord (TimedHash 'Pkg) (Map ModuleName ModuleRecord)

data ModuleTrace = ModuleTrace {
  mtUnordered :: Set ResolvedModuleRef,
  mtOrdered :: [ResolvedModuleRef]
  }

pushTrace :: ResolvedModuleRef -> ModuleTrace -> ModuleTrace
pushTrace rmr (ModuleTrace u o) = ModuleTrace (S.insert rmr u) (rmr:o)

checkCycle :: ResolvedModuleRef -> ModuleTrace -> Maybe [ResolvedModuleRef]
checkCycle rmr (ModuleTrace u o) = case S.member rmr u of
  True -> Just o
  False -> Nothing
