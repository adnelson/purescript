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

data PackageMeta = PackageMeta {
  pmRoot :: FilePath,
  pmModules :: Map ModuleName (FilePath, ModuleHash, UTCTime)
  } deriving (Show)

-- | Kinds of things that can be hashed
data HashType
  = PackageHash
  | ModuleHash

newtype Hash (a :: HashType) = Hash { unHash :: B8.ByteString }
  deriving (Show, Eq, FromField, ToField)

type PackageHash = Hash 'PackageHash
type ModuleHash = Hash 'ModuleHash

-- TODO could be a more efficient/correct way to implement this
instance Semigroup (Hash a) where
  Hash h1 <> Hash h2 = Hash $ MD5.finalize $ MD5.update (MD5.update MD5.init h1) h2

hashFile :: FilePath -> IO B8.ByteString
hashFile p = B8.readFile p >>= \contents ->
  pure $ MD5.finalize $ MD5.update MD5.init contents

makeHash :: B8.ByteString -> Hash a
makeHash bs = Hash $ MD5.finalize $ MD5.update MD5.init bs

makeModuleHash :: B8.ByteString -> [ModuleHash] -> ModuleHash
makeModuleHash source deps = foldr (<>) (makeHash source) deps

data ModuleRecord = ModuleRecord ModuleHash (Set ResolvedModuleRef)
data PackageRecord = PackageRecord PackageHash (Map ModuleName ModuleRecord)
newtype PackageId = PackageId Int deriving (FromField, ToField)
newtype ModuleId = ModuleId Int deriving (FromField, ToField)

data Signature a = Sig { sHash :: !(Hash a), sStamp :: !UTCTime }

instance Semigroup (Signature a) where Sig h1 s1 <> Sig h2 s2 = Sig (h1 <> h2) (max s1 s2)

{-WIP
newtype Promise m a = Promise { unPromise :: m (Async (StM m a)) }

instance Functor m => Functor (Promise m) where
  fmap f (Promise p) = _what

instance Applicative m => Applicative (Promise m) where
  pure = Promise . async . return
  Promise mf <*> Promise mx = Promise $ do
      f <- mf
      x <- mx
      (f', x') <- waitBoth f x
      async $ return $ f' x'

instance Monad m => Monad (Promise m) where
   Promise p >>= f = Promise $ p >>= wait >>= unPromise . f

runPromise :: MonadBaseControl IO m => Promise m a -> m a
runPromise = wait <=< unPromise
-}
