{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
module Language.PureScript.Package where

import Prelude.Compat

import Control.Concurrent.MVar
import Control.Concurrent (forkIO)
import Control.Monad (void, forM_, when)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.IO.Class (MonadIO(..))
import System.FilePath ((</>))
import Data.Aeson as Aeson
import Data.Char (isUpper, toLower)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime)
import Data.List (dropWhile)
import Data.Set (Set)
import Data.Map (Map)
import Data.Text (Text)

import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Map as M

import GHC.Generics

import Language.PureScript.Names
import Language.PureScript.Externs
-- import Language.PureScript.

newtype PackageName = PackageName Text
  deriving (Show, Eq, Ord, FromJSON, ToJSON, ToJSONKey, FromJSONKey)

data PackagePlan = PackagePlan {
  ppName :: PackageName,
  ppDependencies :: Set PackageName,
  ppSourceDirectories :: [FilePath],
  ppOutputDirectory :: FilePath
  } deriving (Show, Eq, Generic)

instance FromJSON PackagePlan where parseJSON = genericParseJSON dropLowerCasePrefix
instance ToJSON PackagePlan where toJSON = genericToJSON dropLowerCasePrefix

-- Removes the lower case prefix while parsing or generating a field name.
-- E.g changes 'meExterns' to 'externs'.
dropLowerCasePrefix :: Aeson.Options
dropLowerCasePrefix = defaultOptions {
  fieldLabelModifier = \s -> case dropWhile (not . isUpper) s of
    firstChar:rest -> toLower firstChar : rest
    _ -> s
  }

-- | A recording of a particular module with a particular timestamp.
data ManifestEntry = ManifestEntry {
  meExterns :: ExternsFile,
  meTimestamp :: UTCTime
  } deriving (Show, Generic)

newtype Manifest dep = Manifest { mExterns :: Map ModuleName dep }
  deriving (Show, Eq, Generic)

-- | Contains metadata about a package, perhaps given in a config file
-- needed to specify dependencies (of various kinds) and source files.
data Package' entry = Package {
  pName :: PackageName,
  -- ^ Name of the package
  pDependencies :: Map PackageName (Manifest entry)
  -- ^ PureScript dependency packages. Polymorphic; could be e.g.
  -- prebuilt packages, or MVars for current builds
  } deriving (Show, Eq, Generic)


newtype BuildingPackage = BuildingPackage (Package' (MVar ManifestEntry))
newtype BuiltPackage = BuiltPackage (Package' ManifestEntry)
  deriving (Show, Generic, ToJSON, FromJSON)

instance FromJSON ManifestEntry where parseJSON = genericParseJSON dropLowerCasePrefix
instance ToJSON ManifestEntry where toJSON = genericToJSON dropLowerCasePrefix

instance FromJSON dep => FromJSON (Manifest dep) where parseJSON = genericParseJSON dropLowerCasePrefix
instance ToJSON dep => ToJSON (Manifest dep)  where toJSON = genericToJSON dropLowerCasePrefix

instance FromJSON dep => FromJSON (Package' dep) where parseJSON = genericParseJSON dropLowerCasePrefix
instance ToJSON dep => ToJSON (Package' dep) where toJSON = genericToJSON dropLowerCasePrefix

-- startBuildingManifest
--   :: forall m. (MonadIO m, MonadBaseControl IO m)
--   => BuildPlan -> m (MVar (Manifest ManifestEntry))

readManifest
  :: forall m. (MonadIO m, MonadBaseControl IO m)
  => FilePath -> m (Manifest ManifestEntry)
readManifest root = liftIO $ do
  let path = root </> "manifest.json"
  liftIO $ fromMaybe (Manifest mempty) . decode <$> BL8.readFile path

finishBuildingManifest
  :: forall m. (MonadIO m, MonadBaseControl IO m)
  => Manifest (MVar ManifestEntry) -> m (MVar (Manifest ManifestEntry))
finishBuildingManifest (Manifest entryMVs) = do
  result <- liftIO $ newEmptyMVar
  inProgress <- liftIO $ newMVar mempty
  forM_ (M.toList entryMVs) $ \(modName, entryMV) -> void $ liftIO $ forkIO $ do
    entry <- takeMVar entryMV
    m' <- liftIO $ modifyMVar inProgress $ \m -> do
            let m' = M.insert modName entry m in pure (m', m')
    when (M.size m' == M.size entryMVs) $ do
      liftIO $ putMVar result Manifest { mExterns = m' }
  pure result


-- | Complete the building of a package by resolving all MVars. Blocking.
finishBuildingPackage
  :: forall m. (MonadIO m, MonadBaseControl IO m)
  => BuildingPackage -> m (MVar BuiltPackage)
finishBuildingPackage (BuildingPackage package) = do
  result <- liftIO $ newEmptyMVar
  inProgress <- liftIO $ newMVar mempty
  forM_ (M.toList (pDependencies package)) $ \(pkgName, mvManifest) -> do
    manifest <- liftIO . takeMVar =<< finishBuildingManifest mvManifest
    p <- liftIO $ modifyMVar inProgress $ \m -> do
           let m' = M.insert pkgName manifest m in pure (m', m')
    when (M.size p == M.size (pDependencies package)) $ do
      liftIO $ putMVar result $ BuiltPackage (package { pDependencies = p })
  pure result

-- buildPackagePlan :: forall m. MonadIO m => PackagePlan -> m BuiltPackage
-- buildPackagePlan (PackagePlan {..}) = do
