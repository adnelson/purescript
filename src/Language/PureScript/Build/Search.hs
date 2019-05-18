module Language.PureScript.Build.Search where

import Prelude.Compat

import Control.Monad.Base (liftBase)
import Control.Monad (forM_, filterM)
import Control.Monad.Except (MonadError)
import Control.Monad.State.Strict (StateT, modify, execStateT)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Data.Char (isUpper)
import qualified Data.Set as S
import qualified Data.Text as T
import Language.PureScript.Names
import Language.PureScript.Errors
import Language.PureScript.Build.Types

import           System.FilePath ((</>), takeFileName, takeDirectory, takeBaseName, takeExtension)
import qualified System.Directory as D





manifestFileName :: FilePath
manifestFileName = "manifest.db"

-- Characters which are allowed to appear in module names
validChars :: S.Set Char
validChars = S.fromList $ ['0'..'9'] <> ['a'..'z'] <> ['A'..'Z'] <> "_'"

-- Check if a given (absolute) path is a valid directory to contain submodules
isValidPursSubdir :: MonadBaseControl IO m => FilePath -> m Bool
isValidPursSubdir path = case takeFileName path of
  (c:cs) | isUpper c && all (flip S.member validChars) cs -> do
    liftBase $ D.doesDirectoryExist path
  _ -> pure False

-- Check if a given (absolute) path is valid to be a purescript module
isValidPursFile :: MonadBaseControl IO m => FilePath -> m Bool
isValidPursFile path = case (takeBaseName path, takeExtension path) of
  (c:cs, ".purs") | isUpper c && all (flip S.member validChars) cs -> do
    liftBase $ D.doesFileExist path
  _ -> pure False

-- | Collect all of the source files from a given root directory.
findModulesIn :: FilePath -> IO [ModuleName]
findModulesIn root = do
  putStrLn $ "Discovering source files in " <> root
  found <- execStateT (go (ModuleName [])) []
  putStrLn $ "Found " <> show (length found) <> " files"
  pure found
  where
  go :: ModuleName -> StateT [ModuleName] IO ()
  go modName = do
    let dir = takeDirectory $ root </> moduleNameToRelPath modName
    files <- liftBase $ D.listDirectory dir
    pursFiles <- filterM (liftBase . isValidPursFile . (dir </>)) files
    subdirs <- filterM (liftBase . isValidPursSubdir . (dir </>)) files

    -- Collect files in this directory
    forM_ pursFiles $ \filename -> do
      -- Strip off the ".purs" extension
      let baseModName = ProperName $ T.dropEnd 5 $ T.pack filename
      let modName' = addModuleName baseModName modName
      modify (modName':)

    -- Recur on subdirectories
    forM_ subdirs $ \subdir -> do
      go (addModuleName (ProperName $ T.pack subdir) modName)

data ModulesAndRoot = ModulesAndRoot {
  -- Root directory of the discovered modules
  mrRoot :: FilePath,
  -- Names of all the modules discovered in this directory
  mrModules :: [ModuleName]
  } deriving (Show)

data DiscoveredPackage
  = Uncompiled ModulesAndRoot
  -- ^ Uncompiled package. Return all discovered modules. At some
  -- point this could instead contain a list of modules/roots.
  | Precompiled FilePath
  -- ^ If the package is precompiled, return the path to its manifest.
  deriving (Show)

discoverPackage
  :: forall m. (MonadBaseControl IO m, MonadError MultipleErrors m)
  => PackageRef -- ^ Package being compiled
  -> [FilePath] -- ^ Search locations
  -> m DiscoveredPackage
discoverPackage LocalPackage _ = undefined
discoverPackage (DepPackage pn@(PackageName (T.unpack -> n))) locations = go locations
  where
    findPackageModules :: [FilePath] -> m (Maybe ModulesAndRoot)
    findPackageModules [] = pure Nothing
    findPackageModules (root:otherPaths) = liftBase (findModulesIn root) >>= \case
      [] -> findPackageModules otherPaths
      modules -> pure (Just $ ModulesAndRoot root modules)

    go :: [FilePath] -> m DiscoveredPackage
    go [] = throwSimpleError $ PackageNotFound pn
    go (dir:dirs) = liftBase (D.doesDirectoryExist (dir </> n)) >>= \case
      -- If no folder with the package name exists, keep searching.
      False -> go dirs
      True -> liftBase (D.doesFileExist (dir </> n </> manifestFileName)) >>= \case
        -- If there's a manifest, merge it into the project manifest.
        True -> pure $ Precompiled (dir </> n </> manifestFileName)
        -- Otherwise, try to find package modules in it.
        False -> findPackageModules [dir, dir </> "root"] >>= \case
          Just modules -> pure $ Uncompiled modules
          Nothing -> go dirs
