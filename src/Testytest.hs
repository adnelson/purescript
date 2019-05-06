{-# LANGUAGE NamedFieldPuns #-}
module Testytest where

import Prelude.Compat

import Debug.Trace
import           Data.Maybe (fromMaybe)
import           Data.String (fromString)
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Control.Monad
import           Control.Monad.Error (MonadError, throwError)
import           Control.Monad.Except (runExceptT)
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.State.Strict (StateT, modify, execStateT)
import           Control.Concurrent.MVar.Lifted (MVar, newMVar, withMVar)
import           Control.Monad.Trans.Control (MonadBaseControl(..))
import qualified Data.Aeson as A
import           Data.Bool (bool)
import           Data.Char (isUpper)
import qualified Data.ByteString.Lazy.UTF8 as LBU8
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Language.PureScript as P
import qualified Language.PureScript.Ide.Imports as P
import           Language.PureScript.Errors.JSON
import           Language.PureScript.Make
import           System.FilePath ((</>), takeFileName, takeBaseName, takeExtension)
import qualified System.Console.ANSI as ANSI
import qualified System.Directory as D
import           System.Exit (exitSuccess, exitFailure)
import           System.Directory (getCurrentDirectory)
import           System.IO (hPutStrLn, stderr)
import qualified System.IO as IO
import           System.IO.UTF8 (readUTF8FileT)
import           Database.SQLite.Simple (Connection, Query, Only(..))
import           Database.SQLite.Simple (query, query_, execute_, executeMany, open, execute)
import           Database.SQLite.Simple.ToField (ToField(..))

data PSCMakeOptions = PSCMakeOptions
  { pscmPackageLocations  :: [FilePath]
  , pscmDependentPackages :: [Text]
  , pscmSourceDirectories :: [FilePath]
  , pscmOutputDir         :: FilePath
  , pscmOpts              :: P.Options
  , pscmUsePrefix         :: Bool
  , pscmJSONErrors        :: Bool
  }

opts :: PSCMakeOptions
opts = PSCMakeOptions {
  pscmPackageLocations = ["thetest/js/bower_components"],
  pscmDependentPackages = ["purescript-prelude"],
  pscmSourceDirectories = ["thetest/js/bower_components/purescript-prelude/src", "thetest/input"],
  pscmOutputDir = "thetest/output",
  pscmOpts = P.defaultOptions { P.optionsVerboseErrors = True },
  pscmUsePrefix = True,
  pscmJSONErrors = True
  }

-- | Argumnets: verbose, use JSON, warnings, errors
printWarningsAndErrors :: Bool -> Bool -> P.MultipleErrors -> Either P.MultipleErrors a -> IO ()
printWarningsAndErrors verbose False warnings errors = do
  pwd <- getCurrentDirectory
  cc <- bool Nothing (Just P.defaultCodeColor) <$> ANSI.hSupportsANSI stderr
  let ppeOpts = P.defaultPPEOptions { P.ppeCodeColor = cc, P.ppeFull = verbose, P.ppeRelativeDirectory = pwd }
  when (P.nonEmpty warnings) $
    hPutStrLn stderr (P.prettyPrintMultipleWarnings ppeOpts warnings)
  case errors of
    Left errs -> do
      hPutStrLn stderr (P.prettyPrintMultipleErrors ppeOpts errs)
      exitFailure
    Right _ -> return ()
printWarningsAndErrors verbose True warnings errors = do
  hPutStrLn stderr . LBU8.toString . A.encode $
    JSONResult (toJSONErrors verbose P.Warning warnings)
               (either (toJSONErrors verbose P.Error) (const []) errors)
  either (const exitFailure) (const (return ())) errors

readSourceFile :: FilePath -> P.ModuleName -> IO SourceFile
readSourceFile root modName = SourceFile modName <$> do
  let path = root </> (P.moduleNameToRelPath modName <> ".purs")
  putStrLn $ "Reading source file from " <> path
  readUTF8FileT path

compile :: PSCMakeOptions -> IO ()
compile PSCMakeOptions{..} = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering

  scannedDirectories <- forM pscmSourceDirectories $ \root -> do
    files <- readSourceDirectory readSourceFile root
    pure (root, files)
  (makeErrors, makeWarnings) <- runMake pscmOpts $ do
    -- Read dependencies
    let allFiles = mconcat $ map (uncurry sfsToTuples) scannedDirectories
    ms <- P.parseModulesFromFiles id allFiles
    let filePathMap = M.fromList $ map (\(fp, P.Module _ _ mn _ _) -> (mn, Right fp)) ms
    foreigns <- inferForeignModules filePathMap
    let makeActions = buildMakeActions pscmOutputDir filePathMap foreigns pscmUsePrefix
    P.make makeActions (map snd ms)
  printWarningsAndErrors (P.optionsVerboseErrors pscmOpts) pscmJSONErrors makeWarnings makeErrors
  exitSuccess


-- | Metadata about a source file (e.g. some purescript source file)
data SourceFile = SourceFile {
  sfModuleName :: P.ModuleName,
  -- ^ Relative path of the source file.
  sfContents :: Text
  -- ^ The raw contents of the file.
  } deriving (Eq, Ord)

instance Show SourceFile where
  show (SourceFile p _) = "SourceFile(" <> show p <> ")"

sfToPath :: FilePath -> SourceFile -> FilePath
sfToPath root (SourceFile name _) = root </> P.moduleNameToRelPath name

-- | Convert a SourceFile to a tuple, given its root path
sfsToTuples :: FilePath -> [SourceFile] -> [(FilePath, Text)]
sfsToTuples root = map (\p -> (sfToPath root p, sfContents p))

-- Characters which are allowed to appear in module names
validChars :: S.Set Char
validChars = S.fromList $ ['0'..'9'] <> ['a'..'z'] <> ['A'..'Z'] <> "_'"

-- Check if a given (absolute) path is a valid directory to contain submodules
isValidPursSubdir :: MonadIO m => FilePath -> m Bool
isValidPursSubdir path = case takeFileName path of
  (c:cs) | isUpper c && all (flip S.member validChars) cs -> do
    liftIO $ D.doesDirectoryExist path
  _ -> pure False

-- Check if a given (absolute) path is valid to be a purescript module
isValidPursFile :: MonadIO m => FilePath -> m Bool
isValidPursFile path = case (takeBaseName path, takeExtension path) of
  (c:cs, ".purs") | isUpper c && all (flip S.member validChars) cs -> do
    liftIO $ D.doesFileExist path
  _ -> pure False

-- | Collect all of the source files from a given root directory.
readSourceDirectory
  :: forall a. (FilePath -> P.ModuleName -> IO a) -> FilePath -> IO [a]
readSourceDirectory read' root = do
  putStrLn $ "Discovering source files in " <> root
  found <- execStateT (go (P.ModuleName [])) []
  putStrLn $ "Found " <> show (length found) <> " files"
  pure found
  where
  go :: P.ModuleName -> StateT [a] IO ()
  go modName = do
    let dir = root </> P.moduleNameToRelPath modName
    files <- liftIO $ D.listDirectory dir
    pursFiles <- filterM (liftIO . isValidPursFile . (dir </>)) files
    subdirs <- filterM (liftIO . isValidPursSubdir . (dir </>)) files

    -- Collect files in this directory
    forM_ pursFiles $ \filename -> do
      -- Strip off the ".purs" extension
      let baseModName = P.ProperName $ T.dropEnd 5 $ T.pack filename
      let modName' = P.addModuleName baseModName modName

      result <- liftIO $ read' root modName'
      modify (result :)

    -- Recur on subdirectories
    forM_ subdirs $ \subdir -> do
      go (P.addModuleName (P.ProperName $ T.pack subdir) modName)


data MakeOptions = MakeOptions
  { moPackageLocations  :: [FilePath]
  -- ^ Where to find the code (compiled or not) for dependencies. For
  -- each dependency named 'p', there should be some directory 'f' in
  -- this list for which 'f/p' exists. The code at this location could
  -- be compiled (already has a manifest.db), or it could be
  -- PureScript, in which case it will be compiled once.
  , moDependencies      :: [P.PackageName]
  -- ^ The names of package dependencies. Modules in these packages
  -- will be in scope for the project.
  , moSourceDirectories :: [FilePath]
  -- ^ Directories containing source code for the current project. The
  -- relative paths of modules in these directories corresponds to the
  -- names of those modules.
  , moOutputDirectory   :: FilePath
  -- ^ Where code is to be output to. This directory will contain
  -- compiled JS for the current project, as well as that of any
  -- dependencies which were not precompiled.
  , moCompilerOptions   :: P.Options
  -- ^ PureScript compilation settings.
  , moUsePrefix         :: Bool
  -- ^ Whether to add a "Generated by purs version X.Y.Z" comment.
  , moJSONErrors        :: Bool
  -- ^ Whether to report errors in JSON format.
  } deriving (Show)

opts' :: MakeOptions
opts' = MakeOptions {
  moPackageLocations = ["thetest/js/bower_components"],
  moDependencies = [P.PackageName "purescript-prelude"],
  moSourceDirectories = ["thetest/input"],
  moOutputDirectory = "thetest/output2",
  moCompilerOptions = P.defaultOptions { P.optionsVerboseErrors = True },
  moUsePrefix = True,
  moJSONErrors = True
  }

-- Thread safety of SQLite connections seems dicey (see:
-- https://github.com/IreneKnapp/direct-sqlite/issues/61). So, hide
-- the handle to the database behind an MVar to synchronize access.
data ManifestHandle = ManifestHandle FilePath (MVar Connection)

instance Show ManifestHandle where
  show (ManifestHandle fp _) = "ManifestHandle(" <> show fp <> ")"

data BuildConfig = BuildConfig {
  bcOptions :: MakeOptions,
  bcManifestHandle :: ManifestHandle
  } deriving (Show)

createPackageModulesTable :: Query
createPackageModulesTable = fromString $ unlines [
  "CREATE TABLE IF NOT EXISTS package_modules (",
  "  package TEXT NOT NULL,",
  "  module_name TEXT NOT NULL,",
  "  externs BLOB NOT NULL,",
  "  timestamp TEXT NOT NULL,",
  -- Perhaps better than this would be to store the source of the
  -- package, which can be either a path to a manifest or to a source
  -- directory.
  "  is_relative_path INT NOT NULL",
  ");"
  ]

createExternsTable :: Query
createExternsTable = fromString $ unlines [
  "CREATE TABLE IF NOT EXISTS externs (",
  "  module TEXT NOT NULL,",
  "  package TEXT,",
  "  externs BLOB NOT NULL,",
  "  timestamp TEXT NOT NULL,",
  "  UNIQUE(package, module)",
  ");"
  ]

createLocalModulesTable :: Query
createLocalModulesTable = fromString $ unlines [
  "CREATE TABLE IF NOT EXISTS local_paths (",
  "  module_name TEXT NOT NULL PRIMARY KEY,",
  "  source_path TEXT NOT NULL",
  ");"
  ]

createLocalModulesExternsTable :: Query
createLocalModulesExternsTable = fromString $ unlines [
  "CREATE TABLE IF NOT EXISTS local_externs (",
  "  module_name TEXT NOT NULL PRIMARY KEY,",
  "  externs BLOB NOT NULL,",
  "  timestamp TEXT NOT NULL,",
  ");"
  ]

addPackageModuleQuery :: Query
addPackageModuleQuery =
  "INSERT INTO package_modules (package, module_name, externs, timestamp, is_relative_path) VALUES (?, ?, ?, ?, ?)"

addLocalModulePathQuery :: Query
addLocalModulePathQuery =
  "INSERT OR REPLACE INTO local_paths (module_name, source_path) VALUES (?, ?)"

addExternsQuery :: Query
addExternsQuery = do
  "INSERT OR REPLACE INTO externs (module, package, externs, timestamp) VALUES (?, ?, ?, ?)"

addLocalModuleQuery :: Query
addLocalModuleQuery =
  "INSERT OR REPLACE INTO local_modules (module_name, externs, timestamp) VALUES (?, ?, ?)"

readPrecompiledExternsQuery :: Query
readPrecompiledExternsQuery =
  "SELECT module_name, externs, timestamp FROM package_modules WHERE package = ?"

readPackageModulesQuery :: Query
readPackageModulesQuery = "SELECT module_name FROM package_modules"

-- | Could support a version of this which disambiguates with a package name
readPackageExternsQuery :: Query
readPackageExternsQuery =
  "SELECT externs FROM package_modules WHERE module_name = ?"

-- | Check if a module name is from a package module
hasPackageModuleQuery :: Query
hasPackageModuleQuery =
  "SELECT module_name FROM package_modules WHERE module_name = ? LIMIT 1"

-- | Check a timestamp for a single local module
getLocalModuleTimestampQuery :: Query
getLocalModuleTimestampQuery =
  "SELECT timestamp FROM local_externs WHERE module_name = ?"

-- | Get the path on disk for a single local module
getLocalModulePathQuery :: Query
getLocalModulePathQuery =
  "SELECT source_path FROM local_paths WHERE module_name = ?"

type SomeMake m = (MonadIO m, MonadBaseControl IO m, MonadReader BuildConfig m, MonadError P.MultipleErrors m, MonadWriter P.MultipleErrors m)

addPackageSourcesToManifest
  :: forall m. SomeMake m
  => P.PackageName -- ^ Name of the package being loaded
  -> FilePath -- ^ Location where the package lives
  -> m () -- ^ Compiles the modules and adds them to the manifest DB.
addPackageSourcesToManifest pname@(P.PackageName (T.unpack -> name)) dir = do
  liftIO $ putStrLn $ "Loading package " <> show name <> " from " <> dir
  -- Find the first valid purescript file in the folder, use this to determine the source root.
  let
    root' = dir </> name
    go [] = throwError $ P.errorMessage $ P.CannotFindSourceRoot root'
    go (root:otherPaths) = liftIO (readSourceDirectory readSourceFile root) >>= \case
      [] -> go otherPaths
      sfs -> do
        liftIO $ putStrLn $ "Found source files for package " <> show name <> " in " <> root
        pure (root, sfs :: [SourceFile])

  (root, sourceFiles) <- go [root', root' </> "src"]

  BuildConfig (MakeOptions {..}) (ManifestHandle _ mv) <- ask
  ms <- P.parseModulesFromFiles id $ sfsToTuples root sourceFiles
  let filePathMap = M.fromList $ map (\(fp, P.Module _ _ mn _ _) -> (mn, Right fp)) ms
  foreigns <- inferForeignModules filePathMap
  makeActions <- buildMakeActions' name filePathMap foreigns
  externsFiles <- runMake' $ P.make makeActions (map snd ms)

  -- Add all of the new externs to the database.
  withMVar mv $ \conn -> do
    liftIO $ executeMany conn addPackageModuleQuery
      $ flip map externsFiles $ \ef ->
        -- Not precompiled, will require a relative path.
        (pname, P.efModuleName ef, ef, True)

runMake'
  :: forall m a. SomeMake m
  => P.Make a -> m a
runMake' action = do
  options <- moCompilerOptions . bcOptions <$> ask
  (result, warnings) <- liftIO (runMake options action)
  tell warnings
  case result of
    Left errors -> throwError errors
    Right result' -> pure result'


loadDependency
  :: forall m. SomeMake m
  => P.PackageName -- ^ Package being compiled
  -> m ()
loadDependency pn@(P.PackageName (T.unpack -> n)) = do
  -- If it's already in the database, return
  -- Search for the package name in package locations
  BuildConfig (MakeOptions {..}) (ManifestHandle _ mv) <- ask
  let
    -- Add modules from a precompiled manifest to the current manifest
    mergeManifest :: FilePath -> m ()
    mergeManifest dbpath = do
      pkgManifestConn <- liftIO $ open dbpath
      withMVar mv $ \conn -> liftIO $ do
        putStrLn $ "Loading precompiled modules from package " <> show n
        pkgModules <- query_ pkgManifestConn readPrecompiledExternsQuery
        executeMany conn addPackageModuleQuery
          $ flip map pkgModules $ \(mn :: P.ModuleName, ef :: P.ExternsFile) -> (pn, mn, ef)
        putStrLn $ "Imported " <> show (length pkgModules) <> " modules from " <> show n

    go :: [FilePath] -> m ()
    go [] = error $ "Couldn't find package " <> show n
    go (dir:dirs) = liftIO (D.doesDirectoryExist (dir </> n)) >>= \case
        -- If no folder with the package name exists, keep searching.
        False -> go dirs
        True -> liftIO (D.doesFileExist (dir </> n </> "manifest.db")) >>= \case
          -- If there's a manifest, merge it into the project manifest.
          True -> mergeManifest (dir </> n </> "manifest.db")
          -- Otherwise, discover the modules in it and compile.
          False -> addPackageSourcesToManifest pn dir
  go moPackageLocations

-- Create the build directory and the initial manifest DB.
initBuildConfig :: MakeOptions -> IO BuildConfig
initBuildConfig options@(MakeOptions {..}) = do
  D.createDirectoryIfMissing True moOutputDirectory
  let dbPath = (moOutputDirectory </> "manifest.db")
  manifestConn <- open dbPath
  -- Create tables
  execute_ manifestConn createPackageModulesTable
  execute_ manifestConn createLocalModulesTable
  handle <- ManifestHandle dbPath <$> newMVar manifestConn
  pure $ BuildConfig options handle

storeExternsFile'
  :: forall m. SomeMake m
  => P.ModuleName
  -> P.ExternsFile
  -> UTCTime
  -> Maybe P.PackageName
  -> m ()
storeExternsFile' modName externs stamp pkgName = do
  BuildConfig (MakeOptions {..}) (ManifestHandle _ mv) <- ask
  liftIO $ withMVar mv $ \conn -> do
    execute conn addExternsQuery (modName, pkgName, externs, stamp)

buildLocalModules
  :: forall m. SomeMake m
  => ModuleDepGraph -- (FilePath, [SourceFile])]
  -> m ()
buildLocalModules (ModuleDepGraph mtcs) = do
  BuildConfig (MakeOptions {..}) (ManifestHandle _ mv) <- ask
  ms <- P.parseModulesFromFiles id (map (\m -> (mtcPath m, mtcContents m)) $ M.elems mtcs)
  let filePathMap = M.fromList $ map (\(fp, P.Module _ _ mn _ _) -> (mn, Right fp)) ms
  foreigns <- inferForeignModules filePathMap
  makeActions <- buildMakeActions' "" filePathMap foreigns
  externsFiles <- runMake' $ P.make makeActions (map snd ms)

  -- Add all of the new externs to the database.
  withMVar mv $ \conn -> liftIO $ do
    executeMany conn addLocalModuleQuery
      $ flip map externsFiles $ \ef -> do
        let mName = P.efModuleName ef
        case M.lookup mName mtcs of
          Nothing -> P.internalError $ "Didn't produce module " <> show mName
          Just (LocalModuleToCompile {..}) -> (mName, ef, mtcTimestamp, mtcPath)

buildMakeActions'
  :: forall m. MonadReader BuildConfig m
  => [Char]
  -> M.Map P.ModuleName (Either RebuildPolicy FilePath)
  -> M.Map P.ModuleName FilePath
  -> m (MakeActions Make)
buildMakeActions' subdir filePathMap foreigns = do
  BuildConfig (MakeOptions {..}) (ManifestHandle _ mv) <- ask
  let outDir = if subdir /= "" then moOutputDirectory else moOutputDirectory </> subdir
  -- Create a customized MakeActions object which will look up externs from the database.
  let
    readExternsFile mn = withMVar mv $ \conn -> do
      liftIO $ putStrLn $ "Looking up externsfile for module " <> P.renderModuleName mn
      liftIO (query conn readPackageExternsQuery (Only mn)) >>= \case
        [Only externs] -> pure $ Just externs
        [] -> pure Nothing
        _ -> P.internalError $ "Ambiguous module " <> P.renderModuleName mn

    getCachedExterns = error "getCachedExterns not implemented"
    storeExternsFile = error "storeExternsFile not implemented"

    getInputTimestamp mn = liftIO $ withMVar mv $ \conn -> do
      -- If it's a local module, get the path from the local modules
      -- table and check the file timestamp
      query conn getLocalModulePathQuery (Only mn) >>= \case
        Only path:_ -> Right <$> D.getModificationTime path
        _ -> do
          -- Check if it's a dependency, return a neverrebuild policy
          query conn hasPackageModuleQuery (Only mn) >>= \case
            Only (_ :: P.ModuleName):_ -> pure $ Left P.RebuildNever
            _ -> P.internalError $ "Undefined module " <> P.renderModuleName mn

    getOutputTimestamp mn = liftIO $ withMVar mv $ \conn -> do
      -- If it's a local module, read the timestamp from the local modules table
      query conn getLocalModuleTimestampQuery (Only mn) >>= \case
        Only stamp:_ -> pure $ Just stamp
        _ -> do
          query conn hasPackageModuleQuery (Only mn) >>= \case
            Only (_ :: P.ModuleName):_ -> Just <$> getCurrentTime
            _ -> pure Nothing


  pure (buildMakeActions outDir filePathMap foreigns moUsePrefix) {
    readExternsFile,
    getInputTimestamp,
    getOutputTimestamp,
    getCachedExterns,
    storeExternsFile
    }


data LocalModuleToCompile = LocalModuleToCompile {
  mtcName :: P.ModuleName,
  mtcPath :: FilePath,
  mtcTimestamp :: UTCTime,
  mtcDependencies :: [P.ModuleName],
  mtcContents :: Text
  } deriving (Show)

newtype ModuleDepGraph = ModuleDepGraph (M.Map P.ModuleName LocalModuleToCompile)

instance Show ModuleDepGraph where
  show (ModuleDepGraph g) = "{" <> T.unpack inner <> "}" where
    inner = T.intercalate ", " $ map inner' (M.toList g)
    inner' (mName, toCompile) = P.runModuleName mName <> ": " <> T.pack (show toCompile)

readLocalModuleToCompile :: FilePath -> P.ModuleName -> IO LocalModuleToCompile
readLocalModuleToCompile root mtcName = do
  let mtcPath = root </> P.moduleNameToRelPath mtcName <> ".purs"
  mtcTimestamp <- D.getModificationTime mtcPath
  runExceptT (P.parseImportsFromFile mtcPath) >>= \case
    Left err -> error $ show err -- TODO
    Right (_, imports) -> do
      mtcContents <- readUTF8FileT mtcPath
      pure LocalModuleToCompile {
        mtcDependencies = flip map imports $ \(mn, _, _) -> mn,
        ..
        }

-- | Generate a dependency graph for the local modules.
buildModuleGraph
  :: forall m. (MonadIO m, MonadBaseControl IO m, MonadReader BuildConfig m)
  => m ModuleDepGraph
buildModuleGraph = do
  dirs <- moSourceDirectories . bcOptions <$> ask
  let read' root modName = readLocalModuleToCompile root modName
  ModuleDepGraph . M.fromList . map (\m -> (mtcName m, m)) . mconcat <$> do
    mapM (liftIO . readSourceDirectory read') dirs

-- | Given a dependency graph, we can find the names of all modules
-- not contained within the local project. TODO map these back to the
-- source annotation for error reporting.
getUnboundModules :: ModuleDepGraph -> S.Set P.ModuleName
getUnboundModules (ModuleDepGraph graph) = S.fromList $ mconcat $ map go (M.elems graph)
  where go (LocalModuleToCompile {..}) = filter (\n -> not (M.member n graph)) mtcDependencies

-- | Finds all of the module(s) that should be provided by
-- packages. Rebuilds packages if some modules are not found.
-- Fails if multiple packages have the same module.
-- If this function succeeds, all requested modules should have a
-- single entry in the package_modules table in the manifest database.
resolveModulesFromPackages
  :: forall m. SomeMake m
  => S.Set P.ModuleName
  -> m ()
resolveModulesFromPackages moduleNames = do
  BuildConfig (MakeOptions {..}) (ManifestHandle _ mv) <- ask
  -- get current module names
  pkgModules <- S.fromList . map fromOnly <$> do
    liftIO $ withMVar mv $ flip query_ readPackageModulesQuery
  when (S.size (S.difference moduleNames pkgModules) > 0) $ do
    liftIO $ putStrLn "Missing modules found. Refreshing packages..."
    mapM_ loadDependency moDependencies
  refreshedModules <- S.fromList . map fromOnly <$> do
    liftIO $ withMVar mv $ flip query_ readPackageModulesQuery
  let missing = S.difference moduleNames refreshedModules
  when (S.size missing > 0) $ do
    let toError modName = P.ErrorMessage [] $ P.ModuleNotFound modName
    throwError $ P.MultipleErrors $ map toError $ S.toList missing

run :: MakeOptions -> IO ()
run options@(MakeOptions{..}) = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering
  cfg <- initBuildConfig options
  (result, warnings) <- runMake moCompilerOptions (runBuild cfg)
  printWarningsAndErrors (P.optionsVerboseErrors moCompilerOptions) moJSONErrors warnings result

runBuild
  :: forall m. (MonadBaseControl IO m, MonadIO m, MonadError P.MultipleErrors m, MonadWriter P.MultipleErrors m)
  => BuildConfig -> m ()
runBuild buildConfig = do
  flip runReaderT buildConfig $ do
    depGraph <- buildModuleGraph
    resolveModulesFromPackages $ getUnboundModules depGraph
    buildLocalModules depGraph

  -- * For any dependency which doesn't exist in the manifest, compile it and add it.
  -- * Generate a module dependency tree from the input files (including modules from dependencies)
  -- * Filter this tree to the modules which require a rebuild, which means either:
  --   * The module is not listed in the manifest,
  --   * The timestamp of the source file is newer than the one in the manifest,
  --   * Or the above is true for any of the module's dependencies.
  -- * Compile local modules in dependency order (if there are any to be compiled).
  --    * We should be able to invoke 'Make.rebuildModule', we just
  --      need to make sure it's pointing at the right directory.
    liftIO $ putStrLn "ok"
