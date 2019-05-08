-- | A nix-style approach to building modules.
--
-- The build process goes bottom-up, beginning with some leaf-node
-- module, compiling all of its dependencies, and then building the
-- module. A build results in a unique hash, which is used to skip
-- unnecessary work.
--
module Language.PureScript.Build where

import Prelude.Compat

import Control.Concurrent.MVar.Lifted
import Control.Concurrent.Async.Lifted
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as B8
-- import Data.Aeson (encode)
import Language.PureScript.Build.Search

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Control.Arrow ((***))
import qualified Data.Text.Encoding as T
import Database.SQLite.Simple
import Control.Monad ((>=>))
import Control.Monad.Base
import Control.Monad.Supply
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Writer.Class (MonadWriter(..))
import Control.Monad.Reader.Class (MonadReader(..), asks)


import Language.PureScript.Names
import Language.PureScript.AST
import Language.PureScript.Errors (MultipleErrors, throwSimpleError)
import qualified Language.PureScript.CoreFn as CF
import Language.PureScript.Environment
import Language.PureScript.Externs
import Language.PureScript.Linter
import qualified Language.PureScript.Parser as P
import Language.PureScript.Renamer (renameInModules)
import Language.PureScript.Sugar (desugar, desugarCaseGuards, createBindingGroups, collapseBindingGroups)
import Language.PureScript.TypeChecker


-- This outlines a hash-based approach for package compilation. The building blocks:

-- Jobs are done asynchronously. They return the compiled interface,
-- and the unique has of the compilation.

type Job = Async (ExternsFile, Hash)
type Jobs = Map ResolvedModuleRef Job
type Hash = B8.ByteString

data CachedBuild = CachedBuild {
  cbExterns :: !ExternsFile,
  cbHash :: !Hash
  } deriving (Show)

-- | Distinct from the normal moduleref type, which can refer to either a
-- module within a particular package, or just "some module with this
-- name". This one always refers to one or the other category of
-- module. It will only be instantiated if the module was discovered on disk.
data ResolvedModuleRef
  = LocalModule !ModuleName
  | PackageModule !PackageName !ModuleName
  deriving (Show, Eq, Ord)

data BuildConfig = BuildConfig {
  bcDependentPackages :: [PackageName],
  bcPackageLocations :: [FilePath],
  bcLocalSourceDirectories :: NonEmpty FilePath,
  bcEntryPoints :: NonEmpty ModuleName,
  bcOutput :: FilePath
  } deriving (Show, Eq)

-- instance ToJSON BuildConfig where toJSON = genericToJSON defaultOptions
-- instance FromJSON BuildConfig where parseJSON = genericParseJSON defaultOptions

type PrecompiledRecord = (ModuleName, ExternsFile, Hash)
type ModuleRecord = Map ModuleName (ExternsFile, Hash)
type AsyncLoad m a = Async (StM m a)
type AsyncLoads m k a = MVar (Map k (AsyncLoad m a))

data BuildVars m = BuildVars {
  bvConfig :: BuildConfig,
  bvConn :: MVar Connection,
  bvPackageRecords :: AsyncLoads m PackageName PackageRecord,
  bvPackageLoads :: AsyncLoads m PackageName ModuleRecord,
  bvPathSearches :: AsyncLoads m ModuleRef (ResolvedModuleRef, FilePath),
  bvExternCompiles :: AsyncLoads m ResolvedModuleRef (ExternsFile, Hash)
  }

type Build m = (MonadBaseControl IO m, MonadReader (BuildVars m) m,
                MonadError MultipleErrors m, MonadWriter MultipleErrors m)

loadAsync
  :: forall m k a. (Ord k, Build m)
  => (BuildVars m -> AsyncLoads m k a)
  -> (k -> m a)
  -> k
  -> m (AsyncLoad m a)
loadAsync getMVars fromScratch key = do
  mv <- asks getMVars
  modifyMVar mv $ \(asyncs :: Map k (AsyncLoad m a)) -> case M.lookup key asyncs of
    -- If there's already a job going, wait for it to complete,
    -- then cache the result.
    Just asy -> pure (asyncs, asy)
    Nothing -> do
      asy <- async $ fromScratch key
      pure (M.insert key asy asyncs, asy)


  -- Use a join on the module source table to get the path.
getCachedBuildFromDatabase :: Build m => ResolvedModuleRef -> m (Maybe CachedBuild)
getCachedBuildFromDatabase rmref = error $ "getCachedBuildFromDatabase " <> show rmref

cacheBuildInDatabase :: Build m => ResolvedModuleRef -> CachedBuild -> m ()
cacheBuildInDatabase _ _ = error $ "cacheBuildInDatabase"

getPathToModuleQ :: Query
getPathToModuleQ = undefined

storePathToPackageModuleQ :: Query
storePathToPackageModuleQ = undefined

compileModule :: Build m => [ExternsFile] -> Module -> m ExternsFile
compileModule externs modl = do
  let ffiCodegen = error "ffiCodegen not defined"
  let env = foldl' (flip applyExternsFileToEnvironment) initEnvironment externs
      withPrim = importPrim modl
  lint withPrim
  ((Module ss coms _ elaborated exps, env'), nextVar) <- runSupplyT 0 $ do
    desugar externs [withPrim] >>= \case
      [desugared] -> runCheck' (emptyCheckState env) $ typeCheckModule desugared
      _ -> error "desugar did not return a singleton"

  -- desugar case declarations *after* type- and exhaustiveness checking
  -- since pattern guards introduces cases which the exhaustiveness checker
  -- reports as not-exhaustive.
  (deguarded, nextVar') <- runSupplyT nextVar $ do
    desugarCaseGuards elaborated

  let moduleName = getModuleName modl
  regrouped <- createBindingGroups moduleName . collapseBindingGroups $ deguarded
  let mod' = Module ss coms moduleName regrouped exps
      corefn = CF.moduleToCoreFn env' mod'
      optimized = CF.optimizeCoreFn corefn
      [renamed] = renameInModules [optimized]
      exts = moduleToExternsFile mod' env'
  ffiCodegen renamed
  -- evalSupplyT nextVar' . codegen renamed env' . encode $ exts
  return exts

parseModule :: Build m => FilePath -> B8.ByteString -> m Module
parseModule path source = do
  case P.lex path (T.decodeUtf8 source) >>= P.runTokenParser path P.parseModule of
    Left err -> throwSimpleError $ ErrorParsingModule err
    Right modl -> pure modl

getSourcePath :: Build m => ModuleRef -> m (Async (StM m (ResolvedModuleRef, FilePath)))
getSourcePath mref = do
  mv <- asks bvPathSearches
  modifyMVar mv $ \searches -> case M.lookup mref searches of
    Just search -> pure (searches, search)
    Nothing -> do
      search <- async $ getSourcePathCached mref
      pure (M.insert mref search searches, search)

-- | Look up the path to a module on disk. These are cached in the database.
-- At this step we also determine if a module is local or packaged.
getSourcePathCached :: Build m => ModuleRef -> m (ResolvedModuleRef, FilePath)
getSourcePathCached mref@(ModuleReference maybePkgName mname) = do
  mv <- asks bvConn
  withMVar mv $ \conn -> do
    case maybePkgName of
      Just pname -> liftBase (query conn getPathToModuleQ (pname, mname)) >>= \case
        [] -> searchForModule mref
        Only path:_ -> pure (PackageModule pname mname, path)
      Nothing -> liftBase (query conn getPathToModuleQ (Only mname)) >>= \case
        [] -> searchForModule mref
        [(pname', path)] -> case pname' of
          Nothing -> pure (LocalModule mname, path)
          Just p -> pure (PackageModule p mname, path)
        _ -> throwSimpleError $ AmbiguousModule mref

data ModuleMetadata = ModuleMetadata FilePath Hash (Set ResolvedModuleRef)
data PackageRecord = PackageRecord (Map ModuleName ModuleMetadata)

getPackageRecord :: forall m. Build m => PackageName -> m (AsyncLoad m PackageRecord)
getPackageRecord = loadAsync bvPackageRecords $ \pkgName -> do
  BuildVars { bvConfig = BuildConfig {..}, ..} <- ask
  withMVar bvConn $ \conn -> do
    liftBase (query getPackageRecordQ pkgName) >>= \case
      [] -> discoverPackage pkgName bcPackageLocations >>= \case
        Uncompiled (ModulesAndRoot root modNames) -> _x :: m PackageRecord
        Precompiled dbPath -> _y :: m PackageRecord
      _modNames -> _z :: m PackageRecord

{-

CREATE TABLE module_meta (
  name TEXT NOT NULL,
  -- Null if there's no source code available
  path TEXT,
  hash BLOB NOT NULL
);

CREATE TABLE module_depends (
  module_meta INT NOT NULL,
  depends INT NOT NULL,
  FOREIGN KEY (module_meta) REFERENCES module_meta(rowid) ON DELETE CASCADE,
  FOREIGN KEY (depends) REFERENCES module_meta(rowid) ON DELETE CASCADE
);

CREATE TABLE package (
  -- Name of the package. Empty string means current package.
  name TEXT NOT NULL UNIQUE,
  hash BLOB NOT NULL
);

CREATE TABLE package_module_meta (
  package INT NOT NULL,
  module_meta INT NOT NULL,
  FOREIGN KEY (package) REFERENCES package(rowid) ON DELETE CASCADE,
  FOREIGN KEY (module_meta) REFERENCES module_meta(rowid) ON DELETE CASCADE
);

-}


loadPackage :: Build m => PackageName -> m (Async (StM m ModuleRecord))
loadPackage pname = undefined {- do
  (connMV, locations) <- (,) <$> asks bvPackageLocations <*> asks bvConn
  -- TODO check a cache here?
  discoverPackage pname locations >>= \case
    Uncompiled (ModulesAndRoot root moduleNames) -> do
      -- Store all of the modules
      forM_ moduleNames $ \modName -> do
        let path = root </> moduleNameToRelPath modName
        withMVar connMV $ \conn ->
          liftBase $ execute conn storePathToPackageModuleQ (pname, modName, path)
      -- Compile all of the modules
      fmap M.fromList $ forM moduleNames $ \modName -> do
        job <- buildResolvedModule (PackageModule pname modName)
        (externs, hash) <- wait job
        pure (modName, (externs, hash))
    Precompiled dbpath -> do
      pkgManifestConn <- liftBase $ open dbpath
      withMVar connMV $ \conn -> liftBase $ do
        putStrLn $ "Loading precompiled modules from package " <> show n
        mods <- query_ pkgManifestConn readPrecompiledExternsQuery
        let rows = flip map mods $ \(mn, ef, hash) -> (pn, mn, ef, hash)
        executeMany conn addPackageModuleQuery rows
        putStrLn $ "Imported " <> show (length pkgModules) <> " modules from " <> show n

-}


searchForModule :: Build m => ModuleRef -> m (ResolvedModuleRef, FilePath)
searchForModule (ModuleReference maybePkg modName) = case maybePkg of
  _ -> undefined {-
  Just pname -> do
    -- The package needs to exist
    asks bvPackageLocations >>= discoverPackage pname >>= \case
      Uncompiled (ModulesAndRoot root modNames) -> undefined
      Precompiled manifestPath -> undefined
  Nothing -> do
    (pNames, roots)  <- (,) <$> asks bvPackageNames <*> asks bvLocalModuleRoots
    -- In this case the reference could be from either a package or local.
    -- Refresh package list.
    forM_ pNames $ do
      discovered <- discoverPackage pName

    --
    asks bvLocalModuleRoots >>= discoverLocalModules >>= \case
      _ -> error "not sure"
-}

buildModuleCached :: Build m => ResolvedModuleRef -> FilePath -> m (ExternsFile, Hash)
buildModuleCached rmref path = do
  getCachedBuildFromDatabase rmref >>= \case
    Nothing -> do
      source <- liftBase $ B8.readFile path
      modl <- parseModule path source
      deps <- buildModules $ someModuleNamed <$> getModuleImports modl
      let hash = MD5.finalize $ MD5.updates MD5.init (source : map snd deps)
      externs <- compileModule (map fst deps) modl
      (externs, hash) <$ cacheBuildInDatabase rmref (CachedBuild externs hash)

    Just (CachedBuild externs storedHash) -> do
      -- Build dependencies first.
      deps <- buildModules $ someModuleNamed <$> efImportedModuleNames externs
      -- Hash the contents of the source file + dependency hashes to
      -- get the unique signature of the module we're building.
      source <- liftBase $ B8.readFile path
      let hash = MD5.finalize $ MD5.updates MD5.init (source : map snd deps)
      -- If the hash matches, no more work needs to be done. Otherwise build.
      if storedHash == hash then pure (externs, hash)
        else do
          -- A failure here could also be stored in the database, to
          -- prevent meaningless rebuilds...
          externs' <- parseModule path source >>= compileModule (map fst deps)
          (externs', hash) <$ cacheBuildInDatabase rmref (CachedBuild externs hash)

-- | Build a list of modules in parallel.
buildModules :: Build m => [ModuleRef] -> m [(ExternsFile, Hash)]
buildModules = mapM (buildModule >=> wait)

-- NOTE: Is this the spot to do cycle detection?
buildModule :: forall m. Build m => ModuleRef -> m (Async (StM m (ExternsFile, Hash)))
buildModule mref = do
  (resolvedRef, path) <- getSourcePath mref >>= wait
  buildResolvedModule resolvedRef path

buildResolvedModule
  :: forall m. Build m => ResolvedModuleRef -> FilePath -> m (Async (StM m (ExternsFile, Hash)))
buildResolvedModule resolvedRef path = do
  mv <- asks bvExternCompiles
  modifyMVar mv $ \jobs -> case M.lookup resolvedRef jobs of
    -- If there's already a job going, wait for it to complete,
    -- then cache the result.
    Just job -> pure (jobs, job)
    Nothing -> do
      job <- async $ buildModuleCached resolvedRef path
      pure (M.insert resolvedRef job jobs, job)


-- | Could do this in two steps: the first one finds the packages that
-- need to be built, and the second compiles the modules to
-- javascript. So the first one would be akin to nix-instantiate, while
-- the second to nix-build.
--
-- purescript "externs" are like nix "store paths"
--   * They are the thing you want to produce (along with corresponding codegen)
--   * They are used as inputs into builds
--   * Their existence means that they don't need to be rebuilt
--
-- purescript "modules" are like nix "expressions"
--   * They haven't been built yet, might not be correct
--   * They can be compiled into _ which are like derivations
--
-- purescript _ are like nix "derivations"
--   * They can contain all information needed to execute build steps
--
-- Issue seems to be that purescript doesn't have a clear analogue of derivations.
