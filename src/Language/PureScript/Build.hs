-- | A nix-style approach to building modules.
--
-- The build process goes bottom-up, beginning with some leaf-node
-- module, compiling all of its dependencies, and then building the
-- module. A build results in a unique hash, which is used to skip
-- unnecessary work.
--
module Language.PureScript.Build where

import Prelude.Compat
import Protolude (forM, ordNub)
import System.FilePath ((</>))

import Control.Concurrent.MVar.Lifted
import Control.Concurrent.Async.Lifted
import qualified Data.ByteString.Char8 as B8
import qualified System.Directory as D

import Database.SQLite.Simple (Connection, Only(..))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Data.List (foldl')
import Data.Map (Map)
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Map as M
import qualified Data.Text.Encoding as T
import qualified Database.SQLite.Simple as SQLite
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
import Language.PureScript.Build.Search
import Language.PureScript.Build.Manifest
import Language.PureScript.Build.Types

type AsyncLoad m a = Async (StM m a)
type AsyncLoads m k a = MVar (Map k (AsyncLoad m a))

data BuildConfig = BuildConfig {
  bcDependentPackages :: [PackageName],
  bcPackageLocations :: [FilePath],
  bcLocalSourceDirectory :: FilePath,
  bcEntryPoints :: NonEmpty ModuleName,
  bcOutput :: FilePath
  } deriving (Show, Eq)

-- instance ToJSON BuildConfig where toJSON = genericToJSON defaultOptions
-- instance FromJSON BuildConfig where parseJSON = genericParseJSON defaultOptions

data BuildVars m = BuildVars {
  bvConfig :: BuildConfig,
  bvConn :: MVar SQLite.Connection,
  bvPackageMetas :: AsyncLoads m PackageRef PackageMeta,
  -- ^ Cached modules contained in a package (unvalidated)..

  bvExternCompiles :: AsyncLoads m ResolvedModuleRef (ExternsFile, ModuleStamp),
  -- ^ Caches built modules.

  bvResolvedMods :: AsyncLoads m (PackageRef, ModuleRef) ResolvedModuleRef,
  -- ^ Caches module resolution.

  bvModuleRecords :: AsyncLoads m ResolvedModuleRef ModuleRecord

--  bvPackagePaths :: AsyncLoads m PackageName FilePath,
--  bvModules :: AsyncLoads m ModuleRef (ResolvedModuleRef, FilePath),
--  bvModulePaths :: AsyncLoads m ResolvedModuleRef FilePath,
--  bvModuleRefs :: AsyncLoads m ResolvedModuleRef (Set ResolvedModuleRef, ModuleStamp),
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

-- | Acquire the connnection to the database and use it in an action.
withConn :: Build m => (SQLite.Connection -> m a) -> m a
withConn action = asks bvConn >>= \mv -> withMVar mv action

execute :: (MonadBaseControl IO m, SQLite.ToRow i) => Query i () -> i -> Connection -> m ()
execute (Query qry) args conn = liftBase $ SQLite.execute conn qry args

executeMany :: (MonadBaseControl IO m, SQLite.ToRow i) => Query i () -> [i] -> Connection -> m ()
executeMany (Query q) args conn = liftBase $ SQLite.executeMany conn q args

query
  :: (MonadBaseControl IO m, SQLite.ToRow i, SQLite.FromRow o)
  => Query i o -> i -> Connection -> m [o]
query (Query qry) args conn = liftBase (SQLite.query conn qry args)

-- Use a join on the module source table to get the path.
getCachedBuildFromDatabase :: Build m => ResolvedModuleRef -> m (Maybe CachedBuild)
getCachedBuildFromDatabase rmref = error $ "getCachedBuildFromDatabase " <> show rmref

cacheBuildInDatabase :: Build m => ResolvedModuleRef -> CachedBuild -> m ()
cacheBuildInDatabase _ _ = error $ "cacheBuildInDatabase"

compileModule :: Build m => [ExternsFile] -> Module -> m ExternsFile
compileModule externs modl = do
  let ffiCodegen = error "ffiCodegen not defined"
  let env = foldl' (flip applyExternsFileToEnvironment) initEnvironment externs
      -- This adds `import Prim` and `import qualified Prim` to the
      -- module. Is this really necessary? It seems redundant and
      -- inefficient. Instead, these modules could just be treated
      -- specially when resolving them.
      withPrim = importPrim modl
  lint withPrim
  ((Module ss coms _ elaborated exps, env'), nextVar) <- runSupplyT 0 $ do
    desugar externs [withPrim] >>= \case
      [desugared] -> runCheck' (emptyCheckState env) $ typeCheckModule desugared
      _ -> error "desugar did not return a singleton"

  -- desugar case declarations *after* type- and exhaustiveness checking
  -- since pattern guards introduces cases which the exhaustiveness checker
  -- reports as not-exhaustive.
  (deguarded, _) <- runSupplyT nextVar $ desugarCaseGuards elaborated

  let moduleName = getModuleName modl
  regrouped <- createBindingGroups moduleName . collapseBindingGroups $ deguarded
  let mod' = Module ss coms moduleName regrouped exps
      corefn = CF.moduleToCoreFn env' mod'
      optimized = CF.optimizeCoreFn corefn
      [renamed] = renameInModules [optimized]
      exts = moduleToExternsFile mod' env'
  _ <- ffiCodegen renamed
  -- evalSupplyT nextVar' . codegen renamed env' . encode $ exts
  return exts

parseModule :: Build m => FilePath -> B8.ByteString -> m Module
parseModule path source = do
  case P.lex path (T.decodeUtf8 source) >>= P.runTokenParser path P.parseModule of
    Left err -> throwSimpleError $ ErrorParsingModule err
    Right modl -> pure modl

-- | TODO switch this to only read as much from the file as it needs
parseModuleImports :: Build m => FilePath -> B8.ByteString -> m [ModuleRef]
parseModuleImports path source = getModuleImports <$> parseModule path source

-- | Resolve a module reference. Success means the module has a valid
-- and unambiguous name, and hence a known path.
--
-- A valid module reference is one of the following:
--
-- * A qualified name referring to a dependent package and a module
--   found in that package.
-- * The unqualified name of a module which is in the same package as where
--   the reference originated (modules with the same name might exist
--   in other packages)
-- * The unqualified name of a module which appears in exactly ONE dependent
--   package
--   * This does mean that a package might compile on its own but not
--     when used as a dependency, if other dependencies provide a
--     module with the same name. However in practice I imagine this
--     will be rare, and if precompiled packages become the norm, the
--     metadata can disambiguate.
--
-- With the above rules, we can see that we can't cache modulerefs on
-- their own, because the origin of the reference affects the correct
-- resolution. But we *can* cache a moduleref PLUS the packageref of
-- its origin, so references to the same module coming from the same
-- package don't need to be repeatedly looked up.
--
resolveModuleRef
  :: forall m. Build m
  => PackageRef -> ModuleRef -> m (AsyncLoad m ResolvedModuleRef)
resolveModuleRef = curry $ loadAsync bvResolvedMods $ \(pref, ModuleRef mPkg name) -> case mPkg of
  Just pname -> do
    -- We don't need to use the information here; just make sure that it loaded successfully
    PackageMeta _ mods <- getPackageMeta (DepPackage pname) >>= wait
    case M.lookup name mods of
      Nothing -> throwSimpleError $ ModuleNotFoundInPackage pname name
      Just mId -> pure $ ResolvedModuleRef mId (DepPackage pname) name
  Nothing -> do
    -- First, attempt to load the module from the same package.
    pkgModules <- fmap pmModules $ getPackageMeta pref >>= wait
    case M.lookup name pkgModules of
      Just mid -> pure $ ResolvedModuleRef mid pref name
      Nothing -> do
        -- It wasn't found in the package. Load other packages, then
        -- check if there's an unambiguous module with that name.
        depNames <- asks (bcDependentPackages . bvConfig)
        let packages = filter (/= pref) $ LocalPackage : map DepPackage depNames
        fromOtherPackages <- forConcurrently packages $ \pref' -> do
          PackageMeta _ mods <- getPackageMeta pref' >>= wait
          pure $ do
            modId <- M.lookup name mods
            pure $ ResolvedModuleRef modId pref' name
        case catMaybes fromOtherPackages of
          [result] -> pure result
          [] -> throwSimpleError $ ModuleNotFound name
          refs -> do
            let pkgNames = flip mapMaybe refs $ \(ResolvedModuleRef _ p _) -> case p of
                  LocalPackage -> Nothing
                  DepPackage p' -> Just p'
            throwSimpleError $ AmbiguousModule name (ordNub pkgNames)

-- | Get the path to a module which is known to exist.
getSourcePath :: Build m => ResolvedModuleRef -> m FilePath
getSourcePath rmref = do
  PackageMeta root _ <- getPackageMeta (rmrPackageRef rmref) >>= wait
  pure $ root </> moduleNameToRelPath (rmrModuleName rmref)

-- | Asynchronously load the names of all of the modules in a package.
-- If this successfully completes, it means:
-- * The database has a record of this package
-- * The databases has records of all names of all modules in the package
-- It does NOT mean that the full module tree is valid (that happens
-- in 'getPackageRecord').
getPackageMeta
  :: forall m. Build m
  => PackageRef
  -> m (AsyncLoad m PackageMeta)
getPackageMeta = loadAsync bvPackageMetas $ \pref -> do
  res <- withConn (query getPackageIdQ (Only pref))
  case res of
    -- If we have a package ID the modules of that package have been discovered.
    (pkgId, root):_ -> do
      modules :: Map ModuleName ModuleId <- M.fromList <$> do
        withConn (query getPackageModulesFromIdQ pkgId)
      pure $ PackageMeta root modules

    -- If we don't have that package, we have to load it.
    _ -> do
      -- s <- ask
      let locations :: [FilePath] = undefined -- <- bcPackageLocations . bvConfig <$> ask
      discoverPackage pref locations >>= \case
        Uncompiled (ModulesAndRoot root modNames) -> do
          -- Load each module in the db. Since this is the first time
          -- reading them, we record the stamp with no comparison.
          stamps <- forConcurrently modNames $ \n -> do
            let path = root </> moduleNameToRelPath n
            stamp <- liftBase $ readStamp path
            pure (n, stamp)

          -- Once modules are loaded, we can put them in the database.
          namesAndIds <- withConn $ \conn -> do
            execute insertPackageQ (pref, root) conn
            let rows = map (\(n, s) -> (pref, n, s)) stamps
            executeMany insertModuleQ rows conn
            -- As a last step grab all of the newly inserted rows
            query getPackageModulesQ (Only pref) conn

          -- Construct the package record.
          pure $ PackageMeta root $ M.fromList namesAndIds
        Precompiled _manifestPath -> error "precompiled manifests not yet implemented"


-- | Asynchronously load a module record.
-- Since this will recur on dependencies of a module, it can detect cycles.
getModuleRecord
  :: forall m. Build m => ModuleTrace -> ResolvedModuleRef -> m (AsyncLoad m ModuleRecord)
getModuleRecord trace = loadAsync bvModuleRecords getRecord where
  getRecord :: ResolvedModuleRef -> m ModuleRecord
  getRecord (r@(ResolvedModuleRef mId pref _)) = case checkCycle r trace of
    -- Cycle detection. TODO incorporate packages into trace
    Just pkgs -> do
      throwSimpleError $ CycleInModules (map (someModuleNamed . rmrModuleName) pkgs)
    Nothing -> do
      path <- getSourcePath r
      -- See if it's in the database; if so it will have a hash and timestamp.
      (withConn (query getModuleStampQ mId) :: m [Only ModuleStamp]) >>= \case
        (Only mstamp):_ -> do
          -- There's an entry in the database. But is it up-to-date? First check the timestamp.
          (latestStamp, maybeSource) <- liftBase $ refreshStamp mstamp path
          depRefs :: Either [ModuleRef] [ResolvedModuleRef] <- case maybeSource of
            -- There's a new file; we need to reparse.
            Just source -> Left <$> parseModuleImports path source
            -- If it hasn't changed, then we can reuse the same dependency list from before.
            Nothing -> Right <$> do
              rows <- withConn (query getModuleDependsQ mId)
              pure $ flip map rows $ \(rmrModuleId, rmrPackageRef, rmrModuleName) -> do
                ResolvedModuleRef {..}

          rmrefs <- case depRefs of
            -- Already resolved: return as-is
            Right rmrefs -> pure rmrefs
            -- Load unresolved references
            Left unresolved -> forConcurrently unresolved $ resolveModuleRef pref >=> wait

          -- Recur on these dependencies
          depRecords :: [ModuleRecord] <- do
            asyncs <- forM rmrefs $ getModuleRecord (pushTrace r trace)
            mapM wait asyncs

          -- Compute the hash off of dependencies
          let stamp = foldr (<>) latestStamp (mrStamp <$> depRecords)
          withConn $ \conn -> do
            execute insertModuleDependsListQ (mId, stamp) conn
            let rows = flip map rmrefs $ \dep -> (mId, rmrModuleId dep)
            executeMany insertModuleDependsQ rows conn

          pure $ ModuleRecord path stamp rmrefs

        [] -> do
          -- No dependencies recorded in the database.
          stamp <- liftBase $ readStamp path
          source <- liftBase $ B8.readFile path
          unresolvedImports <- parseModuleImports path source
          resolvedImportAsyncs <- forConcurrently unresolvedImports $ \mref -> do
            -- Note that we're passing in the package ref of *this* module; i.e. the one
            -- currently being loaded. By adding this info we can unambiguously determine which
            -- package/module combo is the correct answer for this package.
            rmref :: ResolvedModuleRef <- do
              resolveModuleRef (rmrPackageRef r) mref >>= wait
            (rmref,) <$> getModuleRecord (pushTrace r trace) rmref

          depInfos :: [(ResolvedModuleRef, ModuleRecord)] <- do
            forM resolvedImportAsyncs $ \(rmref, a) -> do
              recd :: ModuleRecord <- wait a
              pure (rmref, recd)

          let depStamps = map (mrStamp . snd) depInfos
          let stamp' = foldr (<>) stamp depStamps

          withConn $ \conn -> do
            -- Create a module dependency list
            execute insertModuleDependsListQ (mId, stamp') conn
            let depIds = map (rmrModuleId . fst) depInfos
            executeMany insertModuleDependsQ ((mId,) <$> depIds) conn

          pure $ ModuleRecord path stamp (map fst resolvedImportAsyncs)


-- | Given a resolved module reference, compile it into externs. This result is cached.
buildResolvedModule
  :: forall m. Build m
  => ResolvedModuleRef -> m (AsyncLoad m (ExternsFile, ModuleStamp))
buildResolvedModule = loadAsync bvExternCompiles $ \rmref -> do
  -- Get dependencies from the record and recur on them first.
  ModuleRecord path stamp depRefs <- getModuleRecord newTrace rmref >>= wait
  deps :: [(ExternsFile, ModuleStamp)] <- forConcurrently depRefs (buildResolvedModule >=> wait)

  getCachedBuildFromDatabase rmref >>= \case
    -- If the stored stamp is up to date, return immediately
    Just (CachedBuild externs stamp') | stamp' >= stamp -> pure (externs, stamp')

    -- Otherwise, rebuild the module, cache it and return the new stamp.
    _ -> do
      source <- liftBase $ B8.readFile path
      modl <- parseModule path source
      externs <- compileModule (map fst deps) modl
      (externs, stamp) <$ cacheBuildInDatabase rmref (CachedBuild externs stamp)


cfg :: BuildConfig
cfg = BuildConfig {
  bcDependentPackages = [PackageName "purescript-prelude"],
  bcPackageLocations = ["thetest/js/bower_components"],
  bcLocalSourceDirectory = "thetest/input",
  bcOutput = "thetest/output2",
  bcEntryPoints = ModuleName [ProperName "Test"] N.:| []
  }


-- Create the build directory and the initial manifest DB.
initBuild :: MonadBaseControl IO m => BuildConfig -> m (BuildVars m)
initBuild bvConfig@(BuildConfig {..}) = liftBase $ do
  D.createDirectoryIfMissing True bcOutput
  conn <- SQLite.open (bcOutput </> "manifest.db")
  -- Create tables
  let execute_ (Query q) = SQLite.execute_ conn q
  mapM_ execute_ [createPackageTableQ, createModuleMetaTableQ,
                  createModuleDependsTableQ,
                  createPackageModuleMetaTableQ,
                  createPackageRecordViewQ, createModulePathHashViewQ]

  -- Create mvars
  bvConn <- newMVar conn
  bvExternCompiles <- newMVar mempty
  bvResolvedMods <- newMVar mempty
  bvModuleRecords <- newMVar mempty
  bvPackageMetas <- newMVar mempty
  pure BuildVars {..}
