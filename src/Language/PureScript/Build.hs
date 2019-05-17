-- | A nix-style approach to building modules.
--
-- The build process goes bottom-up, beginning with some leaf-node
-- module, compiling all of its dependencies, and then building the
-- module. A build results in a unique hash, which is used to skip
-- unnecessary work.
--
module Language.PureScript.Build where

import Prelude.Compat
import Protolude (forM, forM_, ordNub)
import System.FilePath ((</>))

import Control.Concurrent.MVar.Lifted
import Control.Concurrent.Async.Lifted
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as B8
import Data.Time.Clock (UTCTime)
import Control.Monad.State.Strict
import qualified System.Directory as D
-- import Data.Aeson (encode)

import Database.SQLite.Simple (Connection, Only(..))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Map as M
import Control.Arrow ((***))
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
  bvPackageModules :: AsyncLoads m PackageRef PackageMeta,
  -- ^ Cached modules contained in a package (unvalidated)..

  bvPackageRecords :: AsyncLoads m PackageRef PackageRecord,
  -- ^ Caches the full dependency tree of modules within a package.

  bvExternCompiles :: AsyncLoads m ResolvedModuleRef (ExternsFile, ModuleHash),
  -- ^ Caches built modules.

  bvResolvedMods :: AsyncLoads m (PackageRef, ModuleRef) ResolvedModuleRef,
  -- ^ Caches module resolution.

  bvModuleRecords :: AsyncLoads m ResolvedModuleRef ModuleRecord

--  bvPackagePaths :: AsyncLoads m PackageName FilePath,
--  bvModules :: AsyncLoads m ModuleRef (ResolvedModuleRef, FilePath),
--  bvModulePaths :: AsyncLoads m ResolvedModuleRef FilePath,
--  bvModuleRefs :: AsyncLoads m ResolvedModuleRef (Set ResolvedModuleRef, ModuleHash),
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

-- | TODO switch this to only read as much from the file as it needs
parseModuleImports :: Build m => FilePath -> B8.ByteString -> m [ModuleRef]
parseModuleImports path source = getModuleImports <$> parseModule path source

-- rmrefToId :: Build m => ResolvedModuleRef -> m ModuleId
-- rmrefToId =

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
-- resolution. But we *can* cache a moduleref PLUS packageref, so
-- references to the same module coming from the same package don't need to
-- be repeatedly looked up.
--
resolveModuleRef
  :: forall m. Build m
  => (PackageRef, ModuleRef) -> m (AsyncLoad m ResolvedModuleRef)
resolveModuleRef = loadAsync bvResolvedMods $ \(pref, ModuleRef mPkg name) -> case mPkg of
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
                  DepPackage p -> Just p
            throwSimpleError $ AmbiguousModule name (ordNub pkgNames)

-- | Get the path to a module which is known to exist.
getSourcePath :: Build m => ResolvedModuleRef -> m FilePath
getSourcePath rmref = do
  PackageMeta root _ <- getPackageMeta (rmrPackageRef rmref) >>= wait
  pure $ root </> moduleNameToRelPath (rmrModuleName rmref)


  --   rmref -> do
  -- withConn (query getModuleMetaFromResolvedQ rmref) >>= \case
  --   [] -> searchForModule mref >>= wait
  --   (Only path):_ -> pure path

-- | Look up the path to a module on disk. These are cached in the database.
-- At this step we also determine if a module is local or packaged.
-- getSourcePathCached :: Build m => ModuleRef -> m (ResolvedModuleRef, FilePath)
-- getSourcePathCached mref@(ModuleReference maybePkgName mname) = do
--   case maybePkgName of
--     Just pname -> do
--       let rmref = ResolvedModuleRef (DepPackage pname) mname
--       withConn (query getModuleMetaFromResolvedQ rmref) >>= \case
--         [] -> searchForModule mref >>= wait
--         (Only path):_ -> pure (rmref, path)
--     Nothing -> do
--       withConn (query getModuleMetaFromNameQ (Only mname)) >>= \case
--         [] -> searchForModule mref >>= wait
--         [(pref, path)] -> pure (ResolvedModuleRef pref mname, path)
--         _ -> throwSimpleError $ AmbiguousModule mref

-- getPackageRecord :: forall m. Build m => PackageName -> m (AsyncLoad m PackageRecord)
-- getPackageRecord = loadAsync bvPackageRecords $ \pkgName -> do
--   BuildVars { bvConfig = BuildConfig {..}, ..} <- ask
--   withConn (query getPackageRecordQ (Only pkgName)) >>= \case
--     [] -> discoverPackage pkgName bcPackageLocations >>= \case
--       Uncompiled (ModulesAndRoot root modNames) -> undefined :: m PackageRecord
--       Precompiled dbPath -> undefined :: m PackageRecord
--     _modNames -> undefined :: m PackageRecord

{-



to get a package record, we need to flatten:

{
  name: string,
  hash: string,
  modules: map string {
    path: string,
    hash: string,
    refs: set {
      package: option string,
      module: string
    }
  }
}

So the columns would be:

package_name | package_hash | module_name | path | hash | ref_package | ref_module

Could normalize these, but not sure of performance tradeoff.

-}

-- loadPackage :: Build m => PackageRef -> m (AsyncLoad m PackageRecord)
-- loadPackage pref = do
--   (connMV, locations) <- (,) <$> asks bvPackageLocations <*> asks bvConn
--   discoverPackage pname locations >>= \case
--     Uncompiled (ModulesAndRoot root moduleNames) -> do
--       -- Store all of the modules
--       forM_ moduleNames $ \modName -> do
--         let path = root </> moduleNameToRelPath modName
--         withMVar connMV $ \conn ->
--           liftBase $ execute conn storePathToDepModuleQ (pname, modName, path)
--       -- Compile all of the modules
--       fmap M.fromList $ forM moduleNames $ \modName -> do
--         job <- buildResolvedModule (DepModule pname modName)
--         (externs, hash) <- wait job
--         pure (modName, (externs, hash))
--     Precompiled dbpath -> do
--       pkgManifestConn <- liftBase $ open dbpath
--       withMVar connMV $ \conn -> liftBase $ do
--         putStrLn $ "Loading precompiled modules from package " <> show n
--         mods <- query_ pkgManifestConn readPrecompiledExternsQuery
--         let rows = flip map mods $ \(mn, ef, hash) -> (pn, mn, ef, hash)
--         executeMany conn addDepModuleQuery rows
--         putStrLn $ "Imported " <> show (length pkgModules) <> " modules from " <> show n

--
-- * Start with some module ref and a config (captured via monadic
-- * context). Note we don't yet know if the module ref (or indeed
-- * much of anything else) is valid.
--
-- conceptual type: ModuleRef + Config
--
-- * Translate into resolved module references. This is the set of discovered
-- * modules. We know that the directories are structured correctly, all listed package requirements and
-- * that each package has at least one discovered module.
-- * The returned timestamp can be used to avoid having to load a file from disk.
--
-- conceptual type: AsyncLoad ResolvedModuleRef (FilePath, Timestamp)
-- implemented with:
--
-- * Given a starting point amongs the discovered modules, build the
-- * module dependency tree. When this step completes, we'll know that
-- * modules are acyclic, that all modules referenced were
-- * unambiguously defined, and that module files are parsable at
-- * least as far as imports go.  In addition to returning a list of
-- * modules, this returns a unique hash which can be compared to for
-- * rebuilds.
--
-- conceptual type: Map ResolvedModuleRef (Set ResolvedModuleRef, ModuleHash)
--
-- * Beginning from the same starting module as the previous step, compile
-- * all required modules to javascript. When this step completes, we'll
-- * know that everything type checked and code-generated correctly.
--
-- conceptual type: Map ResolvedModuleRef (Externs, FilePath)
--

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
getPackageMeta = loadAsync bvPackageModules $ \pref -> do
  res <- withConn (query getPackageIdQ (Only pref))
  undefined :: m PackageMeta

-- loadPackageMeta :: forall m. Bui
  -- case res of
  --   -- If we have a package ID the modules of that package have been discovered.
  --   (pkgId, root):_ -> do
  --     modules :: Map ModuleName ModuleMeta <- M.fromList <$> do
  --       res <- withConn (query getPackageModulesQ pkgId)
  --       pure $ map (\(m, p, h, s) -> (m, (ModuleMeta p (TimedHash h s)))) res
  --     pure $ PackageMeta root modules

  --   -- If we don't have that package, we have to load it.
  --   _ -> do
  --     s <- ask
  --     let locations :: [FilePath] = undefined -- <- bcPackageLocations . bvConfig <$> ask
  --     liftBase (discoverPackage pref locations) >>= \case
  --       Uncompiled (ModulesAndRoot root modNames) -> do
  --         -- Load each module in the db. Since this is the first time
  --         -- reading them, we record the stamp with no comparison.
  --         timedHashes <- forConcurrently modNames $ \n -> do
  --           let path = root </> moduleNameToRelPath n
  --           stamp <- liftBase $ readStamp path
  --           contents <- liftBase $ hashFile path
  --           pure $ (n, TimedHash stamp contents)

  --         -- Once modules are loaded, we can put them in the database.
  --         withConn $ \conn -> do
  --           execute insertPackageQ (pref, root) conn
  --           executeMany insertModuleQ $ map (\(n, TimedHash t h) -> (pref, n, t, h)) timedHashes

  --         -- Construct the package record.
  --         let modules = M.fromList $ flip map timedHashes $ \(mn, th) -> _what
  --         pure $ PackageMeta root modules
  --       Precompiled manifestPath -> error "precompiled manifests not yet implemented"

    {-

Could do something like:

Id | Package | Module  | Path
1  | ''      | Foo.Bar | src/Foo/Bar.purs
2  | ''      | Foo.Baz | src/Foo/Baz.purs
3  | ''      | Foo     | src/Foo.purs

-- Imagine that Foo.Baz depends on Foo.Bar and Foo depends on Foo.Bar and Foo.Baz.

-- Module depends table (presence in row means list has been loaded)
Id | Module
1  | 2       -- Dependencies have been loaded for Foo.Baz
2  | 1       -- Dependencies loaded for Foo.Bar
3  | 3       -- Dependencies loaded for Foo


-- | Module dependency table (individual dependencies)
Id | ModuleDepends | Dependency
1  | 1  (Foo.Baz)  | 1 (Foo.Bar)
1  | 3  (Foo)      | 1 (Foo.Bar)
1  | 3  (Foo)      | 2 (Foo.Baz)

so, storing whether modules have been loaded or not in the database. Since it's behind an mvar this should be safe, but it feels dirty. Maybe:

-- Module meta table
Module  | Path
Foo.Bar | src/Foo/Bar.purs

-- Module "dependencies loaded" table
Module | Depend

-}

-- | Asynchronously load a module record.
-- Since this will recur on dependencies of a module, it can detect cycles.
getModuleRecord
  :: forall m. Build m => ModuleTrace -> ResolvedModuleRef -> m (AsyncLoad m ModuleRecord)
getModuleRecord trace = loadAsync bvModuleRecords getRecord where
  getRecord :: ResolvedModuleRef -> m ModuleRecord
  getRecord (r@(ResolvedModuleRef mId pref mname)) = case checkCycle r trace of
    -- Cycle detection. TODO incorporate packages into trace
    Just trace -> do
      throwSimpleError $ CycleInModules (map (someModuleNamed . rmrModuleName) trace)
    Nothing -> do
      path <- getSourcePath r
      -- See if it's in the database; if so it will have a hash and timestamp.
      (withConn (query getModuleDependsListQ mId) :: m [(ModuleStamp, ModuleHash)]) >>= \case
        (mstamp, mhash):_ -> do
          -- There's an entry in the database. But is it up-to-date? First check the timestamp.
          let curHash = TimedHash mstamp mhash
          (latestHash@(TimedHash latestStamp _), maybeSource) <- do
            liftBase $ refreshTimedHash curHash path
          depRefs :: Either [ModuleRef] [ResolvedModuleRef] <- case maybeSource of
            -- There's a new file; we need to reparse.
            Just source -> Left <$> parseModuleImports path source
            -- If it hasn't changed, then we can reuse the same dependency list from before.
            Nothing -> Right <$> do
              rows <- withConn (query getModuleDependsQ mId)
              pure $ flip map rows $ \(rmrModuleId, rmrPackageRef, rmrModuleName) -> do
                ResolvedModuleRef {..}

          rmrefs <- case depRefs of
            Left unresolved -> undefined
            Right rmrefs -> pure rmrefs

          -- Recur on these dependencies
          depRecords :: [ModuleRecord] <- do
            asyncs <- forM rmrefs $ getModuleRecord (pushTrace r trace)
            mapM wait asyncs

          -- Compute the hash off of dependencies
          let hash'@(TimedHash stamp hash) = foldr (<>) latestHash (mrHash <$> depRecords)
          withConn $ \conn -> do
            execute insertModuleDependsListQ (mId, stamp, hash) conn
            let rows = flip map rmrefs $ \r -> (mId, rmrModuleId r)
            executeMany insertModuleDependsQ rows conn

          pure $ ModuleRecord hash' rmrefs

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
              resolveModuleRef (rmrPackageRef r, mref) >>= wait
            (rmref,) <$> getModuleRecord (pushTrace r trace) rmref

          depInfos :: [(ResolvedModuleRef, ModuleRecord)] <- do
            forM resolvedImportAsyncs $ \(rmref, a) -> do
              recd :: ModuleRecord <- wait a
              pure (rmref, recd)

          let depHashes = map (mrHash . snd) depInfos
          let thash@(TimedHash stamp' hash') =
                foldr (<>) (TimedHash stamp (initHash source)) depHashes

          withConn $ \conn -> do
            -- Create a module dependency list
            execute insertModuleDependsListQ (mId, stamp', hash') conn
            let depIds = map (rmrModuleId . fst) depInfos
            executeMany insertModuleDependsQ ((mId,) <$> depIds) conn

          pure $ ModuleRecord thash (map fst resolvedImportAsyncs)

-- | Asynchronously load a package record.
-- This basically adds an additional layer of validation on top of
-- 'getPackageMeta'. Specifically, successful completion means that the
-- full dependency tree of the package (including modules required
-- from other packages) is known, and that there are no cyclic
-- dependencies. It does NOT mean that the modules will compile
-- successfully.
getPackageRecord
  :: forall m. Build m => PackageRef -> m (AsyncLoad m PackageRecord)
getPackageRecord = loadAsync bvPackageRecords $ \pref -> do
  PackageMeta root modules <- getPackageMeta pref >>= wait
  resolvedMods <- forConcurrently (M.keys modules) $ \modName -> do
    let path = root </> moduleNameToRelPath modName
    source <- liftBase $ B8.readFile path
    imports <- parseModuleImports path source >>= mapM (\mref -> resolveModuleRef (pref, mref))
    undefined :: m Int
  undefined :: m PackageRecord

-- | Build a list of modules in parallel.
buildModules :: Build m => PackageRef -> [ModuleRef] -> m [(ExternsFile, ModuleHash)]
buildModules pref = mapM (buildModule pref >=> wait)

-- NOTE: Is this the spot to do cycle detection?
buildModule
  :: forall m. Build m
  => PackageRef -> ModuleRef -> m (AsyncLoad m (ExternsFile, ModuleHash))
buildModule pref mref = do
  rmref <- resolveModuleRef (pref, mref) >>= wait
  buildResolvedModule rmref

-- | Given a resolved module reference, compile it into externs. This result is cached.
buildResolvedModule
  :: forall m. Build m
  => ResolvedModuleRef -> m (AsyncLoad m (ExternsFile, ModuleHash))
buildResolvedModule = loadAsync bvExternCompiles $ \rmref -> do
  let pref = rmrPackageRef rmref
  path <- getSourcePath rmref
  getCachedBuildFromDatabase rmref >>= \case
    Nothing -> do
      -- No cached build; needs to be built from scratch.
      source <- liftBase $ B8.readFile path
      modl <- parseModule path source
      deps <- buildModules pref $ getModuleImports modl
      let hash = initHashWithDeps source (map snd deps)
      externs <- compileModule (map fst deps) modl
      (externs, hash) <$ cacheBuildInDatabase rmref (CachedBuild externs hash)

    Just (CachedBuild externs storedHash) -> do
      -- Build dependencies first.
      deps <- buildModules pref $ efImportedModuleRefs externs
      -- Hash the contents of the source file + dependency hashes to
      -- get the unique signature of the module we're building.
      source <- liftBase $ B8.readFile path
      let hash = initHashWithDeps source (map snd deps)
      -- If the hash matches, no more work needs to be done. Otherwise build.
      if storedHash == hash then pure (externs, hash)
        else do
          -- A failure here could also be stored in the database, to
          -- prevent meaningless rebuilds...
          externs' <- parseModule path source >>= compileModule (map fst deps)
          (externs', hash) <$ cacheBuildInDatabase rmref (CachedBuild externs hash)


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
  bvPackageModules <- newMVar mempty
  bvPackageRecords <- newMVar mempty
  bvExternCompiles <- newMVar mempty
  bvResolvedMods <- newMVar mempty
  pure BuildVars {..}
