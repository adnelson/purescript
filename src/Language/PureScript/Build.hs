-- | A nix-style approach to building modules.
--
-- The build process goes bottom-up, beginning with some leaf-node
-- module, compiling all of its dependencies, and then building the
-- module. A build results in a unique hash, which is used to skip
-- unnecessary work.
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.PureScript.Build (
  module Language.PureScript.Build,
  module Language.PureScript.Build.Types,
  module Language.PureScript.Build.Monad,
  module Language.PureScript.Build.Manifest
  ) where

import Prelude.Compat
import Protolude (forM, ordNub, when, whenM, throwError)
import System.FilePath ((</>))
import Debug.Trace (traceM)
import Control.Concurrent.Async.Lifted
import Control.Concurrent.MVar.Lifted
import qualified Data.ByteString.Char8 as B8
import qualified System.Directory as D

import Database.SQLite.Simple (Connection, Only(..))
import Data.List.NonEmpty (NonEmpty)
import Data.List (foldl')
import Data.Map (Map)
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text.Encoding as T
import qualified Database.SQLite.Simple as SQLite
import Control.Monad ((>=>))
import Control.Monad.Base
import Control.Monad.Supply
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Reader.Class (MonadReader(..), asks)

import Language.PureScript.Crash (internalError)
import Language.PureScript.Constants (isPrim)
import Language.PureScript.Names
import Language.PureScript.AST
import Language.PureScript.Errors (throwSimpleError)
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
import Language.PureScript.Build.Monad

-- Use a join on the module source table to get the path.
-- getCachedBuildFromDatabase :: Build m => ResolvedModuleRef -> m (Maybe CachedBuild)
-- getCachedBuildFromDatabase rmref = withConn (query error $ "getCachedBuildFromDatabase " <> show rmref

cacheBuildInDatabase :: Build m => ResolvedModuleRef -> CachedBuild -> m ()
cacheBuildInDatabase _ _ = error $ "cacheBuildInDatabase"

compileModule :: Build m => [ExternsFile] -> Map ModuleName PackageRef -> Module -> m ExternsFile
compileModule externs prefs modl= do
  -- let ffiCodegen m = do
  --         let mn = CF.moduleName m
  --             foreignFile = outputFilename mn "foreign.js"
  --         case mn `M.lookup` foreigns of
  --           Just path
  --             | not $ requiresForeign m ->
  --                 tell $ errorMessage' (CF.moduleSourceSpan m) $ UnnecessaryFFIModule mn path
  --             | otherwise ->
  --                 checkForeignDecls m path
  --           Nothing | requiresForeign m -> throwError . errorMessage' (CF.moduleSourceSpan m) $ MissingFFIModule mn
  --                   | otherwise -> return ()
  --         for_ (mn `M.lookup` foreigns) (readTextFile >=> writeTextFile foreignFile)
  let env = foldl' (flip applyExternsFileToEnvironment) initEnvironment externs
      -- This adds `import Prim` and `import qualified Prim` to the
      -- module. Is this really necessary? It seems redundant and
      -- inefficient. Instead, these modules could just be treated
      -- specially when resolving them.
      withPrim = importPrim modl
      moduleName = getModuleName modl
      pref = case M.lookup (getModuleName modl) prefs of
        Just p -> p
        Nothing -> internalError "incomplete package reference map"
  when (renderModuleName moduleName == "Data.Unit") $ do
    traceM $ "Compiling " <> renderModuleName moduleName <> " with dependencies "
          <> show (map (renderModuleName . efModuleName) externs)
  lint withPrim
  ((Module ss coms _ elaborated exps, env'), nextVar) <- runSupplyT 0 $ do
    desugar externs [withPrim] >>= \case
      [desugared] -> runCheck' (emptyCheckState env) $ typeCheckModule desugared
      _ -> error "desugar did not return a singleton"

  -- desugar case declarations *after* type- and exhaustiveness checking
  -- since pattern guards introduces cases which the exhaustiveness checker
  -- reports as not-exhaustive.
  (deguarded, _) <- runSupplyT nextVar $ desugarCaseGuards elaborated


  regrouped <- createBindingGroups moduleName . collapseBindingGroups $ deguarded
  let mod' = Module ss coms moduleName regrouped exps
      corefn = CF.moduleToCoreFn env' mod'
      optimized = CF.optimizeCoreFn corefn
      [renamed] = renameInModules [optimized]
      exts = moduleToExternsFile prefs mod' env'
--  traceM $ "Successfully compiled " <> renderModuleName moduleName
  -- _ <- ffiCodegen renamed
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
resolveModuleRef = curry $ loadAsync bvResolvedMods $ \(pref, mr@(ModuleRef mPkg name)) -> do
  traceM $ "Resolving module ref " <> prettyModuleRef mr
  case mPkg of
    Just pname -> do
      traceM $ prettyModuleRef mr <> " has a specific package reference"
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
          traceM $ prettyModuleRef mr <> " was not found in " <> prettyPackageRef pref
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
            [] -> throwSimpleError (ModuleNotFound name)
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
      let loadModules (ModulesAndRoot root modNames) = do
            traceM $ "Loading modules from " <> root
            -- Load each module in the db. Since this is the first time
            -- reading them, we record the stamp with no comparison.
            -- stamps <- forConcurrently modNames $ \n -> do
            --   traceM $ "Getting stamp for module " <> renderModuleName n
            --   let path = root </> moduleNameToRelPath n
            --   stamp <- liftBase $ readStamp path
            --   pure (n, stamp)

            -- Once modules are loaded, we can put them in the database.
            namesAndIds <- withConn $ \conn -> do
              traceM $ "Inserting package " <> show (pref, root)
              execute insertPackageQ (pref, root) conn
              let rows = map (pref,) modNames
              traceM $ "Inserting modules " <> show (map renderModuleName modNames)
              executeMany insertModuleQ rows conn
              -- As a last step grab all of the newly inserted rows
              query getPackageModulesQ (Only pref) conn

            -- Construct the package record.
            pure $ PackageMeta root $ M.fromList namesAndIds
      case pref of
        LocalPackage -> do
          traceM $ "Loading local modules"
          src <- bcLocalSourceDirectory . bvConfig <$> ask
          localModNames <- liftBase $ findModulesIn src
          traceM $ "Found these modules: " <> show (map renderModuleName localModNames)
          loadModules (ModulesAndRoot src localModNames)
        DepPackage pname -> do
          traceM $ "Loading modules of " <> show pname
          locations :: [FilePath] <- bcPackageLocations . bvConfig <$> ask
          discoverPackage pname locations >>= \case
            Uncompiled modsAndRoot -> loadModules modsAndRoot
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
      (withConn (query getModuleStampQ mId) :: m [Only (Maybe ModuleStamp)]) >>= \case
        (Only (Just mstamp)):_ -> do
          traceM $ "Found timestamp " <> show mstamp <> " for " <> show r
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

          traceM $ "Dependency refs: " <> show depRefs

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
            execute insertModuleStampQ (stamp, mId) conn
            let rows = flip map rmrefs $ \dep -> (mId, rmrModuleId dep)
            executeMany insertModuleDependsQ rows conn

          pure $ ModuleRecord path stamp rmrefs

        _ -> do
          traceM $ "Recording dependencies of " <> prettyRMRef r
          -- No dependencies recorded in the database.
          stamp <- liftBase $ readStamp path
          source <- liftBase $ B8.readFile path
          imports <- parseModuleImports path source
          let importsWithoutPrims = filter (not . isPrim . mrName) imports
          resolvedImportAsyncs <- forConcurrently importsWithoutPrims $ \mref -> do
            traceM $ "Resolving " <> prettyRMRef r <> " => " <> prettyModuleRef mref
            -- Note that we're passing in the package ref of *this* module; i.e. the one
            -- currently being loaded. By adding this info we can unambiguously determine which
            -- package/module combo is the correct answer for this package.
            rmref :: ResolvedModuleRef <- do
              resolveModuleRef (rmrPackageRef r) mref >>= wait
            (rmref,) <$> getModuleRecord (pushTrace r trace) rmref

--          traceM $ "2"
          depInfos :: [(ResolvedModuleRef, ModuleRecord)] <- do
            forM resolvedImportAsyncs $ \(rmref, a) -> do
              recd :: ModuleRecord <- wait a
              pure (rmref, recd)

          let depStamps = map (mrStamp . snd) depInfos
          let stamp' = foldr (<>) stamp depStamps

--          traceM $ "3"
          withConn $ \conn -> do
            -- Create a module dependency list
            execute insertModuleStampQ (stamp', mId) conn
            let depIds = map (rmrModuleId . fst) depInfos
            executeMany insertModuleDependsQ ((mId,) <$> depIds) conn

          pure $ ModuleRecord path stamp (map fst resolvedImportAsyncs)


-- | Given a resolved module reference, compile it into externs. This result is cached.
buildResolvedModule
  :: forall m. Build m
  => ResolvedModuleRef
  -- Returns the compiled externs, a timestamp, and all imported
  -- modules (for downstream packages to add)
  -> m (AsyncLoad m CompilationArtifact)
buildResolvedModule = loadAsync bvExternCompiles $ \rmref -> do
  -- Get dependencies from the record and recur on them first.
  ModuleRecord path modStamp depRefs <- getModuleRecord newTrace rmref >>= wait
  deps :: [(ResolvedModuleRef, CompilationArtifact)] <- do
    forConcurrently depRefs $ \r -> (r,) <$> (buildResolvedModule r >>= wait)
  let
    toNP (ResolvedModuleRef {..}) = (rmrPackageRef, rmrModuleName)
    myDeps = M.fromList $ map (\(r, ca) -> (toNP r, caExterns ca)) deps
    transitiveDeps = foldr M.union mempty $ map (caDeps . snd) deps
    allDeps :: M.Map (PackageRef, ModuleName) ExternsFile = myDeps <> transitiveDeps

    go
      :: S.Set (PackageRef, ModuleName)
      -> [(PackageRef, ModuleName, ExternsFile)]
      -> PackageRef
      -> ModuleName
      -> ExternsFile
      -> (S.Set (PackageRef, ModuleName), [(PackageRef, ModuleName, ExternsFile)])
    go seen ordered pref mn (ef@ExternsFile {..})
      -- TODO want to use packagename here or some other unique reference
      | S.member (pref, mn) seen = (seen, ordered)
      | otherwise = (S.insert (pref, mn) seen', (pref, mn, ef) : ordered')
        where
          imports :: [(PackageRef, ModuleName, ExternsFile)] = do
            flip map (efImportedModules ef) $ \(p, m) -> do
              case M.lookup (p, m) allDeps of
                Nothing -> internalError "Incomplete module/package mapping"
                Just externs -> (p, m, externs)
          goTuple (pref, mn, ef) (seen, ordered) = go seen ordered pref mn ef
          (seen', ordered') = foldr goTuple (seen, ordered) imports



    depExts :: [ExternsFile] = snd $ do
      let f ((pref, mn), ef) (seen, ordered) = go seen ordered pref mn ef
      fmap (\(_, _, e) -> e) <$> foldr f (mempty, mempty) (M.toList allDeps)

  traceM $ "Ordered deps of " <> prettyRMRef rmref <> ": " <> show (map (renderModuleName . efModuleName) depExts)

  -- Could get the externs and the timestamp in a single query. The
  -- advantage is one fewer query; the tradeoff is that if the
  -- timestamp is out of date, we waste time parsing the externs,
  -- which could be quite large. We are locking the database for the
  -- two queries.
  let mId = rmrModuleId rmref
  existing <- withConn $ \conn -> do
    query getModuleExternsStampQ mId conn >>= \case
      Only (Just extsStamp):_ | extsStamp `isUpToDateAgainst` modStamp -> do
        query getModuleExternsQ mId conn >>= \case
          Only result:_ -> pure $ Just (CachedBuild result extsStamp)
          _ -> internalError "Externs stamp recorded but no externs"
      _ -> pure Nothing

  case existing of
    Just (CachedBuild (Just externs) stamp') -> do
      pure $ CompilationArtifact externs stamp' allDeps
    -- Otherwise, rebuild the module, cache it and return the new stamp.
    _ -> do
      stamp' <- liftBase currentTime
      source <- liftBase $ B8.readFile path
      modl <- parseModule path source
      let prefMap = foldr (\(p, mn) -> M.insert mn p) mempty (M.keys allDeps)
      exts <- Right <$> compileModule depExts prefMap modl
      let exts' = either (const Nothing) Just exts
      withConn (execute insertModuleExternsQ (exts', stamp', mId))
      case exts of
        Left err -> throwError err
        Right externs -> pure $ CompilationArtifact externs stamp' allDeps

resolveEntryPoints :: forall m. Build m => m (NonEmpty (AsyncLoad m ResolvedModuleRef))
resolveEntryPoints = do
  entryPoints <- bcEntryPoints . bvConfig <$> ask
  forM entryPoints $ \modName -> do
    resolveModuleRef LocalPackage (ModuleRef Nothing modName)

resolveEntryPointDeps
  :: forall m. Build m => m (NonEmpty (ResolvedModuleRef, ModuleRecord))
resolveEntryPointDeps = do
  entryPoints <- bcEntryPoints . bvConfig <$> ask
  forM entryPoints $ \modName -> do
    rmref <- resolveModuleRef LocalPackage (ModuleRef Nothing modName) >>= wait
    record :: ModuleRecord <- getModuleRecord newTrace rmref >>= wait
    pure (rmref, record)

buildEntryPoints
  :: forall m. Build m => m (NonEmpty CompilationArtifact)
buildEntryPoints = do
  entryPoints <- bcEntryPoints . bvConfig <$> ask
  forM entryPoints $ \modName -> do
    rmref <- resolveModuleRef LocalPackage (ModuleRef Nothing modName) >>= wait
    buildResolvedModule rmref >>= wait

-- Create the build directory and the initial manifest DB.
initBuild :: MonadBaseControl IO m => BuildConfig -> m (BuildVars (Builder m))
initBuild bvConfig@(BuildConfig {..}) = liftBase $ do
  D.createDirectoryIfMissing True bcOutput
  -- TEMP
  whenM (D.doesFileExist (bcOutput </> manifestFileName)) $
    D.removeFile (bcOutput </> manifestFileName)
  conn <- SQLite.open (bcOutput </> manifestFileName)
  -- Create tables
  let execute_ (Query q) = SQLite.execute_ conn q
  mapM_ execute_ [createPackageTableQ, createModuleMetaTableQ,
                  createModuleDependsTableQ, createModuleDependsViewQ,
                  createPackageRecordViewQ, createModulePathHashViewQ]

  -- Create mvars
  bvConn <- newMVar conn
  bvExternCompiles <- newMVar mempty
  bvResolvedMods <- newMVar mempty
  bvModuleRecords <- newMVar mempty
  bvPackageMetas <- newMVar mempty
  pure BuildVars {..}
