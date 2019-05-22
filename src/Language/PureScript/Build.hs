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
import System.FilePath ((</>), normalise, splitPath, makeRelative)
import Debug.Trace (traceM)
import Data.Either (isLeft)
import Data.Version (showVersion)
import Control.Concurrent.Async.Lifted
import Control.Concurrent.MVar.Lifted
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified System.Directory as D
import Control.Monad.Writer
import Data.Aeson (encode)

import Database.SQLite.Simple (Connection, Only(..))
import Data.List.NonEmpty (NonEmpty)
import Data.List (foldl')
import Data.Map (Map)
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Database.SQLite.Simple as SQLite
import Control.Monad ((>=>))
import Control.Monad.Base
import Control.Monad.Supply
import Control.Monad.Except (MonadError)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Reader (MonadReader(..), asks, runReaderT)
import Control.Monad.Writer.Class (MonadWriter(..), tell)
import qualified Paths_purescript as Paths
import qualified Language.PureScript.CoreImp.AST as Imp

import Language.PureScript.Crash (internalError)
import Language.PureScript.Constants (isPrim)
import Language.PureScript.Errors (MultipleErrors)
import Language.PureScript.Names
import Language.PureScript.AST
import Language.PureScript.CodeGen.JS.Printer
import Language.PureScript.Errors (throwSimpleError, errorMessage')
import Language.PureScript.Options (Options(..), CodegenTarget(..))
import qualified Language.PureScript.CoreFn as CF
import qualified Language.PureScript.CoreFn.ToJSON as CF
import Language.PureScript.Environment
import Language.PureScript.Pretty -- .Common (SMap(..))
import Language.PureScript.Externs
import Language.PureScript.Linter
import qualified Language.PureScript.Make.Actions as MA
import qualified Language.PureScript.Parser as P
import Language.PureScript.Renamer (renameInModules, renameInModule)
import Language.PureScript.Sugar (desugar, desugarCaseGuards, createBindingGroups, collapseBindingGroups)
import Language.PureScript.TypeChecker
import Language.PureScript.Build.Search
import Language.PureScript.Build.Manifest
import Language.PureScript.Build.Types
import Language.PureScript.Build.Monad
import SourceMap (generate)
import SourceMap.Types (Pos(..), Mapping(..), SourceMapping(..))
import qualified Language.PureScript.CodeGen.JS as J


codegen
  :: forall m. (MonadBaseControl IO m, MonadError MultipleErrors m)
  => Options
  -> FilePath
  -> CF.Module CF.Ann
  -> Maybe FilePath
  -> m ()
codegen options@(Options {..}) dir modl maybeForeigns = do
  traceM $ "Generating code for " <> modNameStr <> " in " <> dir
  liftBase $ D.createDirectoryIfMissing True dir
  mapM_ gen optionsCodegenTargets
  where
  modNameStr = renderModuleName $ CF.moduleName modl

  gen :: CodegenTarget -> m ()
  gen = \case
    CoreFn -> do
      let json = CF.moduleToJSON Paths.version modl
      liftBase $ BL8.writeFile (dir </> "corefn.json") (encode json)

    JS -> do
      let jsPath = dir </> "index.js"
      rawJs <- flip runReaderT options $ do
        evalSupplyT 0 $ J.moduleToJs modl $ flip fmap maybeForeigns $ \_ -> do
          let require = Imp.Var Nothing "require"
              requireModule = Imp.StringLiteral Nothing "./foreign.js"
          Imp.App Nothing require [requireModule]
      let (pjs, mMaps) = if optionsSourceMaps
                         then Just <$> prettyPrintJSWithSourceMaps rawJs
                         else (prettyPrintJS rawJs, Nothing)
          prefix = if not optionsAddPrefix then []
                   else ["Generated by purs version " <> T.pack (showVersion Paths.version)]
          js = T.unlines $ map ("// " <>) prefix ++ [pjs]
          mapRef = if not optionsSourceMaps then ""
                   else "//# sourceMappingURL=index.js.map\n"

      liftBase $ B8.writeFile jsPath (T.encodeUtf8 $ js <> mapRef)
      forM_ mMaps $ \maps -> do
        genSourceMap dir (jsPath </> ".map") (length prefix) maps

genSourceMap
  :: MonadBaseControl IO m => FilePath -> String -> Int -> [SMap] -> m ()
genSourceMap dir mapFile extraLines mappings = do
  let pathToDir = iterate (".." </>) ".." !! length (splitPath $ normalise dir)
      sourceFile = case mappings of
                    (SMap file _ _ : _) -> Just $ pathToDir </> makeRelative dir (T.unpack file)
                    _ -> Nothing
  let rawMapping = SourceMapping {
        smFile = "index.js",
        smSourceRoot = Nothing,
        smMappings = flip map mappings $ \(SMap _ orig gen) -> Mapping {
            mapOriginal = Just $ convertPos $ add 0 (-1) orig
          , mapSourceFile = sourceFile
          , mapGenerated = convertPos $ add (extraLines+1) 0 gen
          , mapName = Nothing
          }
        }
  let mapping = generate rawMapping
  liftBase $ BL8.writeFile mapFile (encode mapping)
  where
  add :: Int -> Int -> SourcePos -> SourcePos
  add n m (SourcePos n' m') = SourcePos (n+n') (m+m')

  convertPos :: SourcePos -> Pos
  convertPos SourcePos { sourcePosLine = l, sourcePosColumn = c } =
    Pos { posLine = fromIntegral l, posColumn = fromIntegral c }

-- Use a join on the module source table to get the path.
-- getCachedBuildFromDatabase :: Build m => ResolvedModuleRef -> m (Maybe CachedBuild)
-- getCachedBuildFromDatabase rmref = withConn (query error $ "getCachedBuildFromDatabase " <> show rmref

compileModule
  :: Build m
  => [ExternsFile]
  -> Map ModuleName PackageRef
  -> Module
  -> Maybe FilePath
  -> m ExternsFile
compileModule externs prefs modl maybeForeignPath = do
  traceM $ "Compiling " <> renderModuleName (getModuleName modl)
  let env = foldl' (flip applyExternsFileToEnvironment) initEnvironment externs
      -- This adds `import Prim` and `import qualified Prim` to the
      -- module. Is this really necessary? It seems redundant and
      -- inefficient. Instead, these modules could just be treated
      -- specially when resolving them.
      withPrim = importPrim modl
      moduleName = getModuleName modl
  -- traceM $ "Compiling " <> renderModuleName moduleName <> " with dependencies "
  --       <> show (map (renderModuleName . efModuleName) externs)
  lint withPrim
  ((Module ss coms _ elaborated exps, env'), nextVar) <- runSupplyT 0 $ do
    desugar externs [withPrim] >>= \case
      [desugared] -> runCheck' (emptyCheckState env) $ typeCheckModule desugared
      _ -> error "desugar did not return a singleton"

  -- desugar case declarations *after* type- and exhaustiveness checking
  -- since pattern guards introduces cases which the exhaustiveness checker
  -- reports as not-exhaustive.
  (deguarded, nextVar') <- runSupplyT nextVar $ desugarCaseGuards elaborated


  regrouped <- createBindingGroups moduleName . collapseBindingGroups $ deguarded
  let mod' = Module ss coms moduleName regrouped exps
      corefn = CF.moduleToCoreFn env' mod'
      optimized = CF.optimizeCoreFn corefn
      finalCoreModule = renameInModule optimized
      requiresForeign = not $ null $ CF.moduleForeign finalCoreModule
      exts = moduleToExternsFile prefs mod' env'

  -- Check that FFI files are provided if needed
  let toError = errorMessage' (CF.moduleSourceSpan finalCoreModule)
  case (maybeForeignPath, CF.moduleForeign finalCoreModule) of
    (Just path, []) -> tell $ toError $ UnnecessaryFFIModule moduleName path
    (Nothing, _:_) -> throwError $ toError $ MissingFFIModule moduleName
    (Just path, _:_) -> MA.checkForeignDecls optimized path
    _ -> pure ()

  -- Generate the javascript
  output <- getConfig bcOutput
  options <- getConfig bcCompilerOptions
  codegen options (output </> renderModuleName moduleName) finalCoreModule maybeForeignPath
  return exts

parseModule :: Build m => FilePath -> B8.ByteString -> m Module
parseModule path source = do
  case P.lex path (T.decodeUtf8 source) >>= P.runTokenParser path P.parseModule of
    Left err -> throwSimpleError $ ErrorParsingModule err
    Right modl -> pure modl

-- | TODO switch this to only read as much from the file as it needs
parseModuleImports :: Build m => FilePath -> B8.ByteString -> m [ModuleRef]
parseModuleImports path source = do
  allImports <- getModuleImports <$> parseModule path source
  pure $ ordNub $ filter (not . isPrim . mrName) allImports

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
          traceM $ "Searching for module ref " <> prettyModuleRef mr
--          traceM $ prettyModuleRef mr <> " was not found in " <> prettyPackageRef pref
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
      let loadModules (ModulesAndRoot root mods) = do
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
              let rows = flip map mods $ \(DiscoveredModule{..}) ->
                    (pref, dmInferredName, dmPath, dmForeign)
              traceM $ "Inserting modules for package " <> prettyPackageRef pref
                    <> ": " <> show (map (renderModuleName . dmInferredName) mods)
              executeMany insertModuleQ rows conn
              -- As a last step grab all of the newly inserted rows
              query getPackageModulesQ (Only pref) conn

            -- Construct the package record.
            pure $ PackageMeta root $ M.fromList namesAndIds
      case pref of
        LocalPackage -> do
          traceM $ "Loading local modules"
          src <- bcLocalSourceDirectory . bvConfig <$> ask
          localMods <- liftBase $ findModulesIn src
          traceM $ "Found these modules: " <> show (map show localMods)
          loadModules (ModulesAndRoot src localMods)
        DepPackage pname -> do
          traceM $ "Loading modules of " <> show pname
          locations :: [FilePath] <- bcPackageLocations . bvConfig <$> ask
          discoverPackage pname locations >>= \case
            Uncompiled modsAndRoot -> loadModules modsAndRoot
            Precompiled _manifestPath -> error "precompiled manifests not yet implemented"

-- | Get the path to a resolved module. Also returns the path to its
-- FFI file, if one was discovered.
getModulePaths :: Build m => ResolvedModuleRef -> m (FilePath, Maybe FilePath)
getModulePaths rmref = do
  -- Make sure the package has been loaded
  PackageMeta _ _ <- getPackageMeta (rmrPackageRef rmref) >>= wait
  withConn (query getModulePathsQ (rmrModuleId rmref)) >>= \case
    (path, frns):_ -> pure (path, frns)
    _ -> internalError $ "Module " <> prettyRMRef rmref <> " wasn't in DB"

-- | Asynchronously load a module record.
-- Since this will recur on dependencies of a module, it can detect cycles.
getModuleRecord
  :: forall m. Build m => ModuleTrace -> ResolvedModuleRef -> m (AsyncLoad m ModuleRecord)
getModuleRecord trace = loadAsync bvModuleRecords getRecord where
  getRecord :: ResolvedModuleRef -> m ModuleRecord
  getRecord (r@(ResolvedModuleRef mId pref mname)) = case checkCycle r trace of
    -- Cycle detection. TODO incorporate packages into trace
    Just pkgs -> do
      throwSimpleError $ CycleInModules (map (someModuleNamed . rmrModuleName) pkgs)
    Nothing -> do
      -- Get path to the module and latest timestamps (including
      -- foreign if one exists). We'll have to do this regardless of
      -- whether we have a cached version in the database.
      (path, frn) <- getModulePaths r
      modStamp <- liftBase $ readStamp path
      latestStamp <- case frn of
        Nothing -> pure modStamp
        Just frnPath -> do
          frnStamp <- liftBase $ readStamp frnPath
          pure (max frnStamp modStamp)

      -- See if it's in the database; if so it will have a timestamp.
      (wasNew, depRefs :: [ResolvedModuleRef]) <- do
        withConn (query getModuleStampQ mId) >>= \case
          -- If there's an entry in the database, check if it is up-to-date
          (Only (Just s)):_ | s `isUpToDateAgainst` modStamp -> (False,) <$> do
            traceM $ "Loading cached dependency list of " <> renderModuleName mname
            rows <- withConn (query getModuleDependsQ mId)
            pure $ flip map rows $ \(mid, pref, mn) -> ResolvedModuleRef mid pref mn
          _ -> (True,) <$> do
            -- The file changed or there was no cached version; we
            -- need to get the latest import list from the file.
            source <- liftBase $ B8.readFile path
            traceM $ "Parsing imports of " <> renderModuleName mname
            importRefs <- parseModuleImports path source
            -- Resolve these module references
            forM importRefs (resolveModuleRef pref) >>= mapM wait

      -- Recur on dependencies
      depRecords :: [ModuleRecord] <- do
        asyncs <- forM depRefs $ getModuleRecord (pushTrace r trace)
        mapM wait asyncs

      -- Get the most recent timestamp, including among dependencies
      let stamp = foldr (<>) latestStamp (mrStamp <$> depRecords)

      -- If we are (re)loading, store in the database
      when wasNew $ do
        withConn $ \conn -> do
          execute insertModuleStampQ (stamp, mId) conn
          execute removeModuleDependsQ mId conn -- in case it is being re-loaded
          let rows = flip map depRefs $ \dep -> (mId, rmrModuleId dep)
          executeMany insertModuleDependsQ rows conn

      pure $ ModuleRecord path frn stamp depRefs

--         -- TODO: DRY up the logic shared with the code block above
--         _ -> do
--           traceM $ "Recording dependencies of " <> prettyRMRef r
--           -- No dependencies recorded in the database.
--           source <- liftBase $ B8.readFile path
--           imports <- parseModuleImports path source
--           let importsWithoutPrims = filter (not . isPrim . mrName) imports
--           resolvedImportAsyncs <- forConcurrently importsWithoutPrims $ \mref -> do
--             -- Note that we're passing in the package ref of *this* module; i.e. the one
--             -- currently being loaded. By adding this info we can unambiguously determine which
--             -- package/module combo is the correct answer for this package.
--             rmref :: ResolvedModuleRef <- do
--               resolveModuleRef (rmrPackageRef r) mref >>= wait
--             (rmref,) <$> getModuleRecord (pushTrace r trace) rmref

-- --          traceM $ "2"
--           depInfos :: [(ResolvedModuleRef, ModuleRecord)] <- do
--             forM resolvedImportAsyncs $ \(rmref, a) -> do
--               recd :: ModuleRecord <- wait a
--               pure (rmref, recd)

--           let depStamps = map (mrStamp . snd) depInfos
--           let stamp' = foldr (<>) stamp depStamps

-- --          traceM $ "3"
--           withConn $ \conn -> do
--             -- Create a module dependency list
--             execute insertModuleStampQ (stamp', mId) conn
--             let depIds = map (rmrModuleId . fst) depInfos
--             executeMany insertModuleDependsQ ((mId,) <$> depIds) conn

--           pure $ ModuleRecord path stamp (map fst resolvedImportAsyncs)


-- | Given a resolved module reference, compile it into externs. This result is cached.
buildResolvedModule
  :: forall m. Build m
  => ResolvedModuleRef
  -- Returns the compiled externs, a timestamp, and all imported
  -- modules (for downstream packages to add)
  -> m (AsyncLoad m CompilationArtifact)
buildResolvedModule = loadAsync bvExternCompiles $ \rmref -> do
  -- Get dependencies from the record and recur on them first.
  ModuleRecord path frn modStamp depRefs <- getModuleRecord newTrace rmref >>= wait
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

    depExts :: [ExternsFile] = reverse $ snd $ do
      let f ((pref, mn), ef) (seen, ordered) = go seen ordered pref mn ef
      fmap (\(_, _, e) -> e) <$> foldr f (mempty, mempty) (M.toList allDeps)

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
      exts <- Right <$> compileModule depExts prefMap modl frn
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
  -- Uncomment these lines to remove the database
  when bcFromFreshManifest $ do
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
