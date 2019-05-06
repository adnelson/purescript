module Language.PureScript.Make.BuildPlan
  ( BuildPlan()
  , construct
--  , getResult
  , getDependenciesTrans
  , collectErrors
  , collectResults
  , markComplete
  , needsRebuild
  ) where

import           Prelude
import Debug.Trace

import           Control.Concurrent.Lifted as C
import           Control.Monad hiding (sequence)
import           Control.Monad.State.Strict
import           Control.Monad.Error
import           Control.Monad.Writer
import           Control.Monad.Trans.Control (MonadBaseControl(..))
import           Data.Aeson (decode)
import           Data.Either (lefts)
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Data.Version (showVersion)
import           Language.PureScript.AST
import qualified Language.PureScript.Constants as C
import           Language.PureScript.Crash
import           Language.PureScript.Errors
import           Language.PureScript.Externs
import           Language.PureScript.Make.Actions as Actions
import           Language.PureScript.Names (ModuleName, renderModuleName)
import qualified Paths_purescript as Paths

-- | The BuildPlan tracks information about our build progress, and holds all
-- prebuilt modules for incremental builds.
data BuildPlan = BuildPlan
  { bpPrebuilt :: M.Map ModuleName Prebuilt
  , bpDependencies :: M.Map ModuleName ExternsFile
  , bpBuildJobs :: M.Map ModuleName BuildJob
  }

data Prebuilt = Prebuilt
  { pbModificationTime :: UTCTime
  , pbExternsFile :: ExternsFile
  }

data BuildJob = BuildJob
  { bjResult :: C.MVar (Either MultipleErrors (MultipleErrors, ExternsFile))
  }

-- | Called when we finished compiling a module and want to report back the
-- compilation result, as well as any potential errors that were thrown.
markComplete
  :: (MonadBaseControl IO m)
  => BuildPlan
  -> ModuleName
  -> Either MultipleErrors (MultipleErrors, ExternsFile)
  -> m ()
markComplete buildPlan moduleName result = do
  let job = fromMaybe (internalError "make: markComplete no barrier") $ M.lookup moduleName (bpBuildJobs buildPlan)
  putMVar (bjResult job) result

-- | Whether or not the module with the given ModuleName needs to be rebuilt
needsRebuild :: BuildPlan -> ModuleName -> Bool
needsRebuild bp moduleName = M.member moduleName (bpBuildJobs bp)

-- | Collects errors for all modules that have been rebuilt. This will block
-- until all outstanding build jobs are finished.
collectErrors
  :: (MonadBaseControl IO m)
  => BuildPlan
  -> m [MultipleErrors]
collectErrors buildPlan  = do
  errors <- traverse (readMVar . bjResult) $ M.elems (bpBuildJobs buildPlan)
  pure $ lefts errors

-- | Collects ExternsFiles for all prebuilt as well as rebuilt modules. Panics
-- if any build job returned an error.
collectResults
  :: forall m. (MonadBaseControl IO m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => BuildPlan
  -> m (M.Map ModuleName ExternsFile)
collectResults buildPlan = do
  let externs = M.map pbExternsFile (bpPrebuilt buildPlan)
  barrierResults <- traverse (takeMVar . bjResult) $ bpBuildJobs buildPlan
  barrierExterns <- forM barrierResults $ \case
    Left errs -> throwError errs
    Right (warnings, ef) -> tell warnings >> pure ef
  pure (M.union externs barrierExterns)

-- | Resolve all of the modules listed in dependency list, including
-- transitive dependencies.
getDependenciesTrans
  :: forall m. (MonadBaseControl IO m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => BuildPlan
  -> ModuleName
  -> [ModuleName]
  -> m (M.Map ModuleName ExternsFile)
getDependenciesTrans plan moduleName deps = do
  (resolved, unresolved) <- flip execStateT (M.empty, S.singleton moduleName) $ mapM_ go deps
  when (not $ S.null unresolved) $ do
    internalError $ "make: some modules unresolved: "
                  <> show (map renderModuleName $ S.toList unresolved)
  pure resolved
  -- Resolve a particular module name, and recur on its dependencies.
  where
    -- Discover an ExternsFile's dependencies, and then return the ExternsFile.
    resolve ef = let modName = efModuleName ef in do
      modify (S.insert modName <$>)
      mapM_ go (efImportedModuleNames ef)
      modify (S.delete modName <$>)

    go :: ModuleName -> StateT (M.Map ModuleName ExternsFile, S.Set ModuleName) m ()
    go modName | C.isPrim modName = pure ()
               | otherwise  = S.member modName . snd <$> get >>= \case
      -- We're currently resolving this module; means there's a cycle
      True -> internalError "make: undetected cycle in imports"
      False -> M.lookup modName . fst <$> get >>= \case
        -- already resolved
        Just _ -> pure ()
        -- Otherwise we have to resolve it.
        -- Check dependencies first, since that's non-blocking. If it's
        -- not there, find a build job and wait for it to complete.
        Nothing -> case M.lookup modName (bpDependencies plan) of
          Just ef -> resolve ef
          Nothing -> case M.lookup modName (bpBuildJobs plan) of
            Nothing -> internalError $ "make: " <> renderModuleName modName
                                     <> " is not a local module, and wasn't found in dependencies"
            Just job -> readMVar (bjResult job) >>= \case
              Right (warnings, ef) -> tell warnings >> resolve ef
              Left errors -> throwError errors



-- -- | Gets the the build result for a given module name independent of whether it
-- -- was rebuilt or prebuilt. Prebuilt modules always return no warnings.
-- getResult
--   :: (MonadBaseControl IO m)
--   => BuildPlan
--   -> (ModuleName -> m (Maybe ExternsFile))
--   -> ModuleName
--   -> m (MultipleErrors, ExternsFile)
-- getResult buildPlan readExternsFile moduleName =
--   case M.lookup moduleName (bpPrebuilt buildPlan) of
--     Just es -> pure (MultipleErrors [], pbExternsFile es)
--     Nothing -> case M.lookup moduleName (bpBuildJobs buildPlan) of
--       Just job -> readMVar (bjResult job) >>= \case
--         Left err -> throw
--       Nothing -> readExternsFile moduleName >>= \case
--         Nothing -> internalError "make: no barrier"
--         Just externs -> pure (MultipleErrors [], externs)

-- | Constructs a BuildPlan for the given module graph.
--
-- The given MakeActions are used to collect various timestamps in order to
-- determine whether a module needs rebuilding.
construct
  :: forall m. (Monad m, MonadBaseControl IO m)
  => MakeActions m
  -> [Module]
  -> [(ModuleName, [ModuleName])]
  -> M.Map ModuleName ExternsFile
  -> m BuildPlan
construct MakeActions{..} sorted graph dependencyExterns = do
  prebuilt <- foldM findExistingExtern M.empty sorted
  let toBeRebuilt = filter (not . flip M.member prebuilt . getModuleName) sorted
  buildJobs <- foldM makeBuildJob M.empty (map getModuleName toBeRebuilt)
  transitiveDeps <- flip execStateT (Just <$> dependencyExterns) $ do
    mapM_ resolveDependencies (M.elems dependencyExterns)
  let fullDependencyExterns =
        fromMaybe (internalError "make: unresolved dependency module") <$> transitiveDeps
  pure $ BuildPlan prebuilt fullDependencyExterns buildJobs
  where

    -- Resolve modules from package dependencies
    resolveDependencies :: ExternsFile -> StateT (M.Map ModuleName (Maybe ExternsFile)) m ()
    resolveDependencies externsFile = do
      traceM $ "resolving dependencies of " <> renderModuleName (efModuleName externsFile)
      forM_ (efImportedModuleNames externsFile) $ \case
        modName | C.isPrim modName -> pure () -- prim modules are automatically resolved
        modName -> M.lookup modName <$> get >>= \case
          Just (Just _) -> pure () -- already resolved
          Just Nothing -> internalError "make: undetected cycle in imports"
          Nothing -> lift (readExternsFile modName) >>= \case
            Nothing -> internalError "make: externs were missing but no errors reported."
            Just ef -> do
              modify (M.insert modName Nothing)
              resolveDependencies ef
              modify (M.insert modName (Just ef))

    makeBuildJob prev moduleName = do
      buildJob <- BuildJob <$> C.newEmptyMVar
      pure (M.insert moduleName buildJob prev)

    findExistingExtern :: M.Map ModuleName Prebuilt -> Module -> m (M.Map ModuleName Prebuilt)
    findExistingExtern prev (getModuleName -> moduleName) = do
      traceM $ "getting extern for " <> renderModuleName moduleName
      outputTimestamp <- getOutputTimestamp moduleName
      let deps = fromMaybe (internalError "make: module not found in dependency graph.") (lookup moduleName graph)
      case traverse (fmap pbModificationTime . flip M.lookup prev) deps of
        Nothing -> do
          -- If we end up here, one of the dependencies didn't exist in the
          -- prebuilt map and so we know a dependency needs to be rebuilt, which
          -- means we need to be rebuilt in turn.
          pure prev
        Just modTimes -> do
          let dependencyTimestamp = maximumMaybe modTimes
          inputTimestamp <- getInputTimestamp moduleName -- timestamp of the source file
          let
            existingExtern = case (inputTimestamp, dependencyTimestamp, outputTimestamp) of
              (Right t1, Just t3, Just t2) ->
                -- The source file has a timestamp which is newer than the output,
                -- or one of its dependencies is newer than the output,
                -- Then the output must be rebuilt, a.k.a. return Nothing.
                if t1 > t2 || t3 > t2 then Nothing else Just t2
              (Right t1, Nothing, Just t2) ->
                -- The source file has a timestamp which is newer than the output,
                -- And there are no dependency timestamps.
                -- Then the output must be rebuilt.
                if t1 > t2 then Nothing else Just t2
              (Left RebuildNever, _, Just t2) ->
                Just t2
              _ ->
                Nothing
          case existingExtern of
            Nothing -> pure prev
            Just outputTime -> do
              -- Check if we already have an externsfile for this module; if so, return it.
              mexts <- readExternsFile moduleName
              case mexts of
                Just exts ->
                  pure (M.insert moduleName (Prebuilt outputTime exts) prev)
                Nothing -> pure prev

maximumMaybe :: Ord a => [a] -> Maybe a
maximumMaybe [] = Nothing
maximumMaybe xs = Just $ maximum xs
