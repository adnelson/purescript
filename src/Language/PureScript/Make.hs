module Language.PureScript.Make
  (
  -- * Make API
  rebuildModule
  , make
  , inferForeignModules
  , buildModuleNamed
  , module Monad
  , module Actions
  ) where

import           Prelude.Compat

import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Control.Concurrent.Async.Lifted (forConcurrently)
import           Control.Concurrent.Lifted as C
import           Control.Monad hiding (sequence)
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.IO.Class
import           Control.Monad.Supply
import           Control.Monad.Trans.Control (MonadBaseControl(..))
import           Control.Monad.Writer.Class (MonadWriter(..))
import           Data.Aeson (encode)
import           Data.Function (on)
import           Data.Foldable (for_)
import           Data.List (foldl', sortBy)
import qualified Data.List.NonEmpty as NEL
import           Data.Maybe (fromMaybe, isJust)
import qualified Data.Map as M
import qualified Data.Set as S
-- import qualified Data.Text as T
import           Language.PureScript.AST
import           Language.PureScript.Crash
import           Language.PureScript.Environment
import           Language.PureScript.Errors
import           Language.PureScript.Externs
import           Language.PureScript.Linter
import           Language.PureScript.ModuleDependencies
import           Language.PureScript.Names
import           Language.PureScript.Parser.Declarations
import           Language.PureScript.Renamer
import           Language.PureScript.Sugar
import           Language.PureScript.TypeChecker
import           Language.PureScript.Make.BuildPlan
import qualified Language.PureScript.Make.BuildPlan as BuildPlan
import           Language.PureScript.Make.Actions as Actions
import           Language.PureScript.Make.Monad as Monad
import qualified Language.PureScript.CoreFn as CF
import           System.Directory (doesFileExist)
import           System.FilePath (replaceExtension)
import           System.IO.UTF8 (readUTF8FileT)

-- | Rebuild a single module.
--
-- This function is used for fast-rebuild workflows (PSCi and psc-ide are examples).
rebuildModule
  :: forall m
   . (Monad m, MonadBaseControl IO m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => MakeActions m
  -> [ExternsFile]
  -> Module
  -> m ExternsFile
rebuildModule MakeActions{..} externs m@(Module _ _ moduleName _ _) = do
  progress $ CompilingModule moduleName
  let env = foldl' (flip applyExternsFileToEnvironment) initEnvironment externs
      withPrim = importPrim m
  lint withPrim
  ((Module ss coms _ elaborated exps, env'), nextVar) <- runSupplyT 0 $ do
    desugar externs [withPrim] >>= \case
      [desugared] -> runCheck' (emptyCheckState env) $ typeCheckModule desugared
      _ -> internalError "desugar did not return a singleton"

  -- desugar case declarations *after* type- and exhaustiveness checking
  -- since pattern guards introduces cases which the exhaustiveness checker
  -- reports as not-exhaustive.
  (deguarded, nextVar') <- runSupplyT nextVar $ do
    desugarCaseGuards elaborated

  regrouped <- createBindingGroups moduleName . collapseBindingGroups $ deguarded
  let mod' = Module ss coms moduleName regrouped exps
      corefn = CF.moduleToCoreFn env' mod'
      optimized = CF.optimizeCoreFn corefn
      [renamed] = renameInModules [optimized]
      exts = moduleToExternsFile mempty mod' env'
  ffiCodegen renamed
  evalSupplyT nextVar' . codegen renamed env' . encode $ exts
  return exts

parseModuleByName
  :: forall m
   . (Monad m, MonadIO m, MonadBaseControl IO m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => MakeActions m
  -> Maybe PackageName
  -> ModuleName
  -> m Module
parseModuleByName MakeActions{..} pkgName modName = do
  path <- getModulePath pkgName modName
  contents <- liftIO $ readUTF8FileT path
  case parseModuleFromFile id (path, contents) of
    Left err -> throwError $ errorMessage $ ErrorParsingModule err
    Right (_, modl) -> pure modl

buildModuleNamed
  :: forall m
   . (Monad m, MonadIO m, MonadBaseControl IO m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => MakeActions m
  -> ModuleName
  -> Maybe PackageName
  -> m (ExternsFile, UTCTime)
buildModuleNamed ma@(MakeActions{..}) moduleName pkgName = do
  cached <- getCachedExterns moduleName pkgName
  getInputTimestamp moduleName >>= \case
    Left _ -> error "idk"
    Right inputStamp -> do
      externsAndStamps :: [(ExternsFile, UTCTime)] <- undefined -- do
  --    parU' (getModuleImports m) buildModuleByName
      let stamps :: [UTCTime] = map snd externsAndStamps
      let comparisonStamp :: UTCTime = inputStamp -- maximum $ inputStamp : stamps
      let rebuild = do
            stamp <- liftIO getCurrentTime
            modl <- parseModuleByName ma pkgName moduleName
            externs <- rebuildModule ma (map fst externsAndStamps) modl
            storeExternsFile moduleName externs stamp pkgName
            pure (externs, stamp)

      case cached of
        Nothing -> rebuild
        Just (_, stamp) | comparisonStamp > stamp -> rebuild
        Just (externs, stamp) -> pure (externs, stamp)


-- | Compiles in "make" mode, compiling each module separately to a @.js@ file and an @externs.json@ file.
--
-- If timestamps have not changed, the externs file can be used to provide the module's types without
-- having to typecheck the module again.
make :: forall m. (Monad m, MonadIO m, MonadBaseControl IO m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
     => MakeActions m
     -> [Module]
     -> m [ExternsFile]
make ma@MakeActions{..} ms = do
  checkModuleNames
  (sorted, graph, dependencyExterns) <- sortModules readExternsFile ms

  buildPlan <- BuildPlan.construct ma sorted graph dependencyExterns

  let toBeRebuilt = filter (BuildPlan.needsRebuild buildPlan . getModuleName) sorted
  for_ toBeRebuilt $ \m -> fork $ do
    let deps = fromMaybe (internalError "make: module not found in dependency graph.") (lookup (getModuleName m) graph)
    buildModule buildPlan (importPrim m) (deps <> M.keys dependencyExterns)

  -- Wait for all threads to complete, and collect errors.
  errors <- BuildPlan.collectErrors buildPlan

  -- All threads have completed, rethrow any caught errors.
  unless (null errors) $ throwError (mconcat errors)

  -- Collect all ExternsFiles
  results <- BuildPlan.collectResults buildPlan

  -- Here we return all the ExternsFile in the ordering of the topological sort,
  -- so they can be folded into an Environment. This result is used in the tests
  -- and in PSCI.
  let lookupResult mn = fromMaybe (internalError "make: module not found in results") (M.lookup mn results)
  return (map (lookupResult . getModuleName) sorted)

  where
  checkModuleNames :: m ()
  checkModuleNames = checkNoPrim *> checkModuleNamesAreUnique

  checkNoPrim :: m ()
  checkNoPrim =
    for_ ms $ \m ->
      let mn = getModuleName m
      in when (isBuiltinModuleName mn) $
           throwError
             . errorMessage' (getModuleSourceSpan m)
             $ CannotDefinePrimModules mn

  checkModuleNamesAreUnique :: m ()
  checkModuleNamesAreUnique =
    for_ (findDuplicates getModuleName ms) $ \mss ->
      throwError . flip foldMap mss $ \ms' ->
        let mn = getModuleName (NEL.head ms')
        in errorMessage'' (fmap getModuleSourceSpan ms') $ DuplicateModule mn

  -- Find all groups of duplicate values in a list based on a projection.
  findDuplicates :: Ord b => (a -> b) -> [a] -> Maybe [NEL.NonEmpty a]
  findDuplicates f xs =
    case filter ((> 1) . length) . NEL.groupBy ((==) `on` f) . sortBy (compare `on` f) $ xs of
      [] -> Nothing
      xss -> Just xss

  -- Sort a list so its elements appear in the same order as in another list.
  -- TODO I think the above comment is incorrect...? This looks like a union operation.
  inOrderOf :: (Ord a) => [a] -> [a] -> [a]
  inOrderOf xs ys = let s = S.fromList xs in filter (`S.member` s) ys

  buildModule :: BuildPlan -> Module -> [ModuleName] -> m ()
  buildModule buildPlan m@(Module _ _ moduleName _ _) deps = do
    let
      complete :: Either MultipleErrors (MultipleErrors, ExternsFile) -> m ()
      complete = BuildPlan.markComplete buildPlan moduleName
    flip catchError (complete . Left) $ do
      liftIO $ putStrLn $ "Building module " <> renderModuleName moduleName <> " with deps " <> show deps
      -- We need to wait for dependencies to be built, before checking if the current
      -- module should be rebuilt, so the first thing to do is to wait on the
      -- MVars for the module's dependencies.
      externs <- getDependenciesTrans buildPlan moduleName deps
      liftIO $ putStrLn $ renderModuleName moduleName <> " full dependendencies: "
                        <> show (map renderModuleName $ M.keys externs)
      -- mexterns <- fmap unzip . sequence <$> traverse (getResult buildPlan readExternsFile) deps

      -- liftIO $ putStrLn $ "Found " <> show (length externs) <> " externs for " <> renderModuleName moduleName
      case M.lookup moduleName externs of
        Just externsFile -> do
          liftIO $ putStrLn $ renderModuleName moduleName <> " was already built"
          complete $ Right (mempty, externsFile)
        _ -> do
          (exts, warnings) <- listen $ rebuildModule ma (M.elems externs) m
          complete $ Right (warnings, exts)

-- | Infer the module name for a module by looking for the same filename with
-- a .js extension.
inferForeignModules
  :: forall m
   . MonadIO m
  => M.Map ModuleName (Either RebuildPolicy FilePath)
  -> m (M.Map ModuleName FilePath)
inferForeignModules =
    fmap (M.mapMaybe id) . traverse inferForeignModule
  where
    inferForeignModule :: Either RebuildPolicy FilePath -> m (Maybe FilePath)
    inferForeignModule (Left _) = return Nothing
    inferForeignModule (Right path) = do
      let jsFile = replaceExtension path "js"
      exists <- liftIO $ doesFileExist jsFile
      if exists
        then return (Just jsFile)
        else return Nothing
