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

import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text.Encoding as T
import Database.SQLite.Simple
import Control.Monad.Base
import Control.Monad.Supply
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Writer.Class (MonadWriter(..))
import Control.Monad.Reader.Class (MonadReader(..), asks)


import Language.PureScript.Names
import Language.PureScript.AST
import Language.PureScript.Errors (MultipleErrors(..), errorMessage)
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
  cbResolvedModuleRef :: ResolvedModuleRef,
  cbExterns :: !ExternsFile,
  cbHash :: !Hash,
  cbPath :: !FilePath
  } deriving (Show)

-- | Distinct from the normal moduleref type, which can refer to either a
-- module within a particular package, or just "some module with this
-- name". This one always refers to one or the other category of
-- module.
data ResolvedModuleRef
  = LocalModule !ModuleName
  | PackageModule !PackageName !ModuleName
  deriving (Show, Eq, Ord)

data BuildVars m = BuildVars {
  bvConn :: MVar Connection,
  bvPathSearches :: MVar (Map ModuleRef (Async (StM m (ResolvedModuleRef, FilePath)))),
  bvExternCompiles :: MVar (Map ResolvedModuleRef (Async (StM m (ExternsFile, Hash))))
  }

type Build m = (MonadBaseControl IO m, MonadReader (BuildVars m) m,
                MonadError MultipleErrors m, MonadWriter MultipleErrors m)

throwSimpleError :: Build m => SimpleErrorMessage -> m a
throwSimpleError msg = throwError $ errorMessage msg

  -- Use a join on the module source table to get the path.
getCachedBuildFromDatabase :: Build m => ModuleRef -> m (Maybe CachedBuild)
getCachedBuildFromDatabase mref = error $ "getCachedBuildFromDatabase " <> show mref

cacheBuildInDatabase :: Build m => CachedBuild -> m ()
cacheBuildInDatabase _ = error $ "cacheBuildInDatabase"

getPathToModule :: Query
getPathToModule = undefined

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
      search <- async $ getSourcePathDBCached mref
      pure (M.insert mref search searches, search)

-- | Look up the path to a module on disk. These are cached in the database.
-- At this step we can also determine if a module is local or packaged.
getSourcePathDBCached :: Build m => ModuleRef -> m (ResolvedModuleRef, FilePath)
getSourcePathDBCached mref@(ModuleReference maybePkgName mname) = do
  mv <- asks bvConn
  withMVar mv $ \conn -> do
    case maybePkgName of
      Just pname -> liftBase (query conn getPathToModule (pname, mname)) >>= \case
        [] -> searchForModule mref
        Only path:_ -> pure (PackageModule pname mname, path)
      Nothing -> liftBase (query conn getPathToModule (Only mname)) >>= \case
        [] -> searchForModule mref
        [(pname', path)] -> case pname' of
          Nothing -> pure (LocalModule mname, path)
          Just p -> pure (PackageModule p mname, path)
        _ -> throwSimpleError $ AmbiguousModule mref

searchForModule :: Build m => ModuleRef -> m (ResolvedModuleRef, FilePath)
searchForModule = undefined

buildModuleDBCached :: Build m => ModuleRef -> m (ExternsFile, Hash)
buildModuleDBCached mref = do
  getCachedBuildFromDatabase mref >>= \case
    Nothing -> do
      -- Get the path to the file on disk
      (rmref, path) <- wait =<< getSourcePath mref
      source <- liftBase $ B8.readFile path
      modl <- parseModule path source
      deps <- buildModules $ someModuleNamed <$> getModuleImports modl
      let hash = MD5.finalize $ MD5.updates MD5.init (source : map snd deps)
      externs <- compileModule (map fst deps) modl
      (externs, hash) <$ cacheBuildInDatabase (CachedBuild rmref externs hash path)

    Just (CachedBuild rmref externs storedHash path) -> do
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
          (externs', hash) <$ cacheBuildInDatabase (CachedBuild rmref externs hash path)

-- | Build a list of modules in parallel.
buildModules :: Build m => [ModuleRef] -> m [(ExternsFile, Hash)]
buildModules mrefs = forConcurrently mrefs $ \mref -> buildModule mref >>= wait

-- NOTE: Is this the spot to do cycle detection?
buildModule :: forall m. Build m => ModuleRef -> m (Async (StM m (ExternsFile, Hash)))
buildModule mref = do
  (resolvedRef, _ :: FilePath) <- wait =<< getSourcePath mref
  mv <- asks bvExternCompiles
  modifyMVar mv $ \jobs -> case M.lookup resolvedRef jobs of
    -- If there's already a job going, wait for it to complete,
    -- then cache the result.
    Just job -> pure (jobs, job)
    Nothing -> do
      job <- async $ buildModuleDBCached mref
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
