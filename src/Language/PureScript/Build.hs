-- | A nix-style approach to building modules.
module Language.PureScript.Build where

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as B8

import Language.PureScript.Names
import Language.PureScript.Externs
import Language.PureScript.Make

-- This outlines a hash-based approach for package compilation. The building blocks:

data ModuleRef = ModuleRef (Maybe PackageName) ModuleName
-- Jobs are done asynchronously. They return the compiled interface,
-- and the unique has of the compilation.

type Job = Async (ExternsFile, Hash)
type Jobs = Map ModuleRef Job
type Hash = B.ByteString

data CachedBuild = CachedBuild {
  cbExterns :: !ExternsFile,
  cbStoredHash :: !Hash,
  cbPath :: !FilePath
  } deriving (Show)

-- Use a join on the module source table to get the path.
getCachedBuildFromDatabase :: ModuleRef -> m (Maybe CachedBuild)
getCachedBuildFromDatabase mref = error "getCachedBuildFromDatabase " <> show mref

cacheBuildInDatabase :: ModuleRef -> CachedBuild -> m ()
cacheBuildInDatabase mref _ = error "cacheBuildInDatabase " <> show mref

compileModule :: Module -> [ExternsFile] -> m ExternsFile
compileModule modl externs = error "compileModule not implemented"

getSourcePath :: ModuleRef -> m FilePath
getSourcePath = error $ "getSourcePath " <> show mref

buildModuleDBCached :: ModuleRef -> m (ExternsFile, Hash)
buildModuleDBCached mref = do
  getCachedBuildFromDatabase mref >>= \case
    Nothing -> do
      -- Get the path to the file on disk
      path <- getSourcePath mref
      source <- B8.readFile path
      case runTokenParser parseModule (B8.unpack source) of
        Left err -> error $ show err -- TODO
        Right modl -> do
          -- NOTE: cycle detection?
          deps <- forConcurrently (getModuleImports modl) $ \mn ->
            buildModule (ModuleRef Nothing mn)
          let hash = MD5.updates (source : map snd deps) MD5.init
          externs <- compileModule modl (map fst deps)
          (externs, hash) <$ cacheBuildInDatabase mref externs hash

      -- Parse its imports
      -- Recur on the
    Just (CachedBuild externs storedHash path) -> do
      -- Build dependencies first.
      let depRefs = ModuleRef Nothing <$> efImportedModuleNames externs
      deps <- mapConcurrently (buildModule jobsMV) depRefs
      -- Compute the hash as the contents of the source file, plus
      -- dependency hashes.
      source <- B.readFile path
      let hash = MD5.updates (source : map snd deps) MD5.init
      -- Rebuild if hash has changed, otherwise already done.
      if storedHash == hash then pure (externs, hash)
        else do
          -- A failure here could also be stored in the database, to
          -- prevent meaningless rebuilds...
          externs <- case runTokenParser parseModule (B8.unpack source) of
            Left err -> error $ show err -- TODO
            Right modl -> compileModule modl (map fst deps)
          (externs, hash) <$ cacheBuildInDatabase mref externs hash

buildModule :: ModuleRef -> m (ExternsFile, Hash)
buildModule mref = do
  jobsMV <- getJobsMV
  modifyMVar jobsMV $ \jobs -> case M.lookup mref jobs of
    -- If there's already a job going, wait for it to complete,
    -- then cache the result.
    Just job -> pure (job, jobs)
    Nothing -> do
      job <- async $ buildModuleDBCached mref
      pure (job, M.insert mref job jobs)
