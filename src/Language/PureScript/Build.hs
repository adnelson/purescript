-- | A nix-style approach to building modules.
--
-- The build process goes bottom-up, beginning with some leaf-node
-- module, compiling all of its dependencies, and then building the
-- module. A build results in a unique hash, which is used to skip
-- unnecessary work.
--
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

compileModule :: [ExternsFile] -> Module -> m ExternsFile
compileModule externs modl = error "compileModule not implemented"

parseModule :: ByteString -> m ExternsFile
parseModule source = case runTokenParser parseModule (B8.unpack source) of
  Left err -> throwError $ ErrorParsingModule err
  Right modl -> pure modl

getSourcePath :: ModuleRef -> m FilePath
getSourcePath = error $ "getSourcePath " <> show mref

buildModuleDBCached :: ModuleRef -> m (ExternsFile, Hash)
buildModuleDBCached mref = do
  -- NOTE: We could maybe do cycle detection?
  recurOn :: [ModuleName] -> m (ExternsFile, Hash)
  recurOn modNames = parU' (ModuleRef Nothing <$> modNames) buildModule

  getCachedBuildFromDatabase mref >>= \case
    Nothing -> do
      -- Get the path to the file on disk
      path <- getSourcePath mref
      source <- liftIO $ B8.readFile path
      modl <- parseModule source
      deps <- recurOn $ getModuleImports modl
      let hash = MD5.updates (source : map snd deps) MD5.init
      externs <- compileModule (map fst deps) modl
      (externs, hash) <$ cacheBuildInDatabase mref externs hash

    Just (CachedBuild externs storedHash path) -> do
      -- Build dependencies first.
      deps <- recurOn $ efImportedModuleNames externs
      -- Hash the contents of the source file + dependency hashes to
      -- get the unique signature of the module we're building.
      source <- liftIO $ B8.readFile path
      let hash = MD5.updates (source : map snd deps) MD5.init
      -- If the hash matches, no more work needs to be done. Otherwise build.
      if storedHash == hash then pure (externs, hash)
        else do
          -- A failure here could also be stored in the database, to
          -- prevent meaningless rebuilds...
          externs <- parseModule >>= compileModule (map fst deps)
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
