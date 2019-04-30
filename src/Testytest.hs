module Testytest where

import Prelude.Compat

import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State.Strict (StateT, modify, execStateT)
import qualified Data.Aeson as A
import           Data.Bool (bool)
import           Data.Char (isUpper)
import qualified Data.ByteString.Lazy.UTF8 as LBU8
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Language.PureScript as P
import           Language.PureScript.Errors.JSON
import           Language.PureScript.Make
import           System.FilePath ((</>), takeFileName, takeBaseName, takeExtension)
import qualified System.Console.ANSI as ANSI
import qualified System.Directory as D
import           System.Exit (exitSuccess, exitFailure)
import           System.Directory (getCurrentDirectory)
import           System.IO (hPutStrLn, stderr)
import           System.IO.UTF8 (readUTF8FileT)

data PSCMakeOptions = PSCMakeOptions
  { pscmDependencies      :: [FilePath]
  , pscmSourceDirectories :: [FilePath]
  , pscmOutputDir         :: FilePath
  , pscmOpts              :: P.Options
  , pscmUsePrefix         :: Bool
  , pscmJSONErrors        :: Bool
  }

opts :: PSCMakeOptions
opts = PSCMakeOptions {
  pscmDependencies = [],
  pscmSourceDirectories = ["thetest/input"],
  pscmOutputDir = "thetest/output",
  pscmOpts = P.defaultOptions,
  pscmUsePrefix = True,
  pscmJSONErrors = True
  }

-- | Argumnets: verbose, use JSON, warnings, errors
printWarningsAndErrors :: Bool -> Bool -> P.MultipleErrors -> Either P.MultipleErrors a -> IO ()
printWarningsAndErrors verbose False warnings errors = do
  pwd <- getCurrentDirectory
  cc <- bool Nothing (Just P.defaultCodeColor) <$> ANSI.hSupportsANSI stderr
  let ppeOpts = P.defaultPPEOptions { P.ppeCodeColor = cc, P.ppeFull = verbose, P.ppeRelativeDirectory = pwd }
  when (P.nonEmpty warnings) $
    hPutStrLn stderr (P.prettyPrintMultipleWarnings ppeOpts warnings)
  case errors of
    Left errs -> do
      hPutStrLn stderr (P.prettyPrintMultipleErrors ppeOpts errs)
      exitFailure
    Right _ -> return ()
printWarningsAndErrors verbose True warnings errors = do
  hPutStrLn stderr . LBU8.toString . A.encode $
    JSONResult (toJSONErrors verbose P.Warning warnings)
               (either (toJSONErrors verbose P.Error) (const []) errors)
  either (const exitFailure) (const (return ())) errors

compile :: PSCMakeOptions -> IO ()
compile PSCMakeOptions{..} = do
  sourceFiles <- mconcat <$> forM pscmSourceDirectories readSourceDirectory
  (makeErrors, makeWarnings) <- runMake pscmOpts $ do
    ms <- P.parseModulesFromFiles id (map sfToTuple sourceFiles)
    let filePathMap = M.fromList $ map (\(fp, P.Module _ _ mn _ _) -> (mn, Right fp)) ms
    foreigns <- inferForeignModules filePathMap
    let makeActions = buildMakeActions pscmOutputDir filePathMap foreigns pscmUsePrefix
    P.make makeActions (map snd ms)
  printWarningsAndErrors (P.optionsVerboseErrors pscmOpts) pscmJSONErrors makeWarnings makeErrors
  exitSuccess

warnFileTypeNotFound :: String -> IO ()
warnFileTypeNotFound = hPutStrLn stderr . ("purs compile: No files found using pattern: " ++)

-- | Metadata about a source file (e.g. some purescript source file)
data SourceFile = SourceFile {
  sfPath :: FilePath,
  -- ^ Relative path of the source file.
  sfRoot :: FilePath,
  -- ^ Source root, that this file is relative to.
  sfContents :: Text
  -- ^ The raw contents of the file.
  } deriving (Show, Eq, Ord)

sfToTuple :: SourceFile -> (FilePath, Text)
sfToTuple (SourceFile p r c) = (r </> p, c)

-- | Collect all of the source files from a given root directory.
readSourceDirectory :: FilePath -> IO [SourceFile]
readSourceDirectory root = execStateT (go root) [] where
  validChars = S.fromList $ ['0'..'9'] <> ['a'..'z'] <> ['A'..'Z'] <> "_'"
  isValidPursSubdir path = do
    putStrLn $ "checking if " <> path <> " is a valid purescript subdirectory"
    case takeFileName path of
      (c:cs) | isUpper c && all (flip S.member validChars) cs -> do
        D.doesDirectoryExist path
      _ -> pure False
  isValidPursFile path = do
    putStrLn $ "checking if " <> path <> " is a valid purescript file"
    case (takeBaseName path, takeExtension path) of
      (c:cs, ".purs") | isUpper c && all (flip S.member validChars) cs -> do
        D.doesFileExist path
      _ -> do
        putStrLn $ path <> " is NOT a valid purescript file"
        pure False
  go :: FilePath -> StateT [SourceFile] IO ()
  go dir = do
    liftIO $ putStrLn $ "Scanning " <> dir
    files <- liftIO $ D.listDirectory dir
    pursFiles <- filterM (liftIO . isValidPursFile . (dir </>)) files
    subdirs <- filterM (liftIO . isValidPursSubdir . (dir </>)) files
    liftIO $ print (pursFiles, subdirs)
    -- Collect files in this directory
    forM_ pursFiles $ \filename -> do
      let path = dir </> filename
      contents <- liftIO $ readUTF8FileT path
      let sf = SourceFile { sfPath = path, sfRoot = root, sfContents = contents }
      modify (sf :)
    -- Recur on subdirectories
    forM_ subdirs $ \subdir -> go (dir </> subdir)
