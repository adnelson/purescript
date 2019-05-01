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
import qualified Data.Text as T
import qualified Language.PureScript as P
import           Language.PureScript.Errors.JSON
import           Language.PureScript.Make
import           System.FilePath ((</>), takeFileName, takeBaseName, takeExtension)
import qualified System.Console.ANSI as ANSI
import qualified System.Directory as D
import           System.Exit (exitSuccess, exitFailure)
import           System.Directory (getCurrentDirectory)
import           System.IO (hPutStrLn, stderr)
import qualified System.IO as IO
import           System.IO.UTF8 (readUTF8FileT)

data PSCMakeOptions = PSCMakeOptions
  { pscmPackageLocations  :: [FilePath]
  , pscmDependentPackages :: [Text]
  , pscmSourceDirectories :: [FilePath]
  , pscmOutputDir         :: FilePath
  , pscmOpts              :: P.Options
  , pscmUsePrefix         :: Bool
  , pscmJSONErrors        :: Bool
  }

opts :: PSCMakeOptions
opts = PSCMakeOptions {
  pscmPackageLocations = ["thetest/js/bower_components"],
  pscmDependentPackages = ["purescript-prelude"],
  pscmSourceDirectories = ["thetest/js/bower_components/purescript-prelude/src", "thetest/input"],
  pscmOutputDir = "thetest/output",
  pscmOpts = P.defaultOptions { P.optionsVerboseErrors = True },
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
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering

  scannedDirectories <- forM pscmSourceDirectories $ \root -> do
    files <- readSourceDirectory root
    pure (root, files)
  (makeErrors, makeWarnings) <- runMake pscmOpts $ do
    -- Read dependencies
    let allFiles = mconcat $ map (uncurry sfsToTuples) scannedDirectories
    ms <- P.parseModulesFromFiles id allFiles
    let filePathMap = M.fromList $ map (\(fp, P.Module _ _ mn _ _) -> (mn, Right fp)) ms
    foreigns <- inferForeignModules filePathMap
    let makeActions = buildMakeActions pscmOutputDir filePathMap foreigns pscmUsePrefix
    P.make makeActions (map snd ms)
  printWarningsAndErrors (P.optionsVerboseErrors pscmOpts) pscmJSONErrors makeWarnings makeErrors
  exitSuccess

-- | Metadata about a source file (e.g. some purescript source file)
data SourceFile = SourceFile {
  sfModuleName :: P.ModuleName,
  -- ^ Relative path of the source file.
  sfContents :: Text
  -- ^ The raw contents of the file.
  } deriving (Eq, Ord)

instance Show SourceFile where
  show (SourceFile p _) = "SourceFile(" <> show p <> ")"

sfToPath :: FilePath -> SourceFile -> FilePath
sfToPath root (SourceFile name _) = root </> P.moduleNameToRelPath name

-- | Convert a SourceFile to a tuple, given its root path
sfsToTuples :: FilePath -> [SourceFile] -> [(FilePath, Text)]
sfsToTuples root = map (\p -> (sfToPath root p, sfContents p))

-- | Collect all of the source files from a given root directory.
readSourceDirectory :: FilePath -> IO [SourceFile]
readSourceDirectory root = do
  putStrLn $ "Discovering source files in " <> root
  found <- execStateT (go (P.ModuleName [])) []
  putStrLn $ "Found " <> show (length found) <> " files"
  pure found
  where
  -- Characters which are allowed to appear in module names
  validChars = S.fromList $ ['0'..'9'] <> ['a'..'z'] <> ['A'..'Z'] <> "_'"

  -- Check if a given (absolute) path is a valid directory to contain submodules
  isValidPursSubdir path = case takeFileName path of
    (c:cs) | isUpper c && all (flip S.member validChars) cs -> do
      D.doesDirectoryExist path
    _ -> pure False

  -- Check if a given (absolute) path is valid to be a purescript module
  isValidPursFile path = case (takeBaseName path, takeExtension path) of
    (c:cs, ".purs") | isUpper c && all (flip S.member validChars) cs -> do
      D.doesFileExist path
    _ -> pure False

  go :: P.ModuleName -> StateT [SourceFile] IO ()
  go modName = do
    let dir = root </> P.moduleNameToRelPath modName
    files <- liftIO $ D.listDirectory dir
    pursFiles <- filterM (liftIO . isValidPursFile . (dir </>)) files
    subdirs <- filterM (liftIO . isValidPursSubdir . (dir </>)) files

    -- Collect files in this directory
    forM_ pursFiles $ \filename -> do
      contents <- liftIO $ readUTF8FileT (dir </> filename)
      -- Strip off the ".purs" extension
      let baseModName = P.ProperName $ T.dropEnd 5 $ T.pack filename
      let modName' = P.addModuleName baseModName modName
      let sf = SourceFile { sfModuleName = modName', sfContents = contents }
      modify (sf :)

    -- Recur on subdirectories
    forM_ subdirs $ \subdir -> do
      go (P.addModuleName (P.ProperName $ T.pack subdir) modName)
