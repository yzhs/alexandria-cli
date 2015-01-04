module AlexandriaCli (alexandria, defaultConfig) where

import qualified Config.Dyre as Dyre
import Control.Monad (liftM, void)
import System.Environment (getArgs)
import System.Process (callProcess, readProcess)
import System.FilePath ((</>))
import System.Posix (getEnv)

import Alexandria.Render
import Alexandria.Config

import AlexandriaCli.Stats

-- | Parse command line arguments and execute the corresponding functions.
alexandriaMain :: Configuration a => a -> IO ()
alexandriaMain conf = getArgs >>= handleArgs
  where handleArgs [] = return ()
        handleArgs ["-I"] = handleArgs ["--info"]
        handleArgs ["--info"] = printStats conf
        handleArgs ["-i"] = handleArgs ["--index"]
        handleArgs ["--index"] = putStrLn =<< runSwishe conf []
        handleArgs args = findDocs conf args >>= renderResults conf >>= mapM_ putStrLn >> return ()

configDir :: Maybe (IO FilePath)
configDir = Just $ do
  home <- getEnv "HOME"
  maybe (fail "$HOME not defined") return home

-- | This is the function that is called from the users configuration file to
-- provide the main function.
alexandria :: Configuration a => a -> IO ()
alexandria = Dyre.wrapMain $ Dyre.defaultParams {
    Dyre.projectName = "alexandria",
    Dyre.realMain = alexandriaMain,
    Dyre.showError = error "foo",
    Dyre.configDir = configDir
  }
