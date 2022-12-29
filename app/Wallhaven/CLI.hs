module Wallhaven.CLI (runCLIApp) where

import Control.Monad.Reader (ReaderT (runReaderT))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC8
import Options.Applicative
import Types
import UnliftIO (MonadIO)
import UnliftIO.Environment (getEnv)
import Util.Time (seconds)
import Wallhaven.Favorites (syncAllWallpapers)

wallhavenCookieEnvVarName :: String
wallhavenCookieEnvVarName = "WALLHAVEN_COOKIE"

defaultWallpaperDir :: FilePath
defaultWallpaperDir = "/Users/home/wallpapers"

-- Load cookie from environment variable.
loadCookieFromEnv :: MonadIO m => m ByteString
loadCookieFromEnv = BC8.pack <$> getEnv wallhavenCookieEnvVarName

data CLIOpts = CLIOpts
  { cliOptsWallpaperDir :: FilePath,
    cliOptsNumParallelDownloads :: Int,
    cliOptsNumRetries :: Int,
    cliOptsRetryDelay :: Int
  }

-- Parser for command line options.
cliOptsParser :: Parser CLIOpts
cliOptsParser =
  CLIOpts
    <$> strOption
      ( long "wallpaper-dir"
          <> metavar "DIRECTORY"
          <> value defaultWallpaperDir
          <> showDefault
          <> help "Directory where wallpapers will be saved"
      )
    <*> option
      auto
      ( long "num-parallel-downloads"
          <> metavar "NUM"
          <> value 5
          <> showDefault
          <> help "Number of wallpapers to download in parallel"
      )
    <*> option
      auto
      ( long "num-retries"
          <> metavar "NUM"
          <> value 5
          <> showDefault
          <> help "Number of retries to perform when downloading a wallpaper"
      )
    <*> option
      auto
      ( long "retry-delay"
          <> metavar "NUM"
          <> value 3
          <> showDefault
          <> help "Number of seconds to wait between retries"
      )

cliOptsToConfig :: CLIOpts -> ByteString -> Config
cliOptsToConfig opts =
  Config
    (cliOptsWallpaperDir opts)
    (cliOptsNumParallelDownloads opts)
    (RetryConfig (cliOptsNumRetries opts) (seconds $ cliOptsRetryDelay opts))

runCLIApp :: IO ()
runCLIApp = do
  cliOpts <- execParser opts
  cookie <- loadCookieFromEnv
  let config = cliOptsToConfig cliOpts cookie
      env = Env config putStr
  runReaderT syncAllWallpapers env
  where
    opts =
      info
        (cliOptsParser <**> helper)
        ( fullDesc
            <> progDesc "Sync wallpapers from Wallhaven favorites"
            <> header "wallhaven-sync"
        )
