module Wallhaven.CLI (runCLIApp) where

import Control.Monad.Reader (ReaderT (runReaderT))
import Options.Applicative
import Types
import Util.Time (seconds)
import Wallhaven.Favorites (syncAllWallpapers)

defaultWallpaperDir :: FilePath
defaultWallpaperDir = "/Users/home/wallpapers"

data CLIOpts = CLIOpts
  { cliOptsWallpaperDir :: FilePath,
    cliOptsNumParallelDownloads :: Int,
    cliOptsNumRetries :: Int,
    cliOptsRetryDelay :: Int,
    cliOptsDeleteUnliked :: Bool,
    cliOptsWallhavenUsername :: String,
    cliOptsWallhavenAPIKey :: String,
    cliOptsCollectionLabel :: String
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
    <*> switch (long "delete-unliked" <> help "Delete unliked wallpapers")
    <*> strOption
      ( long "wallhaven-username"
          <> metavar "USERNAME"
          <> help "Wallhaven username"
      )
    <*> strOption
      ( long "wallhaven-api-key"
          <> metavar "API_KEY"
          <> help "Wallhaven API key"
      )
    <*> strOption
      ( long "collection-label"
          <> metavar "LABEL"
          <> help "Label of the collection to sync"
          <> value "Default"
          <> showDefault
      )

cliOptsToConfig :: CLIOpts -> Config
cliOptsToConfig opts =
  Config
    (cliOptsWallpaperDir opts)
    (cliOptsNumParallelDownloads opts)
    (RetryConfig (cliOptsNumRetries opts) (seconds $ cliOptsRetryDelay opts))
    (cliOptsDeleteUnliked opts)
    (cliOptsWallhavenUsername opts)
    (cliOptsWallhavenAPIKey opts)
    (cliOptsCollectionLabel opts)

runCLIApp :: IO ()
runCLIApp = do
  cliOpts <- execParser opts
  let config = cliOptsToConfig cliOpts
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
