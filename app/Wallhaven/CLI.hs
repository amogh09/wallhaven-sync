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

defaultParallelization :: NumParallelDownloads
defaultParallelization = 5

defaultRetryConfig :: RetryConfig
defaultRetryConfig = RetryConfig 5 (seconds 3)

cliOptsToConfig :: CLIOpts -> Config
cliOptsToConfig opts =
  Config
    (cliOptsWallpaperDir opts)
    defaultParallelization
    defaultRetryConfig
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
            <> progDesc "Sync wallpapers from a Wallhaven collection"
            <> header "wallhaven-sync"
        )
