module Wallhaven.CLI (runCLIApp) where

import Control.Monad.Reader (ReaderT (runReaderT))
import Options.Applicative
import Wallhaven.Action (syncAllWallpapers)
import Wallhaven.Env (Config (Config), Env (Env))
import Wallhaven.Types

defaultWallpaperDir :: FilePath
defaultWallpaperDir = "/Users/home/wallpapers"

data CLIOpts = CLIOpts
  { cliOptsWallpaperDir :: FilePath,
    cliOptsDeleteUnliked :: Bool,
    cliOptsWallhavenUsername :: String,
    cliOptsWallhavenAPIKey :: String,
    cliOptsCollectionLabel :: String,
    cliOptsDebug :: Bool
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
    <*> switch (long "debug" <> help "Debug mode")

defaultParallelization :: NumParallelDownloads
defaultParallelization = 5

cliOptsToConfig :: CLIOpts -> Config
cliOptsToConfig opts =
  Config
    (cliOptsWallpaperDir opts)
    defaultParallelization
    (cliOptsDeleteUnliked opts)
    (cliOptsWallhavenAPIKey opts)
    (cliOptsDebug opts)

runCLIApp :: IO ()
runCLIApp = do
  cliOpts <- execParser opts
  let config = cliOptsToConfig cliOpts
      env = Env config putStr
      username = cliOptsWallhavenUsername cliOpts
      label = cliOptsCollectionLabel cliOpts
  runReaderT (syncAllWallpapers username label :: ReaderT Env IO ()) env
  where
    opts =
      info
        (cliOptsParser <**> helper)
        ( fullDesc
            <> progDesc "Sync wallpapers from a Wallhaven collection"
            <> header "wallhaven-sync"
        )
