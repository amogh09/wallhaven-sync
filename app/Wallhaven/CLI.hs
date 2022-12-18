module Wallhaven.CLI (runCLIApp) where

import Control.Exception (catchJust)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC8
import Options.Applicative
import System.Environment (getEnv)
import System.IO.Error (isDoesNotExistError)
import Text.Printf (printf)
import Wallhaven.Favorites (Config (Config, configWallpaperDir), Error, downloadAllFavoriteWallpapers)

wallhavenCookieEnvVarName :: String
wallhavenCookieEnvVarName = "WALLHAVEN_COOKIE"

errorWallhavenCookieNotSet :: String
errorWallhavenCookieNotSet = wallhavenCookieEnvVarName <> " cookie not set"

defaultWallpaperDir :: FilePath
defaultWallpaperDir = "/Users/home/wallpapers"

-- Load cookie from environment variable.
loadCookieFromEnv :: IO (Either Error ByteString)
loadCookieFromEnv =
  catchJust
    doesNotExistErr
    (pure . BC8.pack <$> getEnv wallhavenCookieEnvVarName)
    (const $ pure $ Left errorWallhavenCookieNotSet)
  where
    doesNotExistErr e = if isDoesNotExistError e then Just () else Nothing

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
    (cliOptsNumRetries opts)
    (cliOptsRetryDelay opts)

runCLIApp :: IO ()
runCLIApp = do
  cliOpts <- execParser opts
  cookie <- loadCookieFromEnv
  let configOrErr = cliOptsToConfig cliOpts <$> cookie
  case configOrErr of
    Left err -> putStrLn err
    Right config -> do
      errors <- downloadAllFavoriteWallpapers config
      if null errors
        then
          printf
            "All wallpapers were synced successfully to %s\n"
            (configWallpaperDir config)
        else do
          putStrLn "Failed to sync wallpapers due to errors:"
          mapM_ putStrLn errors
  where
    opts =
      info
        (cliOptsParser <**> helper)
        ( fullDesc
            <> progDesc "Sync wallpapers from Wallhaven favorites"
            <> header "wallhaven-sync"
        )
