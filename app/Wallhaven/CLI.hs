module Wallhaven.CLI (loadDefaultConfig, runAppWithDefaultConfig) where

import Control.Exception (catchJust)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC8
import System.Environment (getEnv)
import System.IO.Error (isDoesNotExistError)
import Text.Printf (printf)
import Wallhaven.Favorites (Config (Config), Error, downloadAllFavoriteWallpapers)

wallhavenCookieEnvVarName :: String
wallhavenCookieEnvVarName = "WALLHAVEN_COOKIE"

errorWallhavenCookieNotSet :: String
errorWallhavenCookieNotSet = wallhavenCookieEnvVarName <> " cookie not set"

defaultWallpaperDir :: FilePath
defaultWallpaperDir = "/Users/home/stuff/wallpapers"

-- Load cookie from environment variable.
loadCookieFromEnv :: IO (Either Error ByteString)
loadCookieFromEnv =
  catchJust
    doesNotExistErr
    (pure . BC8.pack <$> getEnv wallhavenCookieEnvVarName)
    (const $ pure $ Left errorWallhavenCookieNotSet)
  where
    doesNotExistErr e = if isDoesNotExistError e then Just () else Nothing

-- Load default configuration.
loadDefaultConfig :: IO (Either Error Config)
loadDefaultConfig = fmap (Config defaultWallpaperDir 5 5 3) <$> loadCookieFromEnv

runAppWithDefaultConfig :: IO ()
runAppWithDefaultConfig = do
  configOrErr <- loadDefaultConfig
  case configOrErr of
    Left err -> putStrLn err
    Right config -> do
      errors <- downloadAllFavoriteWallpapers config
      if null errors
        then putStrLn "All wallpapers were synced successfully."
        else do
          printf "Failed to sync %d wallpapers due to errors:" (length errors)
          mapM_ putStrLn errors
