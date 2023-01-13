module Wallhaven.Env (Env (..), Config (..)) where

import Control.Monad.Reader (ReaderT, asks)
import qualified Database.FileSystem.Action as DBFileSystem
import qualified Network.HTTP.Simple as HTTP
import qualified Retry
import qualified Types
import UnliftIO (MonadIO, MonadUnliftIO)
import qualified Util.HTTP as HTTP
import Util.Time (seconds)
import qualified Wallhaven.API.Action as WallhavenAPI
import Wallhaven.Monad
import Prelude hiding (log)

data Env = Env
  { envConfig :: !Config,
    envLog :: !(String -> IO ())
  }

-- Configuration structure.
data Config = Config
  { -- | The directory where the wallpapers will be saved.
    configWallpaperDir :: FilePath,
    -- | The number of wallpapers to download in parallel.
    configNumParallelDownloads :: Types.NumParallelDownloads,
    -- | Whether to delete unliked wallpapers.
    configDeleteUnliked :: Bool,
    -- | Wallhaven API Key
    configWallhavenAPIKey :: String,
    -- | Debug mode
    configDebug :: Bool
  }

defaultMaxAttempts :: Retry.MaxAttempts
defaultMaxAttempts = 5

defaultDelay :: Retry.RetryDelayMicros
defaultDelay = seconds 3

instance HasDebug Env where
  getDebug = configDebug . envConfig

instance HasDeleteUnliked Env where
  getDeleteUnliked = configDeleteUnliked . envConfig

instance HasLog Env where
  getLog = envLog

instance (MonadUnliftIO m) => MonadGetCollectionURLs (ReaderT Env m) where
  getCollectionURLs username label = do
    apiKey <- asks (configWallhavenAPIKey . envConfig)
    WallhavenAPI.getAllCollectionURLs apiKey username label

instance MonadUnliftIO m => MonadGetFullWallpaper (ReaderT Env m) where
  getFullWallpaper = WallhavenAPI.getFullWallpaper

instance MonadUnliftIO m => MonadDownloadWallpaper (ReaderT Env m) where
  downloadWallpaper =
    HTTP.httpBSWithRetry
      defaultMaxAttempts
      defaultDelay
      HTTP.isTooManyRequestsException
      . HTTP.parseRequest_

instance (MonadIO m) => MonadDeleteWallpaper (ReaderT Env m) where
  deleteWallpaper name = do
    dir <- asks (configWallpaperDir . envConfig)
    DBFileSystem.deleteWallpaper dir name

instance (MonadIO m) => MonadSaveWallpaper (ReaderT Env m) where
  saveWallpaper name wallpaper = do
    dir <- asks (configWallpaperDir . envConfig)
    DBFileSystem.saveWallpaper dir name wallpaper

instance (MonadIO m) => MonadInitDB (ReaderT Env m) where
  initDB =
    asks (configWallpaperDir . envConfig)
      >>= DBFileSystem.createWallpaperDir

instance (MonadIO m) => MonadGetDownloadedWallpapers (ReaderT Env m) where
  getDownloadedWallpapers =
    asks (configWallpaperDir . envConfig)
      >>= DBFileSystem.getWallpaperNames
