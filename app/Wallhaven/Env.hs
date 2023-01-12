module Wallhaven.Env (Env (..), Config (..), RetryConfig (..)) where

import Control.Monad.Reader (ReaderT)
import qualified Database.FileSystem.Action as DBFileSystem
import qualified Network.HTTP.Simple as HTTP
import Retry (MaxAttempts, RetryDelayMicros)
import qualified Types
import UnliftIO (MonadIO, MonadUnliftIO)
import Util.HTTP (CapabilityHTTP, httpBS)
import qualified Wallhaven.API.Action as WallhavenAPI
import qualified Wallhaven.API.Class as WallhavenAPI
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
    -- | Retry config for HTTP requests.
    configRetryConfig :: RetryConfig,
    -- | Whether to delete unliked wallpapers.
    configDeleteUnliked :: Bool,
    -- | Wallhaven API Key
    configWallhavenAPIKey :: String,
    -- | Debug mode
    configDebug :: Bool
  }

data RetryConfig = RetryConfig
  { maxAttempts :: MaxAttempts,
    retryDelayMicros :: RetryDelayMicros
  }

instance HasDebug Env where
  getDebug = configDebug . envConfig

instance WallhavenAPI.HasWallhavenAPIKey Env where
  getWallhavenAPIKey = configWallhavenAPIKey . envConfig

instance HasDeleteUnliked Env where
  getDeleteUnliked = configDeleteUnliked . envConfig

instance HasLog Env where
  getLog = envLog

instance WallhavenAPI.HasNumParallelDownloads Env where
  getNumParallelDownloads = configNumParallelDownloads . envConfig

instance DBFileSystem.HasWallpaperDir Env where
  getWallpaperDir = configWallpaperDir . envConfig

instance MonadIO m => CapabilityHTTP (ReaderT Env m) where
  httpBS = fmap HTTP.getResponseBody . HTTP.httpBS

instance (MonadUnliftIO m) => MonadGetCollectionURLs (ReaderT Env m) where
  getCollectionURLs = WallhavenAPI.getAllCollectionURLs

instance MonadUnliftIO m => MonadGetFullWallpaper (ReaderT Env m) where
  getFullWallpaper = WallhavenAPI.getFullWallpaper

instance (MonadIO m) => MonadDeleteWallpaper (ReaderT Env m) where
  deleteWallpaper = DBFileSystem.deleteWallpaper

instance (MonadIO m) => MonadSaveWallpaper (ReaderT Env m) where
  saveWallpaper = DBFileSystem.saveWallpaper

instance
  (MonadIO m) =>
  MonadGetDownloadedWallpapers (ReaderT Env m)
  where
  getDownloadedWallpapers = DBFileSystem.getWallpaperNames
