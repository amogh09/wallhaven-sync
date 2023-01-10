module Wallhaven.Env (Env (..), Config (..)) where

import Control.Monad.Reader (MonadReader, Reader, ReaderT, asks, liftIO)
import qualified Database.FileSystem.Action as DBFileSystem
import qualified Network.HTTP.Simple as HTTP
import qualified Retry
import qualified Types
import UnliftIO (MonadIO, MonadUnliftIO, hFlush, stdout)
import qualified UnliftIO.Directory as Dir
import Util.HTTP (CapabilityHTTP, httpBS)
import qualified Wallhaven.API.Action as WallhavenAPI
import qualified Wallhaven.API.Class as WallhavenAPI
import Wallhaven.Monad
import qualified Wallhaven.Monad as WallhavenAPI
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
    configRetryConfig :: Retry.RetryConfig,
    -- | Whether to delete unliked wallpapers.
    configDeleteUnliked :: Bool,
    -- | Wallhaven usename
    configUsername :: String,
    -- | Wallhaven API Key
    configWallhavenAPIKey :: String,
    -- | Label of the collection to sync
    configCollectionLabel :: String,
    -- | Debug mode
    configDebug :: Bool
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

instance Retry.HasRetryConfig Env where
  getRetryConfig = configRetryConfig . envConfig

instance MonadIO m => CapabilityHTTP (ReaderT Env m) where
  httpBS = fmap HTTP.getResponseBody . HTTP.httpBS

instance
  (MonadUnliftIO m) =>
  CapabilityGetCollectionURLs (ReaderT Env m)
  where
  getCollectionURLs = WallhavenAPI.getAllCollectionURLs

instance (MonadIO m) => CapabilityDeleteWallpaper (ReaderT Env m) where
  deleteWallpaper = DBFileSystem.deleteWallpaper

instance (MonadIO m) => CapabilitySaveWallpaper (ReaderT Env m) where
  saveWallpaper = DBFileSystem.saveWallpaper

instance
  (MonadIO m) =>
  CapabilityGetDownloadedWallpapers (ReaderT Env m)
  where
  getDownloadedWallpapers = DBFileSystem.getWallpaperNames
