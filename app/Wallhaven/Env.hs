module Wallhaven.Env (Env (..), Config (..)) where

import Control.Monad.Reader (ReaderT, asks)
import UnliftIO (MonadUnliftIO)
import qualified Wallhaven.Implementation.API as API
import qualified Wallhaven.Implementation.FileSystemDB as FileSystemDB
import Wallhaven.Monad
  ( HasDebug (..),
    HasDeleteUnliked (..),
    HasLog (..),
    HasSyncParallelization (..),
    MonadDeleteWallpaper (..),
    MonadDownloadWallpaper (..),
    MonadGetCollectionURLs (..),
    MonadGetDownloadedWallpapers (..),
    MonadInitDB (..),
    MonadSaveWallpaper (..),
  )
import qualified Wallhaven.Types as Types
import Prelude hiding (log)

-- Environment record
data Env = Env
  { envConfig :: !Config,
    envLog :: !(String -> IO ())
  }

-- Configuration record
data Config = Config
  { -- | The directory where the wallpapers will be saved.
    configWallpaperDir :: !FilePath,
    -- | The number of wallpapers to download in parallel.
    configNumParallelDownloads :: !Types.NumParallelDownloads,
    -- | Whether to delete unliked wallpapers.
    configDeleteUnliked :: !Bool,
    -- | Wallhaven API Key
    configWallhavenAPIKey :: !String,
    -- | Debug mode
    configDebug :: !Bool
  }

instance HasDebug Env where
  getDebug = configDebug . envConfig

instance HasDeleteUnliked Env where
  getDeleteUnliked = configDeleteUnliked . envConfig

instance HasLog Env where
  getLog = envLog

instance HasSyncParallelization Env where
  getSyncParallelization = configNumParallelDownloads . envConfig

instance (MonadUnliftIO m) => MonadGetCollectionURLs (ReaderT Env m) where
  getCollectionURLs username label = do
    apiKey <- asks (configWallhavenAPIKey . envConfig)
    API.getCollectionURLs apiKey username label

instance MonadUnliftIO m => MonadDownloadWallpaper (ReaderT Env m) where
  downloadWallpaper = API.downloadWallpaper

instance (MonadUnliftIO m) => MonadDeleteWallpaper (ReaderT Env m) where
  deleteWallpaper name =
    asks (configWallpaperDir . envConfig)
      >>= flip FileSystemDB.deleteWallpaper name

instance (MonadUnliftIO m) => MonadSaveWallpaper (ReaderT Env m) where
  saveWallpaper name wallpaper = do
    dir <- asks (configWallpaperDir . envConfig)
    FileSystemDB.saveWallpaper dir name wallpaper

instance (MonadUnliftIO m) => MonadInitDB (ReaderT Env m) where
  initDB = asks (configWallpaperDir . envConfig) >>= FileSystemDB.initDB

instance (MonadUnliftIO m) => MonadGetDownloadedWallpapers (ReaderT Env m) where
  getDownloadedWallpapers =
    asks (configWallpaperDir . envConfig) >>= FileSystemDB.getDownloadedWallpapers
