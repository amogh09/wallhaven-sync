module Util.Wallhaven.Monad
  ( deleteWallpaper,
    getDownloadedWallpapers,
    CapabilityDeleteWallpaper,
    CapabilityGetDownloadedWallpapers,
  )
where

import Control.Monad.Reader (MonadReader, ReaderT, asks)
import Types
import UnliftIO (MonadIO)
import qualified UnliftIO.Directory as Dir

class CapabilityDeleteWallpaper m where
  deleteWallpaper :: WallpaperName -> m ()

instance (MonadIO m) => CapabilityDeleteWallpaper (ReaderT env m) where
  deleteWallpaper = Dir.removeFile

class CapabilityGetDownloadedWallpapers m where
  getDownloadedWallpapers :: m [WallpaperName]

instance
  (MonadReader env m, HasWallpaperDir env, MonadIO m) =>
  CapabilityGetDownloadedWallpapers (ReaderT env m)
  where
  getDownloadedWallpapers = asks getWallpaperDir >>= Dir.listDirectory
