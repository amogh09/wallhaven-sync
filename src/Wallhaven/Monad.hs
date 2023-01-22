module Wallhaven.Monad
  ( MonadDeleteWallpaper (..),
    MonadGetDownloadedWallpapers (..),
    MonadInitDB (..),
    HasDebug (..),
    HasDeleteUnliked (..),
    HasLog (..),
    MonadGetCollectionURLs (..),
    MonadSaveWallpaper (..),
    MonadWallpaperDB,
    MonadWallhaven,
    MonadDownloadWallpaper (..),
    HasSyncParallelization (..),
  )
where

import Data.ByteString (ByteString)
import Wallhaven.Types
  ( FullWallpaperURL,
    Label,
    Username,
    Wallpaper,
    WallpaperName,
  )

class HasDebug a where
  getDebug :: a -> Bool

class HasDeleteUnliked a where
  getDeleteUnliked :: a -> Bool

class HasLog a where
  getLog :: a -> (String -> IO ())

class HasSyncParallelization a where
  getSyncParallelization :: a -> Int

-- | Monad for interacting with a database for wallpapers.
type MonadWallpaperDB m =
  ( MonadDeleteWallpaper m,
    MonadGetDownloadedWallpapers m,
    MonadSaveWallpaper m,
    MonadInitDB m
  )

-- | Monad for interacting with Wallhaven.
type MonadWallhaven m =
  ( MonadGetCollectionURLs m,
    MonadDownloadWallpaper m
  )

class Monad m => MonadInitDB m where
  initDB :: m ()

class Monad m => MonadDeleteWallpaper m where
  deleteWallpaper :: WallpaperName -> m ()

class Monad m => MonadGetDownloadedWallpapers m where
  getDownloadedWallpapers :: m [WallpaperName]

class Monad m => MonadSaveWallpaper m where
  saveWallpaper :: WallpaperName -> Wallpaper -> m ()

class Monad m => MonadGetCollectionURLs m where
  getCollectionURLs :: Username -> Label -> m [FullWallpaperURL]

class Monad m => MonadDownloadWallpaper m where
  downloadWallpaper :: FullWallpaperURL -> m ByteString
