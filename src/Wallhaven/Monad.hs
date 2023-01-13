module Wallhaven.Monad
  ( MonadDeleteWallpaper (..),
    MonadGetDownloadedWallpapers (..),
    MonadInitDB (..),
    HasDebug (..),
    HasDeleteUnliked (..),
    HasLog (..),
    MonadGetCollectionURLs (..),
    MonadGetFullWallpaper (..),
    MonadSaveWallpaper (..),
    MonadWallpaperDB,
    MonadWallhaven,
    MonadDownloadWallpaper (..),
  )
where

import Data.ByteString (ByteString)
import Types

class HasDebug a where
  getDebug :: a -> Bool

class HasDeleteUnliked a where
  getDeleteUnliked :: a -> Bool

class HasLog a where
  getLog :: a -> (String -> IO ())

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
    MonadGetFullWallpaper m
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

class Monad m => MonadGetFullWallpaper m where
  getFullWallpaper :: FullWallpaperURL -> m ByteString

class Monad m => MonadDownloadWallpaper m where
  downloadWallpaper :: FullWallpaperURL -> m ByteString
