module Wallhaven.Monad
  ( MonadDeleteWallpaper (..),
    MonadGetDownloadedWallpapers (..),
    HasDebug (..),
    HasDeleteUnliked (..),
    HasLog (..),
    MonadGetCollectionURLs (..),
    MonadSaveWallpaper (..),
    MonadWallpaperDB,
    MonadWallhaven,
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
    MonadSaveWallpaper m
  )

-- | Monad for interacting with Wallhaven.
type MonadWallhaven m = MonadGetCollectionURLs m

class Monad m => MonadDeleteWallpaper m where
  deleteWallpaper :: WallpaperName -> m ()

class Monad m => MonadGetDownloadedWallpapers m where
  getDownloadedWallpapers :: m [WallpaperName]

class Monad m => MonadSaveWallpaper m where
  saveWallpaper :: WallpaperName -> ByteString -> m ()

class Monad m => MonadGetCollectionURLs m where
  getCollectionURLs :: Username -> Label -> m [FullWallpaperURL]
