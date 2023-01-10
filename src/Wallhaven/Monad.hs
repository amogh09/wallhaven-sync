module Wallhaven.Monad
  ( CapabilityDeleteWallpaper (..),
    HasWallpaperDir (..),
    CapabilityGetDownloadedWallpapers (..),
    HasDebug (..),
    HasDeleteUnliked (..),
    HasLog (..),
    CapabilityGetCollectionURLs (..),
  )
where

import Types

class HasDebug a where
  getDebug :: a -> Bool

class HasDeleteUnliked a where
  getDeleteUnliked :: a -> Bool

class HasLog a where
  getLog :: a -> (String -> IO ())

class HasWallpaperDir a where
  getWallpaperDir :: a -> FilePath

class CapabilityDeleteWallpaper m where
  deleteWallpaper :: WallpaperName -> m ()

class CapabilityGetDownloadedWallpapers m where
  getDownloadedWallpapers :: m [WallpaperName]

class CapabilityGetCollectionURLs m where
  getCollectionURLs :: Username -> Label -> m [FullWallpaperURL]
