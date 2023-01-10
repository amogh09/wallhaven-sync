module Wallhaven.Monad
  ( CapabilityDeleteWallpaper (..),
    CapabilityGetDownloadedWallpapers (..),
    HasDebug (..),
    HasDeleteUnliked (..),
    HasLog (..),
    CapabilityGetCollectionURLs (..),
    CapabilitySaveWallpaper (..),
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

class CapabilityDeleteWallpaper m where
  deleteWallpaper :: WallpaperName -> m ()

class CapabilityGetDownloadedWallpapers m where
  getDownloadedWallpapers :: m [WallpaperName]

class CapabilitySaveWallpaper m where
  saveWallpaper :: WallpaperName -> ByteString -> m ()

class CapabilityGetCollectionURLs m where
  getCollectionURLs :: Username -> Label -> m [FullWallpaperURL]
