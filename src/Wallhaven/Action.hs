module Wallhaven.Action (deleteUnlikedWallpapers) where

import Control.Monad (unless)
import Types (FullWallpaperURL, WallpaperName)
import qualified Wallhaven.Logic as Logic
import Wallhaven.Monad
  ( CapabilityDeleteWallpaper,
    CapabilityGetDownloadedWallpapers,
    deleteWallpaper,
    getDownloadedWallpapers,
  )

-- Deletes the local wallpapers that are not in the favorites anymore.
-- Returns the wallpapers that were deleted.
deleteUnlikedWallpapers ::
  ( Monad m,
    CapabilityGetDownloadedWallpapers m,
    CapabilityDeleteWallpaper m
  ) =>
  [FullWallpaperURL] ->
  m [WallpaperName]
deleteUnlikedWallpapers favURLs = do
  unliked <- Logic.unlikedWallpapers favURLs <$> getDownloadedWallpapers
  unless (null unliked) (mapM_ deleteWallpaper unliked)
  return unliked
