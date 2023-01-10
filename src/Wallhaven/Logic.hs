module Wallhaven.Logic
  ( wallpaperName,
    unlikedWallpapers,
  )
where

import qualified Data.List.Split as List
import qualified Data.Set as Set
import Types

wallpaperName :: WallpaperPath -> WallpaperName
wallpaperName = last . List.splitOn "/"

unlikedWallpapers :: [FullWallpaperURL] -> LocalWallpapers -> LocalWallpapers
unlikedWallpapers favs = filter (not . (`Set.member` favsSet) . wallpaperName)
  where
    favsSet = Set.fromList . fmap wallpaperName $ favs
