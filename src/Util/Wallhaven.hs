module Util.Wallhaven (wallpaperName, unlikedWallpapers) where

import qualified Data.List as List
import Data.List.Split (splitOn)
import qualified Data.Set as Set
import Types

wallpaperName :: WallpaperPath -> WallpaperName
wallpaperName = head . splitOn "." . last . splitOn "/"

wallpaperNameFromLocalWallpaper :: FilePath -> WallpaperName
wallpaperNameFromLocalWallpaper = stripWallhavenPrefix . wallpaperName

stripWallhavenPrefix :: String -> String
stripWallhavenPrefix name =
  if "wallhaven-" `List.isPrefixOf` name then drop 10 name else name

unlikedWallpapers :: [PreviewURL] -> LocalWallpapers -> LocalWallpapers
unlikedWallpapers favs = filter notFavorite
  where
    notFavorite :: WallpaperPath -> Bool
    notFavorite = not . flip Set.member favNames . wallpaperNameFromLocalWallpaper

    favNames :: Set.Set WallpaperName
    favNames = Set.fromList $ map wallpaperName favs
