module Wallhaven.Logic (wallpaperName) where

import qualified Data.List.Split as List
import Wallhaven.Types (FullWallpaperURL, WallpaperName)

wallpaperName :: FullWallpaperURL -> WallpaperName
wallpaperName = last . List.splitOn "/"
