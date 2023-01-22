module Wallhaven.Logic (wallpaperName) where

import qualified Data.List.Split as List
import Types

wallpaperName :: FullWallpaperURL -> WallpaperName
wallpaperName = last . List.splitOn "/"
