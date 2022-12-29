module Util.Wallhaven () where

import Data.List.Split (splitOn)
import Types

wallpaperName :: String -> Maybe WallpaperName
wallpaperName path = safeLast (splitOn "/" path) >>= safeHead . splitOn "."

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just $ last xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just $ head xs
