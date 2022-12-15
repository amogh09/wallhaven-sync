module Main where

import Wallhaven.Favorites (downloadAllFavoriteWallpapers)
import Prelude hiding (writeFile)

main :: IO ()
main = downloadAllFavoriteWallpapers
