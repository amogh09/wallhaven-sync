module Main where

import Wallhaven.CLI (runAppWithDefaultConfig)
import Prelude hiding (writeFile)

main :: IO ()
main = runAppWithDefaultConfig
