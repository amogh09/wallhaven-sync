module Wallhaven.API.Class where

import Types (NumParallelDownloads)
import Wallhaven.API.Types (APIKey)

class HasWallhavenAPIKey a where
  getWallhavenAPIKey :: a -> APIKey

class HasNumParallelDownloads a where
  getNumParallelDownloads :: a -> NumParallelDownloads
