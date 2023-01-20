module Wallhaven.Exception
  ( WallhavenSyncException (..),
  )
where

import Types
import UnliftIO (Exception, Typeable)

type OneLineError = String

type VerboseError = String

data WallhavenSyncException
  = CollectionFetchException Username Label OneLineError VerboseError
  | InitDBException OneLineError VerboseError
  | GetDownloadWallpapersException OneLineError VerboseError
  | DeleteWallpaperException WallpaperName OneLineError VerboseError
  | SaveWallpaperException WallpaperName OneLineError VerboseError
  | WallpaperDownloadException FullWallpaperURL OneLineError VerboseError
  deriving (Typeable, Show)

instance Exception WallhavenSyncException
