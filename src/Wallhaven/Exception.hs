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
  deriving (Typeable, Show)

instance Exception WallhavenSyncException
