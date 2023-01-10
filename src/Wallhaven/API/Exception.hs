module Wallhaven.API.Exception where

import qualified Network.HTTP.Simple as HTTP
import Types
import UnliftIO.Exception (Exception, Typeable)

type JSONParseError = String

data WallhavenAPIException
  = CollectionsFetchException HTTP.HttpException
  | CollectionsParseException JSONParseError
  | CollectionNotFoundException Label
  | CollectionWallpapersFetchException CollectionID Page HTTP.HttpException
  | WallpapersParseException JSONParseError
  | WallhavenMetaParseException JSONParseError
  deriving (Typeable, Show)

instance Exception WallhavenAPIException
