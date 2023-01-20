module Wallhaven.API.Exception where

import qualified Network.HTTP.Simple as HTTP
import Types
import UnliftIO.Exception (Exception, Typeable)

type JSONParseError = String

data CollectionURLsFetchException
  = UserCollectionsHTTPException HTTP.HttpException
  | UserCollectionsParseException JSONParseError
  | CollectionNotFoundException Label
  | CollectionFetchHTTPException CollectionID Page HTTP.HttpException
  | MetaParseException CollectionID JSONParseError
  | WallpaperURLsParseException JSONParseError
  deriving (Typeable, Show)

instance Exception CollectionURLsFetchException
