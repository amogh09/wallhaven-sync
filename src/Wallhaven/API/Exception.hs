module Wallhaven.API.Exception where

import qualified Network.HTTP.Simple as HTTP
import UnliftIO.Exception (Exception, Typeable)
import Wallhaven.API.Types (Page)
import Wallhaven.Types (CollectionID, Label)

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
