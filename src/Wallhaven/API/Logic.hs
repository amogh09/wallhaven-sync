module Wallhaven.API.Logic
  ( findCollectionByLabel,
    extractWallhavenMetaLastPage,
    wallhavenCollectionPageRequest,
    extractFullWallpaperURLs,
    parseCollectionID,
    wallhavenCollectionsRequest,
  )
where

import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC8
import qualified Data.List as List
import qualified Network.HTTP.Simple as HTTP
import qualified Wallhaven.API.Exception as Exception
import Wallhaven.API.Types
import Wallhaven.Types
  ( CollectionID,
    FullWallpaperURL,
    Label,
    Username,
  )

findCollectionByLabel ::
  String -> WallhavenCollectionsResponse -> Maybe WallhavenCollection
findCollectionByLabel label =
  List.find ((== label) . wallhavenCollectionLabel)
    . wallhavenCollectionsResponseData

-- Parses the given bytestring as Wallhaven meta response and returns
-- the last page field.
extractWallhavenMetaLastPage :: ByteString -> Either String Int
extractWallhavenMetaLastPage =
  fmap wallhavenCollectionWallpapersResponseMetaLastPage
    . Aeson.eitherDecodeStrict

extractFullWallpaperURLs :: ByteString -> Either String [FullWallpaperURL]
extractFullWallpaperURLs =
  fmap
    (fmap wallhavenCollectionWallpaperFullURL . wallhavenCollectionWallpapersResponseData)
    . Aeson.eitherDecodeStrict

-- | Returns an HTTP request for the given page of the given collection.
wallhavenCollectionPageRequest ::
  Username ->
  APIKey ->
  CollectionID ->
  Page ->
  HTTP.Request
wallhavenCollectionPageRequest user apiKey cid page = do
  HTTP.setRequestQueryString
    [ ("apikey", Just $ BC8.pack apiKey),
      ("page", Just . BC8.pack $ show page)
    ]
    . HTTP.parseRequest_
    $ "https://wallhaven.cc/api/v1/collections/" <> user <> "/" <> show cid

-- | Parses Wallhaven collections response and finds the ID of the collection
-- with the provided label.
parseCollectionID ::
  Label -> ByteString -> Either Exception.CollectionURLsFetchException CollectionID
parseCollectionID label jsonBS = do
  collectionsResponse <-
    first
      Exception.UserCollectionsParseException
      (Aeson.eitherDecodeStrict jsonBS)
  maybe
    (Left . Exception.CollectionNotFoundException $ label)
    (pure . wallhavenCollectionID)
    (findCollectionByLabel label collectionsResponse)

wallhavenCollectionsRequest :: Username -> APIKey -> HTTP.Request
wallhavenCollectionsRequest user apiKey =
  HTTP.parseRequest_ $
    "https://wallhaven.cc/api/v1/collections/"
      <> user
      <> "?apikey="
      <> apiKey
