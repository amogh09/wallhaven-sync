module Wallhaven.Logic
  ( wallpaperName,
    unlikedWallpapers,
    wallhavenCollectionsRequest,
    wallhavenCollectionPageRequest,
    parseCollectionID,
  )
where

import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC8
import qualified Data.List.Split as List
import qualified Data.Set as Set
import qualified Network.HTTP.Simple as HTTP
import Types
import qualified Types.WallhavenAPI as API
import qualified Wallhaven.Exception as Exception

wallpaperName :: WallpaperPath -> WallpaperName
wallpaperName = last . List.splitOn "/"

unlikedWallpapers :: [FullWallpaperURL] -> LocalWallpapers -> LocalWallpapers
unlikedWallpapers favs = filter (not . (`Set.member` favsSet) . wallpaperName)
  where
    favsSet = Set.fromList . fmap wallpaperName $ favs

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
  Label -> ByteString -> Either Exception.WallpaperSyncException CollectionID
parseCollectionID label jsonBS = do
  collectionsResponse <-
    first
      Exception.CollectionsParseException
      (Aeson.eitherDecodeStrict jsonBS)
  maybe
    (Left . Exception.CollectionNotFoundException $ label)
    (pure . API.wallhavenCollectionID)
    (API.findCollectionByLabel label collectionsResponse)

wallhavenCollectionsRequest :: Username -> APIKey -> HTTP.Request
wallhavenCollectionsRequest user apiKey =
  HTTP.parseRequest_ $
    "https://wallhaven.cc/api/v1/collections/"
      <> user
      <> "?apikey="
      <> apiKey
