module Types.WallhavenAPI
  ( WallhavenCollectionsResponse (..),
    WallhavenCollection (..),
    WallhavenCollectionWallpaper (..),
    WallhavenCollectionWallpapersResponse (..),
    WallhavenCollectionWallpapersResponseMeta (..),
    extractCollectionIDFromCollectionsResponse,
    extractFullWallpaperURLs,
    extractWallhavenMetaLastPage,
    APIExtractError (..),
    findCollectionByLabel,
  )
where

import Data.Aeson
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.List as List
import Types

data WallhavenCollection = WallhavenCollection
  { wallhavenCollectionID :: CollectionID,
    wallhavenCollectionLabel :: Label
  }
  deriving (Show, Eq)

instance FromJSON WallhavenCollection where
  parseJSON = withObject "WallhavenCollection" $ \o -> do
    WallhavenCollection <$> o .: "id" <*> o .: "label"

newtype WallhavenCollectionsResponse = WallhavenCollectionsResponse
  { wallhavenCollectionsResponseData :: [WallhavenCollection]
  }
  deriving (Show, Eq)

instance FromJSON WallhavenCollectionsResponse where
  parseJSON = withObject "WallhavenCollectionsResponse" $ \o -> do
    WallhavenCollectionsResponse <$> o .: "data"

data WallhavenCollectionWallpaper = WallhavenCollectionWallpaper
  { wallhavenCollectionWallpaperID :: String,
    wallhavenCollectionWallpaperFullURL :: FullWallpaperURL
  }
  deriving (Show, Eq)

instance FromJSON WallhavenCollectionWallpaper where
  parseJSON = withObject "WallhavenCollectionWallpaper" $ \o -> do
    WallhavenCollectionWallpaper <$> o .: "id" <*> o .: "path"

newtype WallhavenCollectionWallpapersResponse = WallhavenCollectionWallpapersResponse
  { wallhavenCollectionWallpapersResponseData :: [WallhavenCollectionWallpaper]
  }
  deriving (Show, Eq)

instance FromJSON WallhavenCollectionWallpapersResponse where
  parseJSON = withObject "WallhavenCollectionWallpapersResponse" $ \o -> do
    WallhavenCollectionWallpapersResponse <$> o .: "data"

data WallhavenCollectionWallpapersResponseMeta = WallhavenCollectionWallpapersResponseMeta
  { wallhavenCollectionWallpapersResponseMetaCurrentPage :: Int,
    wallhavenCollectionWallpapersResponseMetaLastPage :: Int
  }
  deriving (Show, Eq)

instance FromJSON WallhavenCollectionWallpapersResponseMeta where
  parseJSON = withObject "WallhavenCollectionWallpapersResponseMeta" $ \o -> do
    data' <- o .: "meta"
    WallhavenCollectionWallpapersResponseMeta
      <$> data'
      .: "current_page"
      <*> data'
      .: "last_page"

findCollectionByLabel ::
  String -> WallhavenCollectionsResponse -> Maybe WallhavenCollection
findCollectionByLabel label =
  List.find ((== label) . wallhavenCollectionLabel) . wallhavenCollectionsResponseData

data APIExtractError
  = APIParseError {apiParseErrorType :: String, apiParseErrorInternal :: String}
  | APIExtractError {apiExtractErrorType :: String, apiExtractErrorField :: String}
  deriving (Show, Eq)

-- Parses the given bytestring as a Wallpaper Collection Response
-- and returns the ID of the collection with the given label.
extractCollectionIDFromCollectionsResponse ::
  Label ->
  ByteString ->
  Either APIExtractError CollectionID
extractCollectionIDFromCollectionsResponse label jsonBS = do
  collectionsResponse <-
    first
      (APIParseError "Collections")
      (eitherDecodeStrict jsonBS)
  maybe
    (Left . APIExtractError "Collections" $ "label '" <> label <> "'")
    (pure . wallhavenCollectionID)
    (findCollectionByLabel label collectionsResponse)

-- Parses the given bytestring as Wallhaven meta response and returns
-- the last page field.
extractWallhavenMetaLastPage :: ByteString -> Either APIExtractError Int
extractWallhavenMetaLastPage =
  fmap wallhavenCollectionWallpapersResponseMetaLastPage
    . first (APIParseError "Wallpapers Meta")
    . eitherDecodeStrict

extractFullWallpaperURLs :: ByteString -> Either APIExtractError [FullWallpaperURL]
extractFullWallpaperURLs =
  fmap
    (fmap wallhavenCollectionWallpaperFullURL . wallhavenCollectionWallpapersResponseData)
    . first (APIParseError "wallpapers")
    . eitherDecodeStrict
