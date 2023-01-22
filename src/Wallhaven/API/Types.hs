module Wallhaven.API.Types where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Wallhaven.Types (CollectionID, FullWallpaperURL, Label)

type APIKey = String

type Page = Int

data WallhavenCollection = WallhavenCollection
  { wallhavenCollectionID :: CollectionID,
    wallhavenCollectionLabel :: Label
  }
  deriving (Show, Eq)

data WallhavenCollectionWallpaper = WallhavenCollectionWallpaper
  { wallhavenCollectionWallpaperID :: String,
    wallhavenCollectionWallpaperFullURL :: FullWallpaperURL
  }
  deriving (Show, Eq)

newtype WallhavenCollectionsResponse = WallhavenCollectionsResponse
  { wallhavenCollectionsResponseData :: [WallhavenCollection]
  }
  deriving (Show, Eq)

newtype WallhavenCollectionWallpapersResponse = WallhavenCollectionWallpapersResponse
  { wallhavenCollectionWallpapersResponseData :: [WallhavenCollectionWallpaper]
  }
  deriving (Show, Eq)

data WallhavenCollectionWallpapersResponseMeta = WallhavenCollectionWallpapersResponseMeta
  { wallhavenCollectionWallpapersResponseMetaCurrentPage :: Int,
    wallhavenCollectionWallpapersResponseMetaLastPage :: Int
  }
  deriving (Show, Eq)

instance FromJSON WallhavenCollection where
  parseJSON = withObject "WallhavenCollection" $ \o -> do
    WallhavenCollection <$> o .: "id" <*> o .: "label"

instance FromJSON WallhavenCollectionsResponse where
  parseJSON = withObject "WallhavenCollectionsResponse" $ \o -> do
    WallhavenCollectionsResponse <$> o .: "data"

instance FromJSON WallhavenCollectionWallpaper where
  parseJSON = withObject "WallhavenCollectionWallpaper" $ \o -> do
    WallhavenCollectionWallpaper <$> o .: "id" <*> o .: "path"

instance FromJSON WallhavenCollectionWallpapersResponse where
  parseJSON = withObject "WallhavenCollectionWallpapersResponse" $ \o -> do
    WallhavenCollectionWallpapersResponse <$> o .: "data"

instance FromJSON WallhavenCollectionWallpapersResponseMeta where
  parseJSON = withObject "WallhavenCollectionWallpapersResponseMeta" $ \o -> do
    data' <- o .: "meta"
    WallhavenCollectionWallpapersResponseMeta
      <$> data'
      .: "current_page"
      <*> data'
      .: "last_page"
