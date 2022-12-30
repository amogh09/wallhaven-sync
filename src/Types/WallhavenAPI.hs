module Types.WallhavenAPI
  ( WallhavenCollectionsResponse (..),
    WallhavenCollection (..),
    WallhavenCollectionWallpaper (..),
    WallhavenCollectionWallpapersResponse (..),
    findCollectionByLabel,
  )
where

import Data.Aeson
import qualified Data.List as List
import Types

data WallhavenCollection = WallhavenCollection
  { wallhavenCollectionID :: Int,
    wallhavenCollectionLabel :: String
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
    wallhavenCollectionWallpaperFullURL :: URL
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

findCollectionByLabel ::
  String -> WallhavenCollectionsResponse -> Maybe WallhavenCollection
findCollectionByLabel label =
  List.find ((== label) . wallhavenCollectionLabel) . wallhavenCollectionsResponseData
