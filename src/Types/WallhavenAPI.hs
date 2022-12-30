module Types.WallhavenAPI where

import Data.Aeson

data WallhavenCollection = WallhavenCollection
  { wallhavenCollectionID :: Int,
    wallhavenCollectionLabel :: String
  }
  deriving (Show, Eq)

newtype WallhavenCollectionsResponse = WallhavenCollectionsResponse
  { wallhavenCollectionsResponseData :: [WallhavenCollection]
  }
  deriving (Show, Eq)

instance FromJSON WallhavenCollection where
  parseJSON = withObject "WallhavenCollection" $ \o -> do
    WallhavenCollection <$> o .: "id" <*> o .: "label"

instance FromJSON WallhavenCollectionsResponse where
  parseJSON = withObject "WallhavenCollectionsResponse" $ \o -> do
    WallhavenCollectionsResponse <$> o .: "data"
