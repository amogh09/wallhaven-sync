module Types.WallhavenAPISpec (spec) where

import qualified Data.Aeson as Aeson
import Data.Either (isLeft)
import Test.Hspec
import Types.WallhavenAPI

spec :: Spec
spec = do
  describe "Wallhaven API" $ do
    describe "JSON decoding" $ do
      it "can parse a collection" $ do
        let json = "{\"id\": 1, \"label\": \"test\", \"unknown\": \"value\"}"
            expected = WallhavenCollection 1 "test"
        Aeson.decode json `shouldBe` Just expected
      it "can parse a collections response" $ do
        let json = "{\"data\": [{\"id\": 1, \"label\": \"test\"}]}"
            expected = WallhavenCollectionsResponse [WallhavenCollection 1 "test"]
        Aeson.decode json `shouldBe` Just expected
      it "can parse a collection wallpaper" $ do
        let json = "{\"id\": \"1\", \"path\": \"https://wallhaven.cc/w/1\"}"
            expected = WallhavenCollectionWallpaper "1" "https://wallhaven.cc/w/1"
        Aeson.decode json `shouldBe` Just expected
      it "can parse a collection wallpapers response" $ do
        let json = "{\"data\": [{\"id\": \"1\", \"path\": \"https://wallhaven.cc/w/1\"}]}"
            expected =
              WallhavenCollectionWallpapersResponse
                [WallhavenCollectionWallpaper "1" "https://wallhaven.cc/w/1"]
        Aeson.decode json `shouldBe` Just expected
      it "fails to parse a collection with missing label" $ do
        let json = "{\"id\": 1}"
        shouldSatisfy
          (Aeson.eitherDecode json :: Either String WallhavenCollection)
          isLeft
      it "can parse collection wallpapers response meta" $ do
        let json = "{\"meta\": {\"current_page\": 1, \"last_page\": 2}}"
            expected = WallhavenCollectionWallpapersResponseMeta 1 2
        Aeson.decode json `shouldBe` Just expected
    describe "findCollectionByLabel" $ do
      it "can find a collection by label" $ do
        let response =
              WallhavenCollectionsResponse
                [ WallhavenCollection 1 "test",
                  WallhavenCollection 2 "test2"
                ]
        findCollectionByLabel "test" response
          `shouldBe` Just (WallhavenCollection 1 "test")
      it "returns Nothing if the collection is not found" $ do
        let response =
              WallhavenCollectionsResponse
                [ WallhavenCollection 1 "test",
                  WallhavenCollection 2 "test2"
                ]
        findCollectionByLabel "test3" response `shouldBe` Nothing
