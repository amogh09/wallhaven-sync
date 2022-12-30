module Types.WallhavenAPISpec (spec) where

import qualified Data.Aeson as Aeson
import Test.Hspec
import Types.WallhavenAPI

spec :: Spec
spec = do
  describe "Wallhaven API" $ do
    it "can parse a collection" $ do
      let json = "{\"id\": 1, \"label\": \"test\"}"
      let expected = WallhavenCollection 1 "test"
      Aeson.decode json `shouldBe` Just expected
    it "can parse a collections response" $ do
      let json = "{\"data\": [{\"id\": 1, \"label\": \"test\"}]}"
      let expected = WallhavenCollectionsResponse [WallhavenCollection 1 "test"]
      Aeson.decode json `shouldBe` Just expected
