module Wallhaven.API.LogicSpec (spec) where

import Test.Hspec
import Wallhaven.API.Logic (findCollectionByLabel)
import Wallhaven.API.Types

spec :: Spec
spec = do
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
