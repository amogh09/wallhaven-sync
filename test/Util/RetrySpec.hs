module Util.RetrySpec (spec) where

import Control.Monad.State (get, liftIO, modify, runStateT)
import Retry
import Test.Hspec

spec :: Spec
spec = do
  describe "retryM" $ do
    it "doesn't retry if the action succeeds" $ do
      let action = modify (+ 1) >> pure "okay"
      (res, attempts) <-
        liftIO $
          runStateT (retryM 3 0 (const False) action) (0 :: Int)
      res `shouldBe` ("okay" :: String)
      attempts `shouldBe` 1

    it "exhausts all attempts if the action fails each time" $ do
      let action = modify (+ 1) >> pure "bad"
      (res, attempts) <-
        liftIO $
          runStateT (retryM 3 0 (const True) action) (0 :: Int)
      res `shouldBe` ("bad" :: String)
      attempts `shouldBe` 3

    it "stops retrying when the action succeeds" $ do
      let action = modify (+ 1) >> get
      (res, attempts) <-
        liftIO $
          runStateT (retryM 5 0 (< 3) action) (0 :: Int)
      res `shouldBe` (3 :: Int)
      attempts `shouldBe` 3
