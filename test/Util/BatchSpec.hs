module Util.BatchSpec (spec) where

import Test.Hspec
import UnliftIO (evaluate, modifyMVar_, newMVar, readMVar)
import UnliftIO.Concurrent (threadDelay)
import Util.Batch (batchedM)

spec :: Spec
spec = do
  describe "batchedM" $ do
    it "performs the action in batches" $ do
      let size = 5
      resources <- newMVar $ replicate size 0
      let action (x :: Int) = do
            modifyMVar_ resources (evaluate . tail)
            threadDelay 1000
            modifyMVar_ resources (evaluate . (x :))
            pure x
      res <- batchedM size action [1 .. 100]
      res `shouldBe` [1 .. 100]
      resources' <- readMVar resources
      resources' `shouldMatchList` take size [100, 99 .. 1]
