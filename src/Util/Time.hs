module Util.Time (seconds, timed, timed_) where

import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import UnliftIO (MonadIO (liftIO), MonadUnliftIO)

seconds :: Int -> Int
seconds n = n * 10 ^ (6 :: Int)

timed :: (MonadUnliftIO m) => m a -> m (a, NominalDiffTime)
timed action = do
  start <- liftIO getCurrentTime
  result <- action
  end <- liftIO getCurrentTime
  return (result, diffUTCTime end start)

timed_ :: (MonadUnliftIO m) => m a -> m NominalDiffTime
timed_ action = do (_, time) <- timed action; return time
