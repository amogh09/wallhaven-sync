module Retry (retryIO) where

import Control.Concurrent (threadDelay)

retryIO :: Int -> Int -> (a -> Bool) -> IO a -> IO a
retryIO 1 _ _ a = a
retryIO !attempts delay shouldRetry a = do
  res <- a
  if shouldRetry res
    then threadDelay delay >> retryIO (attempts - 1) delay shouldRetry a
    else pure res
