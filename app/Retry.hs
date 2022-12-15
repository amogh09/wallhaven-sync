module Retry (retryIO) where

import Control.Concurrent (threadDelay)

retryIO :: Int -> Int -> IO (Maybe a) -> IO (Maybe a)
retryIO 1 _ a = a
retryIO !attempts delay a = do
  res <- a
  case res of
    Just err -> threadDelay delay >> retryIO (attempts - 1) delay a
    Nothing -> pure res
