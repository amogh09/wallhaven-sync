module Retry (retryIO, MaxAttempts, RetryDelayMicros) where

import Control.Concurrent (threadDelay)

type MaxAttempts = Int

type RetryDelayMicros = Int

retryIO :: MaxAttempts -> RetryDelayMicros -> (a -> Bool) -> IO a -> IO a
retryIO !attempts _ _ a | attempts <= 1 = a
retryIO !attempts delay shouldRetry a = do
  res <- a
  if shouldRetry res
    then threadDelay delay >> retryIO (attempts - 1) delay shouldRetry a
    else pure res
