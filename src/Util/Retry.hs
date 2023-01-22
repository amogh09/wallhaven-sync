module Util.Retry (retryM, MaxAttempts, RetryDelayMicros) where

import UnliftIO (MonadIO)
import UnliftIO.Concurrent (threadDelay)
import Prelude hiding (log)

type MaxAttempts = Int

type RetryDelayMicros = Int

retryM ::
  MonadIO m =>
  MaxAttempts ->
  -- | Maximum number of attempts
  RetryDelayMicros ->
  -- | Delay between attempts in microseconds
  (a -> Bool) ->
  -- | Predicate to check if the action should be retried
  m a ->
  -- | Action
  m a
retryM !attempts _ _ a | attempts <= 1 = a
retryM !attempts delay shouldRetry a = do
  res <- a
  if shouldRetry res
    then threadDelay delay >> retryM (attempts - 1) delay shouldRetry a
    else pure res
