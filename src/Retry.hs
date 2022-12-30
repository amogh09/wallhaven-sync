module Retry
  ( retryM,
  )
where

import Control.Monad.Reader (MonadReader, asks)
import Types
import UnliftIO (MonadIO)
import UnliftIO.Concurrent (threadDelay)
import Prelude hiding (log)

retryM ::
  (MonadIO m, MonadReader env m, HasRetryConfig env) =>
  (a -> Bool) ->
  m a ->
  m a
retryM shouldRetry action = do
  config <- asks getRetryConfig
  helper (maxAttempts config) (retryDelayMicros config) shouldRetry action

helper ::
  (MonadIO m, MonadReader env m) =>
  MaxAttempts ->
  RetryDelayMicros ->
  (a -> Bool) ->
  m a ->
  m a
helper !attempts _ _ a | attempts <= 1 = a
helper !attempts delay shouldRetry a = do
  res <- a
  if shouldRetry res
    then threadDelay delay >> helper (attempts - 1) delay shouldRetry a
    else pure res