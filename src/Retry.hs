module Retry
  ( retryM,
    RetryConfig (RetryConfig),
    HasRetryConfig (getRetryConfig),
    CapabilityThreadDelay (threadDelay),
  )
where

import Control.Monad (void)
import Control.Monad.Reader (MonadReader, ReaderT, asks)
import Control.Monad.Writer (WriterT, tell)
import qualified UnliftIO
import qualified UnliftIO.Concurrent as UnliftIO
import Prelude hiding (log)

type MaxAttempts = Int

type RetryDelayMicros = Int

data RetryConfig = RetryConfig
  { maxAttempts :: MaxAttempts,
    retryDelayMicros :: RetryDelayMicros
  }

class HasRetryConfig a where
  getRetryConfig :: a -> RetryConfig

instance HasRetryConfig RetryConfig where
  getRetryConfig = id

retryM ::
  (MonadReader env m, HasRetryConfig env, CapabilityThreadDelay m) =>
  (a -> Bool) ->
  m a ->
  m a
retryM shouldRetry action = do
  config <- asks getRetryConfig
  helper
    (maxAttempts config)
    (retryDelayMicros config)
    shouldRetry
    action

helper ::
  (Monad m, CapabilityThreadDelay m) =>
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

class CapabilityThreadDelay a where
  threadDelay :: Int -> a ()

instance
  (UnliftIO.MonadUnliftIO m) =>
  CapabilityThreadDelay (ReaderT env m)
  where
  threadDelay = UnliftIO.threadDelay

-- instance Monad m => CapabilityThreadDelay (WriterT [String] m) where
--   threadDelay x = void (tell ["delay " <> show x])
