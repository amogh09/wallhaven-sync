module Util.HTTP
  ( httpBSWithRetry,
    isTooManyRequestsException,
    CapabilityHTTP,
    httpBS,
  )
where

import Control.Monad.Reader (MonadReader, ReaderT)
import Data.ByteString (ByteString)
import Network.HTTP.Client.Conduit
  ( HttpExceptionContent (StatusCodeException),
    responseStatus,
    setRequestCheckStatus,
  )
import Network.HTTP.Simple
  ( HttpException (..),
    Request,
  )
import qualified Network.HTTP.Simple as HTTP
import Network.HTTP.Types (tooManyRequests429)
import qualified Retry
import UnliftIO (MonadIO, MonadUnliftIO)
import UnliftIO.Exception (throwIO, try)

class CapabilityHTTP m where
  httpBS :: Request -> m ByteString

instance (MonadIO m) => CapabilityHTTP (ReaderT env m) where
  httpBS = fmap HTTP.getResponseBody . HTTP.httpBS

httpBSWithRetry ::
  ( CapabilityHTTP m,
    Retry.HasRetryConfig env,
    Retry.CapabilityThreadDelay m,
    MonadUnliftIO m,
    MonadReader env m
  ) =>
  (HttpException -> Bool) ->
  Request ->
  m ByteString
httpBSWithRetry shouldRetry req = do
  res <-
    Retry.retryM retryable
      . try
      . httpBS
      . setRequestCheckStatus
      $ req
  case res of
    Left e -> throwIO e
    Right r -> pure r
  where
    retryable (Left e) = shouldRetry e
    retryable _ = False

isTooManyRequestsException :: HttpException -> Bool
isTooManyRequestsException
  (HttpExceptionRequest _ (StatusCodeException response _)) =
    (== tooManyRequests429) . responseStatus $ response
isTooManyRequestsException _ = False
