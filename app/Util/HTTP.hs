module Util.HTTP (getURL, httpBSWithRetryAndErrorHandling) where

import Control.Exception (tryJust)
import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Network.HTTP.Client.Conduit (HttpExceptionContent (StatusCodeException), responseStatus)
import Network.HTTP.Simple
  ( HttpException (..),
    Request,
    Response,
    getResponseBody,
    httpBS,
  )
import Network.HTTP.Types (Status, tooManyRequests429)
import Retry (MaxAttempts, RetryDelayMicros, retryIO)

getURL :: Request -> IO ByteString
getURL url = getResponseBody <$> httpBS url

-- Performs an HTTP request with retries and catches HTTP exceptions
-- and turns them into errors.
httpBSWithRetryAndErrorHandling ::
  (MonadIO m, MonadError HttpException m) =>
  MaxAttempts ->
  RetryDelayMicros ->
  Request ->
  m ByteString
httpBSWithRetryAndErrorHandling numRetries delay req =
  liftIO (httpBSWithRetry numRetries delay req)
    >>= fmap getResponseBody . liftEither

httpBSWithRetry ::
  MaxAttempts ->
  RetryDelayMicros ->
  Request ->
  IO (Either HttpException (Response ByteString))
httpBSWithRetry attempts delay =
  retryIO attempts delay retryable
    . tryJust (Just @HttpException)
    . httpBS
  where
    retryable :: Either HttpException (Response ByteString) -> Bool
    retryable (Left (HttpExceptionRequest _ (StatusCodeException response _))) =
      isRetryableStatus (responseStatus response)
    retryable (Right response) = isRetryableStatus (responseStatus response)
    retryable _ = False

    isRetryableStatus :: Status -> Bool
    isRetryableStatus status = status == tooManyRequests429
