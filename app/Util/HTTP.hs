module Util.HTTP (http2XXWithRetry) where

import Control.Exception.Safe (MonadCatch, MonadThrow, throwM, try)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.ByteString (ByteString)
import Network.HTTP.Client.Conduit (HttpExceptionContent (StatusCodeException), responseStatus, setRequestCheckStatus)
import Network.HTTP.Simple
  ( HttpException (..),
    Request,
    Response,
    getResponseBody,
    httpBS,
  )
import Network.HTTP.Types (Status, tooManyRequests429)
import Retry (HasRetryConfig, retryIO)

http2XXWithRetry ::
  (MonadThrow m, MonadIO m, MonadReader env m, HasRetryConfig env, MonadCatch m) =>
  Request ->
  m ByteString
http2XXWithRetry req = do
  res <-
    retryIO retryable
      . try
      . httpBS
      . setRequestCheckStatus
      $ req
  case res of
    Left e -> throwM e
    Right r -> pure $ getResponseBody r
  where
    retryable :: Either HttpException (Response ByteString) -> Bool
    retryable (Left (HttpExceptionRequest _ (StatusCodeException response _))) =
      isRetryableStatus (responseStatus response)
    retryable _ = False

    isRetryableStatus :: Status -> Bool
    isRetryableStatus status = status == tooManyRequests429
