module Util.HTTP (http2XXWithRetry) where

import Control.Monad.Reader (MonadReader)
import Data.ByteString (ByteString)
import Network.HTTP.Client.Conduit
  ( HttpExceptionContent (StatusCodeException),
    responseStatus,
    setRequestCheckStatus,
  )
import Network.HTTP.Simple
  ( HttpException (..),
    Request,
    Response,
    getResponseBody,
    httpBS,
  )
import Network.HTTP.Types (Status, tooManyRequests429)
import Retry (retryM)
import Types
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (throwIO, try)

http2XXWithRetry ::
  (MonadUnliftIO m, MonadReader env m, HasRetryConfig env, HasLog env) =>
  Request ->
  m ByteString
http2XXWithRetry req = do
  res <-
    retryM retryable
      . try
      . httpBS
      . setRequestCheckStatus
      $ req
  case res of
    Left e -> throwIO e
    Right r -> pure $ getResponseBody r
  where
    retryable :: Either HttpException (Response ByteString) -> Bool
    retryable (Left (HttpExceptionRequest _ (StatusCodeException response _))) =
      isRetryableStatus (responseStatus response)
    retryable _ = False

    isRetryableStatus :: Status -> Bool
    isRetryableStatus status = status == tooManyRequests429
