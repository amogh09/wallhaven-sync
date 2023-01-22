module Util.HTTP (httpBSWithRetry, isTooManyRequestsException) where

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Either (fromLeft)
import Network.HTTP.Client.Conduit
  ( HttpExceptionContent (StatusCodeException),
    responseStatus,
    setRequestCheckStatus,
  )
import Network.HTTP.Simple
  ( HttpException (..),
    Request,
    getResponseBody,
    httpBS,
  )
import Network.HTTP.Types (tooManyRequests429)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (throwIO, try)
import Util.Retry (MaxAttempts, RetryDelayMicros, retryM)

httpBSWithRetry ::
  (MonadUnliftIO m) =>
  MaxAttempts ->
  -- | Max HTTP attempts
  RetryDelayMicros ->
  -- | Delay between HTTP attempts
  (HttpException -> Bool) ->
  -- | Whether to retry on this exception
  Request ->
  -- } HTTP request
  m ByteString
httpBSWithRetry attempts delay shouldRetry req =
  retryM attempts delay (shouldRetryEither shouldRetry) (tryHttpBS req)
    >>= either throwIO pure

tryHttpBS ::
  (MonadUnliftIO m) => Request -> m (Either HttpException ByteString)
tryHttpBS = try . fmap getResponseBody . httpBS . setRequestCheckStatus

shouldRetryEither :: (HttpException -> Bool) -> Either HttpException a -> Bool
shouldRetryEither shouldRetry = fromLeft False . first shouldRetry

isTooManyRequestsException :: HttpException -> Bool
isTooManyRequestsException
  (HttpExceptionRequest _ (StatusCodeException response _)) =
    (== tooManyRequests429) . responseStatus $ response
isTooManyRequestsException _ = False
