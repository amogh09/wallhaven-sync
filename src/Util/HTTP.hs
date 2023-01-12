module Util.HTTP
  ( httpBSWithRetry,
    isTooManyRequestsException,
    CapabilityHTTP,
    httpBS,
  )
where

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Either (fromLeft)
import Network.HTTP.Client.Conduit
  ( HttpExceptionContent (StatusCodeException),
    responseStatus,
    setRequestCheckStatus,
  )
import Network.HTTP.Simple (HttpException (..), Request)
import Network.HTTP.Types (tooManyRequests429)
import Retry (MaxAttempts, RetryDelayMicros, retryM)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (throwIO, try)

class Monad m => CapabilityHTTP m where
  httpBS :: Request -> m ByteString

httpBSWithRetry ::
  (CapabilityHTTP m, MonadUnliftIO m) =>
  MaxAttempts ->
  RetryDelayMicros ->
  (HttpException -> Bool) ->
  Request ->
  m ByteString
httpBSWithRetry attempts delay shouldRetry req =
  retryM attempts delay (shouldRetryEither shouldRetry) (tryHttpBS req)
    >>= either throwIO pure

tryHttpBS ::
  (CapabilityHTTP m, MonadUnliftIO m) =>
  Request ->
  m (Either HttpException ByteString)
tryHttpBS = try . httpBS . setRequestCheckStatus

shouldRetryEither :: (HttpException -> Bool) -> Either HttpException a -> Bool
shouldRetryEither shouldRetry = fromLeft False . first shouldRetry

isTooManyRequestsException :: HttpException -> Bool
isTooManyRequestsException
  (HttpExceptionRequest _ (StatusCodeException response _)) =
    (== tooManyRequests429) . responseStatus $ response
isTooManyRequestsException _ = False
