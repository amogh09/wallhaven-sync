module Util.HTTP
  ( httpBSWithRetry,
    isTooManyRequestsException,
    CapabilityHTTP,
    httpBS,
  )
where

import Control.Monad.Reader (MonadReader, ReaderT)
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
    Retry.retryM (fromLeft False . first shouldRetry)
      . try
      . httpBS
      . setRequestCheckStatus
      $ req
  either throwIO pure res

isTooManyRequestsException :: HttpException -> Bool
isTooManyRequestsException
  (HttpExceptionRequest _ (StatusCodeException response _)) =
    (== tooManyRequests429) . responseStatus $ response
isTooManyRequestsException _ = False
