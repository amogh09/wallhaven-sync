module Util.HTTPSpec (spec) where

import Control.Monad.Reader
import Data.ByteString (ByteString)
import qualified Network.HTTP.Client as HTTP
import Retry (CapabilityThreadDelay, HasRetryConfig, RetryConfig (RetryConfig), getRetryConfig, threadDelay)
import Test.Hspec
import UnliftIO (IORef, MonadUnliftIO, modifyIORef', readIORef, throwIO)
import UnliftIO.IORef (newIORef)
import Util.HTTP (CapabilityHTTP, httpBS, httpBSWithRetry)

newtype HTTPTestM r m a = TestM {unTestM :: ReaderT r m a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader r,
      MonadIO,
      MonadUnliftIO
    )

runTestM :: HTTPTestM r m a -> r -> m a
runTestM a r = flip runReaderT r . unTestM $ a

data Env = Env
  { envRetryConfig :: RetryConfig,
    envHTTPResponseBS :: Either HTTP.HttpException ByteString,
    envHTTPRequests :: IORef [HTTP.Request]
  }

class HasHTTPResponseBody a where
  getHTTPResponseBody :: a -> Either HTTP.HttpException ByteString

class HasHTTPRequests a where
  getHTTPRequests :: a -> IORef [HTTP.Request]

instance HasHTTPResponseBody Env where
  getHTTPResponseBody = envHTTPResponseBS

instance HasHTTPRequests Env where
  getHTTPRequests = envHTTPRequests

instance HasRetryConfig Env where
  getRetryConfig = envRetryConfig

instance
  (HasHTTPResponseBody env, Monad m, HasHTTPRequests env, MonadIO m) =>
  CapabilityHTTP (HTTPTestM env m)
  where
  httpBS req = do
    reqs <- asks getHTTPRequests
    modifyIORef' reqs (req :)
    asks getHTTPResponseBody >>= either throwIO pure

instance (Monad m) => CapabilityThreadDelay (HTTPTestM env m) where
  threadDelay _ = pure ()

spec :: Spec
spec =
  describe "httpBSWithRetry" $ do
    it "returns response body on successful request" $ do
      reqs <- newIORef []
      let req = HTTP.parseRequest_ "http://www.example.org"
      res <-
        runTestM
          (httpBSWithRetry (const True) req)
          (Env (RetryConfig 1 0) (Right "response body") reqs)
      res `shouldBe` "response body"
      reqs' <- readIORef reqs
      (HTTP.path <$> reqs') `shouldBe` [HTTP.path req]

    it "throws on non-retryable exception" $ do
      reqs <- newIORef []
      let req = HTTP.parseRequest_ "http://www.example.org"
      let ex = HTTP.HttpExceptionRequest req HTTP.ConnectionTimeout
      let action =
            runTestM
              (httpBSWithRetry (const False) req)
              (Env (RetryConfig 1 0) (Left ex) reqs)
      action `shouldThrow` isConnectionTimeoutException
      reqs' <- readIORef reqs
      (HTTP.path <$> reqs') `shouldBe` [HTTP.path req]

    it "exhausts all retry attempts on repeated retryable exceptions" $ do
      reqs <- newIORef []
      let req = HTTP.parseRequest_ "http://www.example.org"
      let ex = HTTP.HttpExceptionRequest req HTTP.ConnectionTimeout
      let action =
            runTestM
              (httpBSWithRetry (const True) req)
              (Env (RetryConfig 5 0) (Left ex) reqs)
      action `shouldThrow` isConnectionTimeoutException
      reqs' <- readIORef reqs
      (HTTP.path <$> reqs') `shouldBe` replicate 5 (HTTP.path req)

isConnectionTimeoutException :: HTTP.HttpException -> Bool
isConnectionTimeoutException (HTTP.HttpExceptionRequest _ HTTP.ConnectionTimeout) = True
isConnectionTimeoutException _ = False
