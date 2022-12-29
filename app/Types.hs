module Types where

import Control.Monad.Reader (MonadReader, asks, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC8
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import UnliftIO (Exception, MonadIO, Typeable, displayException, hFlush, stdout)
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

data Env = Env
  { envConfig :: !Config,
    envLog :: !(String -> IO ())
  }

-- Configuration structure.
data Config = Config
  { -- | The directory where the wallpapers will be saved.
    configWallpaperDir :: FilePath,
    -- | The number of wallpapers to download in parallel.
    configNumParallelDownloads :: NumParallelDownloads,
    -- | Retry config for HTTP requests.
    configRetryConfig :: RetryConfig,
    -- | Cookie to use for authentication.
    configCookie :: AuthCookie
  }

class HasLog a where
  getLog :: a -> (String -> IO ())

instance HasLog Env where
  getLog = envLog

class HasNumParallelDownloads a where
  getNumParallelDownloads :: a -> NumParallelDownloads

instance HasNumParallelDownloads Config where
  getNumParallelDownloads = configNumParallelDownloads

instance HasNumParallelDownloads Env where
  getNumParallelDownloads = getNumParallelDownloads . envConfig

class HasWallpaperDir a where
  getWallpaperDir :: a -> FilePath

instance HasWallpaperDir Config where
  getWallpaperDir = configWallpaperDir

instance HasWallpaperDir Env where
  getWallpaperDir = getWallpaperDir . envConfig

instance HasRetryConfig Config where
  getRetryConfig = configRetryConfig

instance HasRetryConfig Env where
  getRetryConfig = getRetryConfig . envConfig

class HasConfig a where
  getConfig :: a -> Config

instance HasConfig Config where
  getConfig = id

instance HasConfig Env where
  getConfig = envConfig

class HasAuthCookie a where
  getAuthCookie :: a -> AuthCookie

instance HasAuthCookie AuthCookie where
  getAuthCookie = id

instance HasAuthCookie Config where
  getAuthCookie = configCookie

instance HasAuthCookie Env where
  getAuthCookie = getAuthCookie . getConfig

newtype FullWallpaperURLParseException = FullWallpaperURLParseException String
  deriving (Show, Typeable)

instance Exception FullWallpaperURLParseException

type URL = String

type FullWallpaperURL = URL

type PreviewURL = String

type WallpaperName = String

type NumParallelDownloads = Int

type AuthCookie = ByteString

type Page = Int

data WallpaperSyncException = WallpaperSyncException PreviewURL HTTP.HttpException
  deriving (Typeable, Show)

instance Exception WallpaperSyncException where
  displayException
    ( WallpaperSyncException
        url
        (HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException resp _))
      ) = do
      let status = HTTP.responseStatus resp
      url
        <> ": "
        <> show (HTTP.statusCode status)
        <> " "
        <> BC8.unpack (HTTP.statusMessage status)
  displayException
    ( WallpaperSyncException
        url
        (HTTP.HttpExceptionRequest _ HTTP.ResponseTimeout)
      ) = url <> ": " <> "Response timeout"
  displayException
    ( WallpaperSyncException
        url
        (HTTP.HttpExceptionRequest _ HTTP.ConnectionTimeout)
      ) = url <> ": " <> "Connection timeout"
  displayException (WallpaperSyncException url e) =
    url <> ": " <> displayException e

log :: (MonadReader r m, HasLog r, MonadIO m) => String -> m ()
log msg = do
  logger <- asks getLog
  liftIO $ logger msg
  hFlush stdout
