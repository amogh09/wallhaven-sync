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
    -- | Whether to delete unliked wallpapers.
    configDeleteUnliked :: Bool,
    -- | Wallhaven usename
    configUsername :: String,
    -- | Wallhaven API Key
    configWallhavenAPIKey :: String,
    -- | Label of the collection to sync
    configCollectionLabel :: String,
    -- | Cookie to use for authentication.
    configCookie :: AuthCookie
  }

class HasCollectionLabel a where
  getCollectionLabel :: a -> String

instance HasCollectionLabel Config where
  getCollectionLabel = configCollectionLabel

instance HasCollectionLabel Env where
  getCollectionLabel = getCollectionLabel . envConfig

class HasWallhavenUsername a where
  getWallhavenUsername :: a -> String

instance HasWallhavenUsername Config where
  getWallhavenUsername = configUsername

instance HasWallhavenUsername Env where
  getWallhavenUsername = getWallhavenUsername . envConfig

class HasWallhavenAPIKey a where
  getWallhavenAPIKey :: a -> String

instance HasWallhavenAPIKey Config where
  getWallhavenAPIKey = configWallhavenAPIKey

instance HasWallhavenAPIKey Env where
  getWallhavenAPIKey = getWallhavenAPIKey . envConfig

class HasDeleteUnliked a where
  getDeleteUnliked :: a -> Bool

instance HasDeleteUnliked Config where
  getDeleteUnliked = configDeleteUnliked

instance HasDeleteUnliked Env where
  getDeleteUnliked = getDeleteUnliked . envConfig

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

type LocalWallpapers = [FilePath]

type FavoritePreviews = [PreviewURL]

type Label = String

type CollectionID = Int

-- Either local wallpaper path or preview or full wallpaper URL.
type WallpaperPath = String

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

data APIException
  = APIParseException
      { apiParseExceptionMessage :: String,
        apiParseExceptionInternalException :: String
      }
  | LabelNotFoundException Label
  deriving (Show, Typeable)

instance Exception APIException where
  displayException (APIParseException msg e) =
    "Failed to parse Wallhaven API response: " <> msg
  displayException (LabelNotFoundException label) =
    "A collection with label '" <> label <> "' was not found"

log :: (MonadReader r m, HasLog r, MonadIO m) => String -> m ()
log !msg = do
  logger <- asks getLog
  liftIO $ logger msg
  hFlush stdout

logLn :: (MonadReader r m, HasLog r, MonadIO m) => String -> m ()
logLn msg = log (msg <> "\n")
