module Types where

import Control.Monad.Reader (MonadReader, asks, liftIO)
import UnliftIO (MonadIO, hFlush, stdout)
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
    configCollectionLabel :: String
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

type FullWallpaperURL = String

type WallpaperName = String

type NumParallelDownloads = Int

type Page = Int

type LocalWallpapers = [FilePath]

type Label = String

type CollectionID = Int

-- Either local wallpaper path or preview or full wallpaper URL.
type WallpaperPath = String

log :: (MonadReader r m, HasLog r, MonadIO m) => String -> m ()
log !msg = do
  logger <- asks getLog
  liftIO $ logger msg
  hFlush stdout

logLn :: (MonadReader r m, HasLog r, MonadIO m) => String -> m ()
logLn msg = log (msg <> "\n")
