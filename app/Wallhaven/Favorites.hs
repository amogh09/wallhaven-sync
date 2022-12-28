module Wallhaven.Favorites (Config (..), SyncError, getFavoritePreviews, Env (..)) where

import Control.Exception.Safe (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC8
import Data.List (find)
import Network.HTTP.Conduit (HttpException)
import Network.HTTP.Simple (Request, addRequestHeader, parseRequest_)
import Retry (HasRetryConfig, RetryConfig, getRetryConfig)
import Text.HTML.TagSoup (fromAttrib, parseTags, (~==))
import Text.StringLike (StringLike)
import Util.HTTP (http2XXWithRetry)
import Prelude hiding (writeFile)

newtype Env = Env {envConfig :: Config}

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

data SyncError
  = FavoritesFetchError Page HttpException

type FavoritePageURL = String

type FullWallpaperURL = String

type PreviewURL = String

type WallpaperName = String

type NumParallelDownloads = Int

type AuthCookie = ByteString

type Page = Int

type RetryDelaySeconds = Int

getFavoritePreviews ::
  ( MonadReader env m,
    HasConfig env,
    MonadThrow m,
    MonadIO m,
    HasRetryConfig env,
    HasAuthCookie env,
    MonadCatch m
  ) =>
  Page ->
  m [PreviewURL]
getFavoritePreviews page =
  fmap BC8.unpack . parseFavoritePreviews <$> getFavoritesPage page

getFavoritesPage ::
  ( MonadReader env m,
    HasConfig env,
    MonadThrow m,
    MonadIO m,
    HasRetryConfig env,
    HasAuthCookie env,
    MonadCatch m
  ) =>
  Page ->
  m ByteString
getFavoritesPage page =
  favoritesRequest page >>= http2XXWithRetry

favoritesRequest :: (MonadReader env m, HasAuthCookie env) => Page -> m Request
favoritesRequest page = do
  cookie <- asks getAuthCookie
  return
    . addRequestHeader "Cookie" cookie
    . parseRequest_
    $ "https://wallhaven.cc/favorites?page=" <> show page

-- Extracts a list of favorites wallpapers from Wallhaven's favorites page.
parseFavoritePreviews :: (Show str, StringLike str) => str -> [str]
parseFavoritePreviews =
  fmap (fromAttrib "href")
    . filter (~== ("<a class=\"preview\">" :: String))
    . parseTags

-- Parses full wallpaper link from a preview page.
parseFullWallpaperURL :: (Show str, StringLike str) => str -> Maybe str
parseFullWallpaperURL =
  fmap (fromAttrib "href")
    . find (~== ("<a class=\"full\" href=\"#\">" :: String))
    . parseTags

-- downloadWallpaperFromFullURL ::
--   (MonadReader Config m, MonadIO m) =>
--   FullWallpaperURL ->
--   m ()
-- downloadWallpaperFromFullURL url = do
--   let name = wallpaperNameFromURL url
--   path <- wallpaperPathFromURL name
--   contents <- httpBSApp (FullWallpaperDownloadError name) (parseRequest_ url)
--   liftIO $ B8.writeFile path contents

-- wallpaperNameFromURL :: FullWallpaperURL -> WallpaperName
-- wallpaperNameFromURL fullWallpaperURL = last $ splitOn "/" fullWallpaperURL

-- wallpaperPathFromURL :: MonadReader Config m => WallpaperName -> m FilePath
-- wallpaperPathFromURL name =
--   reader (\config -> configWallpaperDir config </> name)

-- downloadWallpaperFromPreviewURL ::
--   (MonadError WallpaperError m, MonadReader Config m, MonadIO m) =>
--   PreviewURL ->
--   m ()
-- downloadWallpaperFromPreviewURL previewURL = do
--   let name = wallpaperNameFromURL previewURL
--   httpBSApp (PreviewFetchError name) (parseRequestThrow_ previewURL)
--     >>= maybe (throwError $ FullWallpaperURLParseError name previewURL) pure
--       . parseFullWallpaperURL
--     >>= downloadWallpaperFromFullURL . BC8.unpack

-- downloadWallpapersInBatches ::
--   Config -> [PreviewURL] -> IO [Either WallpaperError ()]
-- downloadWallpapersInBatches config =
--   fmap join
--     . mapM (downloadWallpaperBatchIO config)
--     . batches (configNumParallelDownloads config)

-- downloadWallpaperBatchIO ::
--   Config -> [PreviewURL] -> IO [Either WallpaperError ()]
-- downloadWallpaperBatchIO config =
--   mapConcurrently (runApp config . downloadWallpaperFromPreviewURL)
