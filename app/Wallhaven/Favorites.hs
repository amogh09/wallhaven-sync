module Wallhaven.Favorites (Config (..), syncAllWallpapers, Env (..)) where

import Control.Monad.Reader (MonadReader, asks)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC8
import Data.List (find)
import Data.List.Split (splitOn)
import Data.Typeable (Typeable)
import Network.HTTP.Simple (Request, addRequestHeader, parseRequest_)
import Retry (HasRetryConfig, RetryConfig, getRetryConfig)
import Text.HTML.TagSoup (fromAttrib, parseTags, (~==))
import Text.StringLike (StringLike)
import UnliftIO (Exception, MonadIO, MonadUnliftIO, throwIO)
import UnliftIO.Directory (listDirectory)
import UnliftIO.IO.File (writeBinaryFile)
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

syncAllWallpapers ::
  ( MonadReader env m,
    MonadUnliftIO m,
    MonadIO m,
    HasWallpaperDir env,
    HasAuthCookie env,
    HasRetryConfig env
  ) =>
  m ()
syncAllWallpapers = do
  localWallpapers <- getLocalWallpapers
  getFavoritePreviews 1 >>= syncWallpapers localWallpapers

getLocalWallpapers ::
  (MonadReader env m, HasWallpaperDir env, MonadIO m) =>
  m [FilePath]
getLocalWallpapers = asks getWallpaperDir >>= listDirectory

getFavoritePreviews ::
  ( MonadReader env m,
    HasRetryConfig env,
    HasAuthCookie env,
    MonadUnliftIO m
  ) =>
  Page ->
  m [PreviewURL]
getFavoritePreviews page =
  fmap BC8.unpack . parseFavoritePreviews <$> getFavoritesPage page

getFavoritesPage ::
  ( MonadReader env m,
    HasRetryConfig env,
    HasAuthCookie env,
    MonadUnliftIO m
  ) =>
  Page ->
  m ByteString
getFavoritesPage page = favoritesRequest page >>= http2XXWithRetry

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
  fmap (fromAttrib "src")
    . find (~== ("<img id=\"wallpaper\">" :: String))
    . parseTags

syncWallpapers ::
  (MonadUnliftIO m, MonadReader env m, HasRetryConfig env, HasWallpaperDir env) =>
  [FilePath] ->
  [PreviewURL] ->
  m ()
syncWallpapers localWallpapers = mapM_ (syncWallpaper localWallpapers)

syncWallpaper ::
  (MonadUnliftIO m, MonadReader env m, HasRetryConfig env, HasWallpaperDir env) =>
  [FilePath] ->
  PreviewURL ->
  m ()
syncWallpaper localWallpapers url = do
  let name = wallpaperName url
  if name `elem` localWallpapers
    then pure ()
    else
      http2XXWithRetry (parseRequest_ url)
        >>= maybe
          (throwIO $ FullWallpaperURLParseException name)
          (downloadFullWallpaper . BC8.unpack)
          . parseFullWallpaperURL

wallpaperName :: URL -> WallpaperName
wallpaperName = last . splitOn "/"

downloadFullWallpaper ::
  (MonadUnliftIO m, MonadReader env m, HasRetryConfig env, HasWallpaperDir env) =>
  FullWallpaperURL ->
  m ()
downloadFullWallpaper url = do
  dir <- asks getWallpaperDir
  let name = wallpaperName url
  http2XXWithRetry (parseRequest_ url) >>= writeBinaryFile (dir <> "/" <> name)
