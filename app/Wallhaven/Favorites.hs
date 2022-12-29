module Wallhaven.Favorites (Config (..), syncAllWallpapers, Env (..)) where

import Control.Monad.Reader (MonadReader, asks)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC8
import Data.List (find)
import Data.List.Split (splitOn)
import Network.HTTP.Simple (Request, addRequestHeader, parseRequest_)
import Text.HTML.TagSoup (fromAttrib, parseTags, (~==))
import Text.StringLike (StringLike)
import Types
import UnliftIO (MonadIO, MonadUnliftIO, throwIO)
import UnliftIO.Directory (listDirectory)
import UnliftIO.IO.File (writeBinaryFile)
import Util.HTTP (http2XXWithRetry)
import Prelude hiding (log, writeFile)

syncAllWallpapers ::
  ( MonadReader env m,
    MonadUnliftIO m,
    MonadIO m,
    HasWallpaperDir env,
    HasAuthCookie env,
    HasRetryConfig env,
    HasLog env
  ) =>
  m ()
syncAllWallpapers = do
  localWallpapers <- getLocalWallpapers
  log $ "Local wallpapers: " <> show localWallpapers
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
  ( MonadUnliftIO m,
    MonadReader env m,
    HasRetryConfig env,
    HasWallpaperDir env,
    HasLog env
  ) =>
  [FilePath] ->
  [PreviewURL] ->
  m ()
syncWallpapers localWallpapers = mapM_ (syncWallpaper localWallpapers)

syncWallpaper ::
  (MonadUnliftIO m, MonadReader env m, HasRetryConfig env, HasWallpaperDir env, HasLog env) =>
  [FilePath] ->
  PreviewURL ->
  m ()
syncWallpaper localWallpapers url = do
  let name = wallpaperName url
  http2XXWithRetry (parseRequest_ url)
    >>= maybe
      (throwIO $ FullWallpaperURLParseException name)
      (downloadFullWallpaper localWallpapers . BC8.unpack)
      . parseFullWallpaperURL

wallpaperName :: URL -> WallpaperName
wallpaperName = last . splitOn "/"

downloadFullWallpaper ::
  (MonadUnliftIO m, MonadReader env m, HasRetryConfig env, HasWallpaperDir env, HasLog env) =>
  [FilePath] ->
  FullWallpaperURL ->
  m ()
downloadFullWallpaper localWallpapers url = do
  let name = wallpaperName url
  log $ "Working on " <> name
  if name `elem` localWallpapers
    then log $ "Already downloaded " <> name
    else do
      log $ "Downloading " <> name
      dir <- asks getWallpaperDir
      let req = parseRequest_ $ fullWallpaperLink url
      http2XXWithRetry req >>= writeBinaryFile (dir <> "/" <> name)
  where
    fullWallpaperLink :: String -> String
    fullWallpaperLink relativePath = "https://w.wallhaven.cc" <> relativePath
