module Wallhaven.Favorites (Config (..), syncAllWallpapers, Env (..)) where

import Control.Monad (forever, unless, when)
import Control.Monad.Reader (MonadReader, asks)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC8
import Data.Either (lefts)
import qualified Data.List as List
import qualified Network.HTTP.Simple as HTTP
import System.FilePath
import Text.HTML.TagSoup (fromAttrib, parseTags, (~==))
import Text.StringLike (StringLike)
import Types
import UnliftIO
import UnliftIO.Concurrent
import UnliftIO.Directory
import UnliftIO.IO.File
import Util.HTTP (http2XXWithRetry)
import Util.List (batches)
import Util.Wallhaven (unlikedWallpapers, wallpaperName)
import Prelude hiding (log, writeFile)

syncAllWallpapers ::
  ( MonadReader env m,
    MonadUnliftIO m,
    MonadIO m,
    HasWallpaperDir env,
    HasAuthCookie env,
    HasRetryConfig env,
    HasLog env,
    HasNumParallelDownloads env,
    HasDeleteUnliked env
  ) =>
  m ()
syncAllWallpapers = do
  asks getWallpaperDir >>= createDirectoryIfMissing True
  localWallpapers <- getLocalWallpapers
  favPreviews <- getFavoritePreviewsStartingPage 1
  shouldDeleteUnliked <- asks getDeleteUnliked
  when shouldDeleteUnliked $ deleteUnlikedWallpapers localWallpapers favPreviews
  syncWallpapers localWallpapers favPreviews

getLocalWallpapers ::
  (MonadReader env m, HasWallpaperDir env, MonadIO m) =>
  m [FilePath]
getLocalWallpapers = asks getWallpaperDir >>= listDirectory

getFavoritePreviewsStartingPage ::
  ( MonadReader env m,
    HasRetryConfig env,
    HasAuthCookie env,
    MonadUnliftIO m,
    HasLog env
  ) =>
  Page ->
  m [PreviewURL]
getFavoritePreviewsStartingPage page = do
  urls <- fmap BC8.unpack . parseFavoritePreviews <$> getFavoritesPage page
  if null urls
    then pure []
    else do
      (<> urls) <$> getFavoritePreviewsStartingPage (page + 1)

-- Deletes the local wallpapers that are not in the favorites anymore.
deleteUnlikedWallpapers ::
  (MonadIO m, MonadReader env m, HasLog env, HasWallpaperDir env) =>
  LocalWallpapers ->
  FavoritePreviews ->
  m ()
deleteUnlikedWallpapers localWallpapers favPreviews = do
  let unliked = unlikedWallpapers favPreviews localWallpapers
  logLn $
    "Following "
      <> show (length unliked)
      <> " wallpapers are not in favorites anymore and will be deleted:\n"
      <> List.intercalate "\n" unliked
  wallpaperDir <- asks getWallpaperDir
  mapM_ removeFile . fmap (wallpaperDir </>) $ unliked

getFavoritesPage ::
  ( MonadReader env m,
    HasRetryConfig env,
    HasAuthCookie env,
    MonadUnliftIO m,
    HasLog env
  ) =>
  Page ->
  m ByteString
getFavoritesPage page = favoritesRequest page >>= http2XXWithRetry

favoritesRequest :: (MonadReader env m, HasAuthCookie env) => Page -> m HTTP.Request
favoritesRequest page = do
  cookie <- asks getAuthCookie
  return
    . HTTP.addRequestHeader "Cookie" cookie
    . HTTP.parseRequest_
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
    . List.find (~== ("<img id=\"wallpaper\">" :: String))
    . parseTags

syncWallpapers ::
  ( MonadUnliftIO m,
    MonadReader env m,
    HasRetryConfig env,
    HasWallpaperDir env,
    HasLog env,
    HasNumParallelDownloads env
  ) =>
  [FilePath] ->
  [PreviewURL] ->
  m ()
syncWallpapers localWallpapers urls = do
  progressVar <- newMVar 0 -- to be updated by threads downloading individual wallpapers.
  results <-
    withAsync
      (forever $ printProgressBar progressVar (length urls) >> threadDelay 100000)
      (const $ syncWallpapersInBatches localWallpapers progressVar urls)
  printProgressBar progressVar (length urls)
  log "\n"
  let exceptions =
        fmap (uncurry WallpaperSyncException)
          . lefts
          . zipWith (\url res -> first (url,) res) urls
          $ results
  if not (null exceptions)
    then do
      log $ "Failed to sync the following " <> show (length exceptions) <> " wallpapers.\n"
      mapM_ log . fmap ((<> "\n") . displayException) $ exceptions
    else do
      log "All wallpapers synced successfully.\n"

syncWallpapersInBatches ::
  ( MonadUnliftIO m,
    MonadReader env m,
    HasNumParallelDownloads env,
    HasRetryConfig env,
    HasWallpaperDir env,
    HasLog env
  ) =>
  [FilePath] ->
  MVar Int ->
  [PreviewURL] ->
  m [Either HTTP.HttpException ()]
syncWallpapersInBatches localWallpapers progressVar urls = do
  parallelDownloads <- asks getNumParallelDownloads
  fmap concat
    . mapM (mapConcurrently (try . syncWallpaper localWallpapers progressVar))
    $ batches parallelDownloads urls

printProgressBar ::
  (MonadIO m, MonadReader env m, HasLog env) => MVar Int -> Int -> m ()
printProgressBar var total = do
  doneCount <- readMVar var
  log ("\rSynced: [" <> show doneCount <> "/" <> show total <> "]")

syncWallpaper ::
  ( MonadUnliftIO m,
    MonadReader env m,
    HasRetryConfig env,
    HasWallpaperDir env,
    HasLog env
  ) =>
  [FilePath] ->
  MVar Int ->
  PreviewURL ->
  m ()
syncWallpaper localWallpapers progressVar url = do
  let name = wallpaperName url
  unless (any (List.isInfixOf name) localWallpapers) $ do
    http2XXWithRetry (HTTP.parseRequest_ url)
      >>= maybe
        (throwIO $ FullWallpaperURLParseException name)
        (downloadFullWallpaper . BC8.unpack)
        . parseFullWallpaperURL
  modifyMVar_ progressVar (return . (+ 1))

downloadFullWallpaper ::
  ( MonadUnliftIO m,
    MonadReader env m,
    HasRetryConfig env,
    HasWallpaperDir env,
    HasLog env
  ) =>
  FullWallpaperURL ->
  m ()
downloadFullWallpaper url = do
  dir <- asks getWallpaperDir
  let name = wallpaperName url
      req = HTTP.parseRequest_ $ "https://w.wallhaven.cc" <> url
  http2XXWithRetry req >>= writeBinaryFile (dir <> "/" <> name)
