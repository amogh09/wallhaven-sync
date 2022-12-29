module Wallhaven.Favorites (Config (..), syncAllWallpapers, Env (..)) where

import Control.Monad (forever, unless)
import Control.Monad.Reader (MonadReader, asks)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC8
import Data.Either (lefts)
import Data.List (find, isInfixOf)
import Data.List.Split (splitOn)
import Network.HTTP.Conduit (HttpExceptionContent (StatusCodeException), responseStatus)
import Network.HTTP.Simple (HttpException (HttpExceptionRequest), Request, addRequestHeader, parseRequest_)
import Network.HTTP.Types (Status (statusCode), statusMessage)
import Text.HTML.TagSoup (fromAttrib, parseTags, (~==))
import Text.StringLike (StringLike)
import Types
import UnliftIO
  ( Exception,
    MVar,
    MonadIO,
    MonadUnliftIO,
    Typeable,
    displayException,
    mapConcurrently,
    modifyMVar_,
    newMVar,
    readMVar,
    throwIO,
    try,
    withAsync,
  )
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Directory (createDirectoryIfMissing, listDirectory)
import UnliftIO.IO.File (writeBinaryFile)
import Util.HTTP (http2XXWithRetry)
import Util.List (batches)
import Prelude hiding (log, writeFile)

data WallpaperSyncException = WallpaperSyncException PreviewURL HttpException
  deriving (Typeable, Show)

instance Exception WallpaperSyncException where
  displayException
    ( WallpaperSyncException
        url
        (HttpExceptionRequest _ (StatusCodeException resp _))
      ) = do
      let status = responseStatus resp
      url
        <> ": "
        <> show (statusCode status)
        <> " "
        <> BC8.unpack (statusMessage status)
  displayException e = displayException e

syncAllWallpapers ::
  ( MonadReader env m,
    MonadUnliftIO m,
    MonadIO m,
    HasWallpaperDir env,
    HasAuthCookie env,
    HasRetryConfig env,
    HasLog env,
    HasNumParallelDownloads env
  ) =>
  m ()
syncAllWallpapers = do
  asks getWallpaperDir >>= createDirectoryIfMissing True
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
    MonadUnliftIO m,
    HasLog env
  ) =>
  Page ->
  m [PreviewURL]
getFavoritePreviews page =
  fmap BC8.unpack . parseFavoritePreviews <$> getFavoritesPage page

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
    HasLog env,
    HasNumParallelDownloads env
  ) =>
  [FilePath] ->
  [PreviewURL] ->
  m ()
syncWallpapers localWallpapers urls = do
  progressVar <- newMVar 0
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
  m [Either HttpException ()]
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
  unless (any (isInfixOf name) localWallpapers) $ do
    http2XXWithRetry (parseRequest_ url)
      >>= maybe
        (throwIO $ FullWallpaperURLParseException name)
        (downloadFullWallpaper . BC8.unpack)
        . parseFullWallpaperURL
  modifyMVar_ progressVar (return . (+ 1))

wallpaperName :: URL -> WallpaperName
wallpaperName = last . splitOn "/"

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
      req = parseRequest_ $ fullWallpaperLink url
  http2XXWithRetry req >>= writeBinaryFile (dir <> "/" <> name)
  where
    fullWallpaperLink :: String -> String
    fullWallpaperLink relativePath = "https://w.wallhaven.cc" <> relativePath
