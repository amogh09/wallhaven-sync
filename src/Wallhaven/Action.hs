module Wallhaven.Action (deleteUnlikedWallpapers, syncAllWallpapers) where

import Control.Monad (forever, unless, when)
import Control.Monad.Reader (MonadReader, asks)
import Data.Bifunctor (first)
import Data.Either (lefts)
import qualified Network.HTTP.Simple as HTTP
import Types (FullWallpaperURL, Label, LocalWallpapers, Username, WallpaperName)
import UnliftIO
import UnliftIO.Concurrent (threadDelay)
import Util.Batch (batchedM)
import Util.HTTP (MonadHTTPRetry, httpBSWithRetry, isTooManyRequestsException)
import Wallhaven.API.Class (HasNumParallelDownloads, getNumParallelDownloads)
import qualified Wallhaven.Exception as Exception
import qualified Wallhaven.Logic as Logic
import Wallhaven.Monad
  ( HasDeleteUnliked,
    HasLog,
    MonadWallhaven,
    MonadWallpaperDB,
    deleteWallpaper,
    getCollectionURLs,
    getDeleteUnliked,
    getDownloadedWallpapers,
    getLog,
    saveWallpaper,
  )
import Prelude hiding (log, writeFile)

log :: (MonadReader r m, HasLog r, MonadIO m) => String -> m ()
log !msg = do
  logger <- asks getLog
  liftIO $ logger msg
  hFlush stdout

logLn :: (MonadReader r m, HasLog r, MonadIO m) => String -> m ()
logLn msg = log (msg <> "\n")

type AppM env m =
  ( HasDeleteUnliked env,
    HasLog env,
    HasNumParallelDownloads env,
    MonadWallhaven m,
    MonadHTTPRetry env m,
    MonadWallpaperDB m
  )

type MonadBatchedHTTP env m =
  ( MonadHTTPRetry env m,
    HasNumParallelDownloads env
  )

syncAllWallpapers :: AppM env m => Username -> Label -> m ()
syncAllWallpapers username label = do
  fullURLs <- getCollectionURLs username label
  deleteUnliked <- asks getDeleteUnliked
  when deleteUnliked $ do
    unliked <- deleteUnlikedWallpapers fullURLs
    logLn "Following wallpapers were unliked and deleted:"
    mapM_ logLn unliked
  syncWallpapers fullURLs

-- Deletes the local wallpapers that are not in the favorites anymore.
-- Returns the wallpapers that were deleted.
deleteUnlikedWallpapers ::
  MonadWallpaperDB m => [FullWallpaperURL] -> m [WallpaperName]
deleteUnlikedWallpapers favURLs = do
  unliked <- Logic.unlikedWallpapers favURLs <$> getDownloadedWallpapers
  unless (null unliked) (mapM_ deleteWallpaper unliked)
  return unliked

-- wallpaper directory. Skips any wallpapers that are already present.
-- Displays a progress bar while downloading.
syncWallpapers ::
  ( MonadHTTPRetry env m,
    HasLog env,
    HasNumParallelDownloads env,
    MonadWallpaperDB m
  ) =>
  [FullWallpaperURL] ->
  m ()
syncWallpapers urls = do
  progressVar <- newMVar 0 -- to be updated by threads downloading individual wallpapers.
  results <-
    withAsync
      ( forever $ do
          printProgressBar progressVar (length urls)
          threadDelay 100000
      )
      (const $ syncWallpapersInBatches progressVar urls)
  printProgressBar progressVar (length urls)
  logLn ""
  let exceptions =
        fmap (uncurry Exception.WallpaperDownloadException)
          . lefts
          . zipWith (\url res -> first (url,) res) urls
          $ results
  if not (null exceptions)
    then do
      logLn
        ( "Failed to sync the following "
            <> show (length exceptions)
            <> " wallpapers.\n"
        )
      mapM_ log . fmap ((<> "\n") . displayException) $ exceptions
    else do
      logLn "All wallpapers synced successfully.\n"

-- | Downloads the given wallpapers in batches of specified size.
syncWallpapersInBatches ::
  (MonadBatchedHTTP env m, MonadWallpaperDB m) =>
  MVar Int ->
  [FullWallpaperURL] ->
  m [Either HTTP.HttpException ()]
syncWallpapersInBatches progressVar urls = do
  localWallpapers <- getDownloadedWallpapers
  parallelDownloads <- asks getNumParallelDownloads
  batchedM
    parallelDownloads
    (try . syncWallpaper localWallpapers progressVar)
    urls

--  | Downloads a single wallpaper and saves it to the wallpaper directory.
--  Updates the progress variable when done.
syncWallpaper ::
  (MonadBatchedHTTP env m, MonadWallpaperDB m) =>
  LocalWallpapers ->
  MVar Int ->
  FullWallpaperURL ->
  m ()
syncWallpaper localWallpapers progressVar url = do
  unless
    (Logic.wallpaperName url `elem` localWallpapers)
    ( httpBSWithRetry isTooManyRequestsException (HTTP.parseRequest_ url)
        >>= saveWallpaper (Logic.wallpaperName url)
    )
  modifyMVar_ progressVar (evaluate . (+ 1))

-- | Prints the current state of the download progress bar.
printProgressBar ::
  (MonadIO m, MonadReader env m, HasLog env) => MVar Int -> Int -> m ()
printProgressBar var total = do
  doneCount <- readMVar var
  log ("\rSynced: [" <> show doneCount <> "/" <> show total <> "]")
