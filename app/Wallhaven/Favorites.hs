module Wallhaven.Favorites (syncAllWallpapers) where

import Control.Monad.Reader (MonadReader, asks)
import qualified Network.HTTP.Conduit as HTTP
import qualified Retry
import Types
import UnliftIO
import Util.Batch (batchedM)
import Util.HTTP (CapabilityHTTP)
import Util.List (batches)
import qualified Wallhaven.Action as Action
import Prelude hiding (log, writeFile)

-- | Syncs all wallpapers from the given collection identified by its label
-- to the specified local directory.
syncAllWallpapers ::
  ( MonadReader env m,
    MonadUnliftIO m,
    MonadIO m,
    HasWallpaperDir env,
    Retry.HasRetryConfig env,
    HasLog env,
    HasNumParallelDownloads env,
    HasDeleteUnliked env,
    HasCollectionLabel env,
    HasWallhavenAPIKey env,
    HasWallhavenUsername env,
    HasDebug env
  ) =>
  m ()
syncAllWallpapers = undefined

-- syncAllWallpapers = catch go handler
--   where
--     go = do
--       asks getWallpaperDir >>= createDirectoryIfMissing True
--       localWallpapers <- getLocalWallpapers
--       collectionID <- getCollectionIDToSync
--       favFullURLs <- getAllCollectionWallpaperFullURLs collectionID
--       shouldDeleteUnliked <- asks getDeleteUnliked
--       when shouldDeleteUnliked $ deleteUnlikedWallpapers localWallpapers favFullURLs
--       syncWallpapers localWallpapers favFullURLs

--     handler e = do
--       debugMode <- asks getDebug
--       if debugMode
--         then logLn $ displayExceptionVerbose e
--         else logLn $ displayException e

-- | Downloads the given wallpapers using their URLs and saves them to the
-- wallpaper directory. Skips any wallpapers that are already present.
-- Displays a progress bar while downloading.
syncWallpapers ::
  ( MonadUnliftIO m,
    MonadReader env m,
    Retry.HasRetryConfig env,
    HasWallpaperDir env,
    HasLog env,
    HasNumParallelDownloads env
  ) =>
  LocalWallpapers ->
  [FullWallpaperURL] ->
  m ()
syncWallpapers = undefined

-- syncWallpapers localWallpapers urls = do
--   progressVar <- newMVar 0 -- to be updated by threads downloading individual wallpapers.
--   results <-
--     withAsync
--       (forever $ printProgressBar progressVar (length urls) >> threadDelay 100000)
--       (const $ syncWallpapersInBatches localWallpapers progressVar urls)
--   printProgressBar progressVar (length urls)
--   log "\n"
--   let exceptions =
--         fmap (uncurry WallpaperDownloadException)
--           . lefts
--           . zipWith (\url res -> first (url,) res) urls
--           $ results
--   if not (null exceptions)
--     then do
--       log $ "Failed to sync the following " <> show (length exceptions) <> " wallpapers.\n"
--       mapM_ log . fmap ((<> "\n") . displayException) $ exceptions
--     else do
--       log "All wallpapers synced successfully.\n"

-- | Prints the current state of the download progress bar.
printProgressBar ::
  (MonadIO m, MonadReader env m, HasLog env) => MVar Int -> Int -> m ()
printProgressBar var total = do
  doneCount <- readMVar var
  log ("\rSynced: [" <> show doneCount <> "/" <> show total <> "]")

-- | Downloads the given wallpapers in batches of specified size.
syncWallpapersInBatches ::
  ( MonadUnliftIO m,
    MonadReader env m,
    HasNumParallelDownloads env,
    Retry.HasRetryConfig env,
    HasWallpaperDir env,
    HasLog env,
    Retry.CapabilityThreadDelay m,
    CapabilityHTTP m
  ) =>
  LocalWallpapers ->
  MVar Int ->
  [FullWallpaperURL] ->
  m [Either HTTP.HttpException ()]
syncWallpapersInBatches localWallpapers progressVar urls = do
  parallelDownloads <- asks getNumParallelDownloads
  batchedM
    parallelDownloads
    (try . syncWallpaper localWallpapers progressVar)
    urls

-- fmap concat
--   . mapM (mapConcurrently (try . syncWallpaper localWallpapers progressVar))
--   $ batches parallelDownloads urls

--  | Downloads a single wallpaper and saves it to the wallpaper directory.
--  Skips if the wallpaper is already present.
--  Updates the progress variable when done.
syncWallpaper ::
  ( MonadUnliftIO m,
    MonadReader env m,
    Retry.HasRetryConfig env,
    HasWallpaperDir env,
    HasLog env,
    Retry.CapabilityThreadDelay m,
    CapabilityHTTP m
  ) =>
  LocalWallpapers ->
  MVar Int ->
  FullWallpaperURL ->
  m ()
syncWallpaper localWallpapers progressVar url = do
  Action.syncWallpaper localWallpapers url
  modifyMVar_ progressVar (return . (+ 1))
