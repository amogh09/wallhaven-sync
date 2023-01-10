module Wallhaven.Favorites (syncAllWallpapers) where

import Control.Monad.Reader (MonadReader)
import qualified Retry
import Types
import UnliftIO
import Wallhaven.Env (log)
import Wallhaven.Monad (HasLog)
import Prelude hiding (log, writeFile)

-- | Syncs all wallpapers from the given collection identified by its label
-- to the specified local directory.
syncAllWallpapers ::
  ( MonadReader env m,
    MonadUnliftIO m,
    MonadIO m,
    Retry.HasRetryConfig env
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
-- syncWallpapers ::
--   ( MonadUnliftIO m,
--     MonadReader env m,
--     Retry.HasRetryConfig env
--   ) =>
--   LocalWallpapers ->
--   [FullWallpaperURL] ->
--   m ()
-- syncWallpapers = undefined

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
