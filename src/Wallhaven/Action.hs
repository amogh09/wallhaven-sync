{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Wallhaven.Action (deleteUnlikedWallpapers, syncAllWallpapers) where

import Control.Monad (forever, unless, when)
import Control.Monad.Reader (MonadReader, asks)
import qualified Data.List as List
import Types (FullWallpaperURL, Label, Username, WallpaperName)
import UnliftIO
import UnliftIO.Concurrent (threadDelay)
import Wallhaven.API.Class (HasNumParallelDownloads)
import Wallhaven.Logic (wallpaperName)
import Wallhaven.Monad
  ( HasDeleteUnliked,
    HasLog,
    MonadDownloadWallpaper,
    MonadInitDB (initDB),
    MonadWallhaven,
    MonadWallpaperDB,
    deleteWallpaper,
    downloadWallpaper,
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
  ( MonadReader env m,
    HasDeleteUnliked env,
    HasLog env,
    HasNumParallelDownloads env,
    MonadWallhaven m,
    MonadWallpaperDB m,
    MonadDownloadWallpaper m,
    MonadUnliftIO m
  )

-- | Main function. Syncs the specified collection.
syncAllWallpapers :: AppM env m => Username -> Label -> m ()
syncAllWallpapers username label = do
  initDB
  urls <- getCollectionURLs username label
  let names = fmap wallpaperName urls
  downloaded <- getDownloadedWallpapers
  deleteUnliked <- asks getDeleteUnliked
  when deleteUnliked $ do
    unliked <- deleteUnlikedWallpapers downloaded names
    logLn "Following wallpapers were found to be unliked and so were deleted:"
    mapM_ logLn unliked
  syncWallpapers downloaded $ names `zip` urls
  logLn "\nAll wallpapers synced successfully."

-- | Syncs the provided wallpapers with the database.
syncWallpapers ::
  AppM env m => [WallpaperName] -> [(WallpaperName, FullWallpaperURL)] -> m ()
syncWallpapers downloaded wallpapers = do
  progressVar <- newMVar 0
  withAsync
    ( forever $ do
        printProgressBar progressVar (length wallpapers)
        threadDelay 100000
    )
    (const $ mapM_ (uncurry $ syncWallpaper progressVar downloaded) wallpapers)
  printProgressBar progressVar (length wallpapers)

-- | Syncs a single wallpaper. Skips downloading if the wallpaper is already
-- downloaded. Downloads and saves the wallpaper otherwise.
syncWallpaper ::
  AppM env m => MVar Int -> [WallpaperName] -> WallpaperName -> FullWallpaperURL -> m ()
syncWallpaper progressVar downloaded name url = do
  unless (name `elem` downloaded) $ do
    wallpaper <- downloadWallpaper url
    saveWallpaper name wallpaper
  modifyMVar_ progressVar (evaluate . (+ 1))

-- Deletes the local wallpapers that are not in the favorites anymore.
-- Returns the wallpapers that were deleted.
deleteUnlikedWallpapers ::
  MonadWallpaperDB m => [WallpaperName] -> [WallpaperName] -> m [WallpaperName]
deleteUnlikedWallpapers downloaded liked = do
  let unliked = downloaded List.\\ liked -- TODO: optimize this
  unless (null unliked) (mapM_ deleteWallpaper unliked)
  return unliked

-- | Prints the current state of the download progress bar.
printProgressBar ::
  (MonadIO m, MonadReader env m, HasLog env) => MVar Int -> Int -> m ()
printProgressBar var total = do
  doneCount <- readMVar var
  log ("\rSynced: [" <> show doneCount <> "/" <> show total <> "]")
