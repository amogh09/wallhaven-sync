module Wallhaven.Favorites (syncAllWallpapers) where

import Control.Monad (forever, unless, when)
import Control.Monad.Reader (MonadReader, asks)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC8
import Data.Either (lefts)
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Simple as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import System.FilePath
import Types
import Types.WallhavenAPI
  ( eitherDecodeStrict,
    extractFullWallpaperURLs,
    extractWallhavenMetaLastPage,
    findCollectionByLabel,
    wallhavenCollectionID,
  )
import UnliftIO
import UnliftIO.Concurrent
import UnliftIO.Directory
import UnliftIO.IO.File
import Util.List (batches)
import qualified Util.Wallhaven.Interaction as Interaction
import Prelude hiding (log, writeFile)

-- | Syncs all wallpapers from the given collection identified by its label
-- to the specified local directory.
syncAllWallpapers ::
  ( MonadReader env m,
    MonadUnliftIO m,
    MonadIO m,
    HasWallpaperDir env,
    HasRetryConfig env,
    HasLog env,
    HasNumParallelDownloads env,
    HasDeleteUnliked env,
    HasCollectionLabel env,
    HasWallhavenAPIKey env,
    HasWallhavenUsername env,
    HasDebug env
  ) =>
  m ()
syncAllWallpapers = catch go handler
  where
    go = do
      asks getWallpaperDir >>= createDirectoryIfMissing True
      localWallpapers <- getLocalWallpapers
      collectionID <- getCollectionIDToSync
      favFullURLs <- getAllCollectionWallpaperFullURLs collectionID
      shouldDeleteUnliked <- asks getDeleteUnliked
      when shouldDeleteUnliked $ deleteUnlikedWallpapers localWallpapers favFullURLs
      syncWallpapers localWallpapers favFullURLs

    handler e = do
      debugMode <- asks getDebug
      if debugMode
        then logLn $ displayExceptionVerbose e
        else logLn $ displayException e

-- | Downloads the given wallpapers using their URLs and saves them to the
-- wallpaper directory. Skips any wallpapers that are already present.
-- Displays a progress bar while downloading.
syncWallpapers ::
  ( MonadUnliftIO m,
    MonadReader env m,
    HasRetryConfig env,
    HasWallpaperDir env,
    HasLog env,
    HasNumParallelDownloads env
  ) =>
  LocalWallpapers ->
  [FullWallpaperURL] ->
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
        fmap (uncurry WallpaperDownloadException)
          . lefts
          . zipWith (\url res -> first (url,) res) urls
          $ results
  if not (null exceptions)
    then do
      log $ "Failed to sync the following " <> show (length exceptions) <> " wallpapers.\n"
      mapM_ log . fmap ((<> "\n") . displayException) $ exceptions
    else do
      log "All wallpapers synced successfully.\n"

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
    HasRetryConfig env,
    HasWallpaperDir env,
    HasLog env
  ) =>
  LocalWallpapers ->
  MVar Int ->
  [FullWallpaperURL] ->
  m [Either HTTP.HttpException ()]
syncWallpapersInBatches localWallpapers progressVar urls = do
  parallelDownloads <- asks getNumParallelDownloads
  fmap concat
    . mapM (mapConcurrently (try . syncWallpaper localWallpapers progressVar))
    $ batches parallelDownloads urls

--  | Downloads a single wallpaper and saves it to the wallpaper directory.
--  Skips if the wallpaper is already present.
--  Updates the progress variable when done.
syncWallpaper ::
  ( MonadUnliftIO m,
    MonadReader env m,
    HasRetryConfig env,
    HasWallpaperDir env,
    HasLog env
  ) =>
  LocalWallpapers ->
  MVar Int ->
  FullWallpaperURL ->
  m ()
syncWallpaper localWallpapers progressVar url = do
  Interaction.syncWallpaper localWallpapers url
  modifyMVar_ progressVar (return . (+ 1))

-- | Gets a list of all wallpapers in the given collection.
getAllCollectionWallpaperFullURLs ::
  ( MonadReader env m,
    HasWallhavenUsername env,
    HasWallhavenAPIKey env,
    HasRetryConfig env,
    MonadUnliftIO m,
    HasNumParallelDownloads env
  ) =>
  CollectionID ->
  m [FullWallpaperURL]
getAllCollectionWallpaperFullURLs collectionID = do
  lastPage <- getWallpapersLastPage collectionID
  parallelDownloads <- asks getNumParallelDownloads
  fmap (concat . concat)
    . mapM (mapConcurrently (getCollectionWallpaperURLsForPage collectionID))
    . batches parallelDownloads
    $ [1 .. lastPage]
