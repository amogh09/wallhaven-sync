module Wallhaven.Favorites (syncAllWallpapers) where

import Control.Monad (forever, unless, when)
import Control.Monad.Reader (MonadReader, asks)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC8
import Data.Either (lefts)
import qualified Data.List as List
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
import Util.HTTP (http2XXWithRetry)
import Util.List (batches)
import Util.Wallhaven (unlikedWallpapers, wallpaperName)
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
    HasWallhavenUsername env
  ) =>
  m ()
syncAllWallpapers = catch go (logLn . displayException @WallpaperSyncException)
  where
    go = do
      asks getWallpaperDir >>= createDirectoryIfMissing True
      localWallpapers <- getLocalWallpapers
      collectionID <- getCollectionIDToSync
      favFullURLs <- getAllCollectionWallpaperFullURLs collectionID
      shouldDeleteUnliked <- asks getDeleteUnliked
      when shouldDeleteUnliked $ deleteUnlikedWallpapers localWallpapers favFullURLs
      syncWallpapers localWallpapers favFullURLs

-- | Gets a list of all local wallpapers in the wallpaper directory.
getLocalWallpapers ::
  (MonadReader env m, HasWallpaperDir env, MonadIO m) =>
  m [FilePath]
getLocalWallpapers = asks getWallpaperDir >>= listDirectory

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
  let name = wallpaperName url
  unless (name `elem` localWallpapers) $ downloadFullWallpaper name
  modifyMVar_ progressVar (return . (+ 1))
  where
    downloadFullWallpaper name = do
      dir <- asks getWallpaperDir
      let filePath = dir </> name
      http2XXWithRetry (HTTP.parseRequest_ url) >>= writeBinaryFile filePath

-- Calls Wallhaven API and retrieves the ID of the collection to sync.
getCollectionIDToSync ::
  ( MonadReader env m,
    HasCollectionLabel env,
    HasWallhavenAPIKey env,
    HasWallhavenUsername env,
    MonadUnliftIO m,
    HasRetryConfig env
  ) =>
  m CollectionID
getCollectionIDToSync = do
  label <- asks getCollectionLabel
  req <- wallhavenCollectionsRequest
  catch
    (http2XXWithRetry req >>= fromEither . parseCollectionID label)
    (throwIO . CollectionsFetchException)

-- | Returns wallhaven request for fetching all collections of the user.
wallhavenCollectionsRequest ::
  (MonadReader env m, HasWallhavenUsername env, HasWallhavenAPIKey env) =>
  m HTTP.Request
wallhavenCollectionsRequest = do
  username <- asks getWallhavenUsername
  apiKey <- asks getWallhavenAPIKey
  let url =
        "https://wallhaven.cc/api/v1/collections/"
          <> username
          <> "?apikey="
          <> apiKey
  return . HTTP.parseRequest_ $ url

-- | Parses Wallhaven collections response and finds the ID of the collection
-- with the provided label.
parseCollectionID ::
  Label -> ByteString -> Either WallpaperSyncException CollectionID
parseCollectionID label jsonBS = do
  collectionsResponse <-
    first
      CollectionsParseException
      (eitherDecodeStrict jsonBS)
  maybe
    (Left . CollectionsParseException $ label)
    (pure . wallhavenCollectionID)
    (findCollectionByLabel label collectionsResponse)

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

-- | Gets a list of all wallpapers in the given collection for the given page.
getCollectionWallpaperURLsForPage ::
  ( MonadReader env m,
    HasWallhavenUsername env,
    HasWallhavenAPIKey env,
    MonadUnliftIO m,
    HasRetryConfig env
  ) =>
  CollectionID ->
  Page ->
  m [FullWallpaperURL]
getCollectionWallpaperURLsForPage cid page =
  catch
    ( wallhavenCollectionPageRequest cid page
        >>= http2XXWithRetry
        >>= fromEither
          . first WallpapersParseException
          . extractFullWallpaperURLs
    )
    (throwIO . CollectionWallpapersFetchException cid page)

-- | Returns an HTTP request for the given page of the given collection.
wallhavenCollectionPageRequest ::
  ( MonadReader env m,
    HasWallhavenUsername env,
    HasWallhavenAPIKey env
  ) =>
  CollectionID ->
  Page ->
  m HTTP.Request
wallhavenCollectionPageRequest cid page = do
  username <- asks getWallhavenUsername
  apiKey <- asks (BC8.pack . getWallhavenAPIKey)
  return
    . HTTP.setRequestQueryString
      [("apikey", Just apiKey), ("page", Just . BC8.pack $ show page)]
    . HTTP.parseRequest_
    $ "https://wallhaven.cc/api/v1/collections/" <> username <> "/" <> show cid

-- | Gets the last page number of the given collection.
getWallpapersLastPage ::
  ( MonadReader env m,
    MonadUnliftIO m,
    HasRetryConfig env,
    HasWallhavenAPIKey env,
    HasWallhavenUsername env
  ) =>
  CollectionID ->
  m Int
getWallpapersLastPage cid =
  catch
    ( wallhavenCollectionPageRequest cid 1
        >>= http2XXWithRetry
        >>= fromEither
          . first WallhavenMetaParseException
          . extractWallhavenMetaLastPage
    )
    (throwIO . CollectionWallpapersFetchException cid 1)

-- Deletes the local wallpapers that are not in the favorites anymore.
deleteUnlikedWallpapers ::
  (MonadIO m, MonadReader env m, HasLog env, HasWallpaperDir env) =>
  LocalWallpapers ->
  [FullWallpaperURL] ->
  m ()
deleteUnlikedWallpapers localWallpapers favURLs = do
  let unliked = unlikedWallpapers favURLs localWallpapers
  unless (null unliked) $ do
    logLn $
      "Following "
        <> show (length unliked)
        <> " wallpapers are not in favorites anymore and will be deleted:\n"
        <> List.intercalate "\n" unliked
    wallpaperDir <- asks getWallpaperDir
    mapM_ removeFile . fmap (wallpaperDir </>) $ unliked

data WallpaperSyncException
  = WallpaperDownloadException FullWallpaperURL HTTP.HttpException
  | CollectionsFetchException HTTP.HttpException
  | CollectionsParseException String
  | CollectionNotFoundException Label
  | CollectionWallpapersFetchException CollectionID Page HTTP.HttpException
  | WallpapersParseException String
  | WallhavenMetaParseException String
  deriving (Typeable, Show)

instance Exception WallpaperSyncException where
  displayException (WallpaperDownloadException url e) =
    url <> ": " <> displayHTTPException e
  displayException (CollectionsFetchException e) =
    "Failed to fetch collections: " <> displayHTTPException e
  displayException (CollectionsParseException _) =
    "Failed to parse API response to collections"
  displayException (CollectionNotFoundException label) =
    "Collection with label " <> label <> " was not found"
  displayException (CollectionWallpapersFetchException collectionID page e) =
    "Failed to fetch wallpapers for collection "
      <> show collectionID
      <> " page "
      <> show page
      <> ": "
      <> displayHTTPException e
  displayException (WallpapersParseException _) =
    "Failed to parse wallpapers response"
  displayException (WallhavenMetaParseException _) =
    "Failed to parse meta information from wallpapers response"

displayHTTPException :: HTTP.HttpException -> String
displayHTTPException (HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException resp _)) = do
  let status = HTTP.responseStatus resp
  show (HTTP.statusCode status)
    <> " "
    <> BC8.unpack (HTTP.statusMessage status)
displayHTTPException (HTTP.HttpExceptionRequest _ HTTP.ResponseTimeout) =
  "Response timeout"
displayHTTPException (HTTP.HttpExceptionRequest _ HTTP.ConnectionTimeout) =
  "Connection timeout"
displayHTTPException e = displayException e
