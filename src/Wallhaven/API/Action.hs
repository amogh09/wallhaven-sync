module Wallhaven.API.Action (getAllCollectionURLs, getFullWallpaper) where

import Control.Monad.Reader (MonadReader, asks)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Network.HTTP.Simple (Request, parseRequest_)
import qualified Retry
import Types
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception
import Util.Batch (batchedM)
import Util.HTTP (httpBSWithRetry, isTooManyRequestsException)
import Util.Time (seconds)
import Wallhaven.API.Class
  ( HasNumParallelDownloads,
    HasWallhavenAPIKey,
    getNumParallelDownloads,
    getWallhavenAPIKey,
  )
import qualified Wallhaven.API.Exception as Exception
import Wallhaven.API.Logic
  ( extractFullWallpaperURLs,
    extractWallhavenMetaLastPage,
    parseCollectionID,
    wallhavenCollectionPageRequest,
    wallhavenCollectionsRequest,
  )

getAllCollectionURLs ::
  ( MonadUnliftIO m,
    MonadReader env m,
    HasWallhavenAPIKey env,
    HasNumParallelDownloads env
  ) =>
  Username ->
  Label ->
  m [FullWallpaperURL]
getAllCollectionURLs username collectionLabel = do
  numParallelDownloads <- asks getNumParallelDownloads
  collectionID <- getCollectionID username collectionLabel
  lastPage <- getWallpapersLastPage username collectionID
  fmap concat
    . batchedM
      numParallelDownloads
      (getCollectionWallpaperURLsForPage username collectionID)
    $ [1 .. lastPage]

-- | Gets a list of all wallpapers in the given collection for the given page.
getCollectionWallpaperURLsForPage ::
  ( MonadReader env m,
    HasWallhavenAPIKey env,
    MonadUnliftIO m
  ) =>
  Username ->
  CollectionID ->
  Page ->
  m [FullWallpaperURL]
getCollectionWallpaperURLsForPage username cid page = do
  apiKey <- asks getWallhavenAPIKey
  let req = wallhavenCollectionPageRequest username apiKey cid page
  catch
    ( httpCall req
        >>= fromEither
          . first Exception.WallpapersParseException
          . extractFullWallpaperURLs
    )
    (throwIO . Exception.CollectionWallpapersFetchException cid page)

-- | Gets the last page number of the given collection.
getWallpapersLastPage ::
  ( MonadReader env m,
    MonadUnliftIO m,
    HasWallhavenAPIKey env
  ) =>
  Username ->
  CollectionID ->
  m Int
getWallpapersLastPage username cid = do
  apiKey <- asks getWallhavenAPIKey
  let req = wallhavenCollectionPageRequest username apiKey cid 1
  catch
    ( httpCall req
        >>= fromEither
          . first Exception.WallhavenMetaParseException
          . extractWallhavenMetaLastPage
    )
    (throwIO . Exception.CollectionWallpapersFetchException cid 1)

-- Calls Wallhaven API and retrieves the ID of the collection to sync.
getCollectionID ::
  ( MonadReader env m,
    HasWallhavenAPIKey env,
    MonadUnliftIO m
  ) =>
  Username ->
  Label ->
  m CollectionID
getCollectionID username label = do
  apiKey <- asks getWallhavenAPIKey
  let req = wallhavenCollectionsRequest username apiKey
  catch
    (httpCall req >>= fromEither . parseCollectionID label)
    (throwIO . Exception.CollectionsFetchException)

getFullWallpaper :: (MonadUnliftIO m) => FullWallpaperURL -> m ByteString
getFullWallpaper = httpCall . parseRequest_

-- | A sensible default for max API call attempts.
defaultHTTPMaxAttempts :: Retry.MaxAttempts
defaultHTTPMaxAttempts = 5

-- | A sensible default for HTTP call retry delay.
defaultHTTPRetryDelayMicros :: Retry.RetryDelayMicros
defaultHTTPRetryDelayMicros = seconds 3

-- | Makes HTTP calls with retries.
httpCall :: (MonadUnliftIO m) => Request -> m ByteString
httpCall =
  httpBSWithRetry
    defaultHTTPMaxAttempts
    defaultHTTPRetryDelayMicros
    isTooManyRequestsException
