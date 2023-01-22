module Wallhaven.API.Action (getAllCollectionURLs, getFullWallpaper) where

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Network.HTTP.Simple (Request, parseRequest_)
import Types
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception
import Util.Batch (batchedM)
import Util.HTTP (httpBSWithRetry, isTooManyRequestsException)
import qualified Util.Retry as Retry
import Util.Time (seconds)
import qualified Wallhaven.API.Exception as Exception
import Wallhaven.API.Logic
  ( extractFullWallpaperURLs,
    extractWallhavenMetaLastPage,
    parseCollectionID,
    wallhavenCollectionPageRequest,
    wallhavenCollectionsRequest,
  )
import Wallhaven.API.Types (APIKey)

getAllCollectionURLs ::
  (MonadUnliftIO m) => APIKey -> Username -> Label -> m [FullWallpaperURL]
getAllCollectionURLs apiKey username collectionLabel = do
  collectionID <- getCollectionID apiKey username collectionLabel
  lastPage <- getWallpapersLastPage apiKey username collectionID
  fmap concat
    . batchedM
      defaultCallBatchSize
      (getCollectionWallpaperURLsForPage apiKey username collectionID)
    $ [1 .. lastPage]

-- | Gets a list of all wallpapers in the given collection for the given page.
getCollectionWallpaperURLsForPage ::
  (MonadUnliftIO m) =>
  APIKey ->
  Username ->
  CollectionID ->
  Page ->
  m [FullWallpaperURL]
getCollectionWallpaperURLsForPage apiKey username cid page = do
  let req = wallhavenCollectionPageRequest username apiKey cid page
  catch
    ( httpCall req
        >>= fromEither
          . first Exception.WallpaperURLsParseException
          . extractFullWallpaperURLs
    )
    (throwIO . Exception.CollectionFetchHTTPException cid page)

-- | Gets the last page number of the given collection.
getWallpapersLastPage ::
  (MonadUnliftIO m) => APIKey -> Username -> CollectionID -> m Int
getWallpapersLastPage apiKey username cid = do
  let req = wallhavenCollectionPageRequest username apiKey cid 1
  catch
    ( httpCall req
        >>= fromEither
          . first (Exception.MetaParseException cid)
          . extractWallhavenMetaLastPage
    )
    (throwIO . Exception.CollectionFetchHTTPException cid 1)

-- Calls Wallhaven API and retrieves the ID of the collection to sync.
getCollectionID ::
  (MonadUnliftIO m) => APIKey -> Username -> Label -> m CollectionID
getCollectionID apiKey username label = do
  let req = wallhavenCollectionsRequest username apiKey
  catch
    (httpCall req >>= fromEither . parseCollectionID label)
    (throwIO . Exception.UserCollectionsHTTPException)

getFullWallpaper :: (MonadUnliftIO m) => FullWallpaperURL -> m ByteString
getFullWallpaper = httpCall . parseRequest_

defaultCallBatchSize :: NumParallelDownloads
defaultCallBatchSize = 5

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
