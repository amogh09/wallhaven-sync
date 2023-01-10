module Wallhaven.API.Action (getAllCollectionURLs) where

import Control.Monad.Reader (MonadReader, asks)
import Data.Bifunctor (first)
import qualified Retry
import Types
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception
import Util.Batch (batchedM)
import Util.HTTP (CapabilityHTTP, httpBSWithRetry, isTooManyRequestsException)
import Wallhaven.API.Class (HasNumParallelDownloads, HasWallhavenAPIKey, getNumParallelDownloads, getWallhavenAPIKey)
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
    HasNumParallelDownloads env,
    Retry.HasRetryConfig env,
    Retry.CapabilityThreadDelay m,
    CapabilityHTTP m
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
    MonadUnliftIO m,
    Retry.HasRetryConfig env,
    Retry.CapabilityThreadDelay m,
    CapabilityHTTP m
  ) =>
  Username ->
  CollectionID ->
  Page ->
  m [FullWallpaperURL]
getCollectionWallpaperURLsForPage username cid page = do
  apiKey <- asks getWallhavenAPIKey
  let req = wallhavenCollectionPageRequest username apiKey cid page
  catch
    ( httpBSWithRetry isTooManyRequestsException req
        >>= fromEither
          . first Exception.WallpapersParseException
          . extractFullWallpaperURLs
    )
    (throwIO . Exception.CollectionWallpapersFetchException cid page)

-- | Gets the last page number of the given collection.
getWallpapersLastPage ::
  ( MonadReader env m,
    MonadUnliftIO m,
    Retry.HasRetryConfig env,
    HasWallhavenAPIKey env,
    CapabilityHTTP m,
    Retry.CapabilityThreadDelay m
  ) =>
  Username ->
  CollectionID ->
  m Int
getWallpapersLastPage username cid = do
  apiKey <- asks getWallhavenAPIKey
  let req = wallhavenCollectionPageRequest username apiKey cid 1
  catch
    ( httpBSWithRetry isTooManyRequestsException req
        >>= fromEither
          . first Exception.WallhavenMetaParseException
          . extractWallhavenMetaLastPage
    )
    (throwIO . Exception.CollectionWallpapersFetchException cid 1)

-- Calls Wallhaven API and retrieves the ID of the collection to sync.
getCollectionID ::
  ( MonadReader env m,
    HasWallhavenAPIKey env,
    MonadUnliftIO m,
    Retry.HasRetryConfig env,
    Retry.CapabilityThreadDelay m,
    CapabilityHTTP m
  ) =>
  Username ->
  Label ->
  m CollectionID
getCollectionID username label = do
  apiKey <- asks getWallhavenAPIKey
  let req = wallhavenCollectionsRequest username apiKey
  catch
    ( httpBSWithRetry isTooManyRequestsException req
        >>= fromEither . parseCollectionID label
    )
    (throwIO . Exception.CollectionsFetchException)
