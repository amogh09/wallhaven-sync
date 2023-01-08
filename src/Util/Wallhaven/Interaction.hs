{-# LANGUAGE FlexibleInstances #-}

module Util.Wallhaven.Interaction
  ( deleteUnlikedWallpapers,
    getCollectionWallpaperURLsForPage,
    syncWallpaper,
    getLocalWallpapers,
    getCollectionIDToSync,
    getWallpapersLastPage,
  )
where

import Control.Applicative (liftA2)
import Control.Monad (unless)
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.Bifunctor (first)
import qualified Network.HTTP.Simple as HTTP
import qualified Retry
import System.FilePath ((</>))
import Types
import qualified Types.WallhavenAPI as API
import UnliftIO (MonadIO, MonadUnliftIO, fromEither, throwIO)
import UnliftIO.Directory (listDirectory)
import UnliftIO.Exception (catch)
import UnliftIO.IO.File (writeBinaryFile)
import Util.HTTP (http2XXWithRetry)
import qualified Util.Wallhaven.Exception as Exception
import qualified Util.Wallhaven.Logic as Logic
import Util.Wallhaven.Monad
  ( CapabilityDeleteWallpaper,
    CapabilityGetDownloadedWallpapers,
    deleteWallpaper,
    getDownloadedWallpapers,
  )

-- Deletes the local wallpapers that are not in the favorites anymore.
-- Returns the wallpapers that were deleted.
deleteUnlikedWallpapers ::
  ( Monad m,
    CapabilityGetDownloadedWallpapers m,
    CapabilityDeleteWallpaper m
  ) =>
  [FullWallpaperURL] ->
  m [WallpaperName]
deleteUnlikedWallpapers favURLs = do
  unliked <- Logic.unlikedWallpapers favURLs <$> getDownloadedWallpapers
  unless (null unliked) (mapM_ deleteWallpaper unliked)
  return unliked

-- | Gets the last page number of the given collection.
getWallpapersLastPage ::
  ( MonadReader env m,
    MonadUnliftIO m,
    Retry.HasRetryConfig env,
    HasWallhavenAPIKey env,
    HasWallhavenUsername env,
    Retry.CapabilityThreadDelay m
  ) =>
  CollectionID ->
  m Int
getWallpapersLastPage cid = do
  catch
    ( wallhavenCollectionPageReq cid 1
        >>= http2XXWithRetry
        >>= fromEither
          . first Exception.WallhavenMetaParseException
          . API.extractWallhavenMetaLastPage
    )
    (throwIO . Exception.CollectionWallpapersFetchException cid 1)

-- | Gets a list of all wallpapers in the given collection for the given page.
getCollectionWallpaperURLsForPage ::
  ( MonadReader env m,
    HasWallhavenUsername env,
    HasWallhavenAPIKey env,
    MonadUnliftIO m,
    Retry.HasRetryConfig env,
    Retry.CapabilityThreadDelay m
  ) =>
  CollectionID ->
  Page ->
  m [FullWallpaperURL]
getCollectionWallpaperURLsForPage cid page = do
  catch
    ( wallhavenCollectionPageReq cid page
        >>= http2XXWithRetry
        >>= fromEither
          . first Exception.WallpapersParseException
          . API.extractFullWallpaperURLs
    )
    (throwIO . Exception.CollectionWallpapersFetchException cid page)

wallhavenCollectionPageReq ::
  (MonadReader env m, HasWallhavenUsername env, HasWallhavenAPIKey env) =>
  CollectionID ->
  Page ->
  m HTTP.Request
wallhavenCollectionPageReq cid page =
  Logic.wallhavenCollectionPageRequest
    <$> asks getWallhavenUsername
    <*> asks getWallhavenAPIKey
    <*> pure cid
    <*> pure page

-- | Returns wallhaven request for fetching all collections of the user.
wallhavenCollectionsReq ::
  (MonadReader env m, HasWallhavenUsername env, HasWallhavenAPIKey env) =>
  m HTTP.Request
wallhavenCollectionsReq =
  liftA2
    Logic.wallhavenCollectionsRequest
    (asks getWallhavenUsername)
    (asks getWallhavenAPIKey)

-- Calls Wallhaven API and retrieves the ID of the collection to sync.
getCollectionIDToSync ::
  ( MonadReader env m,
    HasCollectionLabel env,
    HasWallhavenAPIKey env,
    HasWallhavenUsername env,
    MonadUnliftIO m,
    Retry.HasRetryConfig env,
    Retry.CapabilityThreadDelay m
  ) =>
  m CollectionID
getCollectionIDToSync = do
  label <- asks getCollectionLabel
  catch
    ( wallhavenCollectionsReq
        >>= http2XXWithRetry
        >>= fromEither . Logic.parseCollectionID label
    )
    (throwIO . Exception.CollectionsFetchException)

-- | Gets a list of all local wallpapers in the wallpaper directory.
getLocalWallpapers ::
  (MonadReader env m, HasWallpaperDir env, MonadIO m) => m [FilePath]
getLocalWallpapers = asks getWallpaperDir >>= listDirectory

syncWallpaper ::
  ( MonadUnliftIO m,
    MonadReader env m,
    Retry.HasRetryConfig env,
    HasWallpaperDir env,
    Retry.CapabilityThreadDelay m
  ) =>
  LocalWallpapers ->
  FullWallpaperURL ->
  m ()
syncWallpaper localWallpapers url = do
  let name = Logic.wallpaperName url
  path <- asks ((</> name) . getWallpaperDir)
  unless (name `elem` localWallpapers) (downloadResource path url)

downloadResource ::
  ( MonadUnliftIO m,
    MonadReader env m,
    Retry.HasRetryConfig env,
    Retry.CapabilityThreadDelay m
  ) =>
  FilePath ->
  String ->
  m ()
downloadResource filepath url =
  http2XXWithRetry (HTTP.parseRequest_ url) >>= writeBinaryFile filepath
