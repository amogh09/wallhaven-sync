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
import qualified Data.List as List
import qualified Network.HTTP.Simple as HTTP
import System.FilePath ((</>))
import Types
import qualified Types.WallhavenAPI as API
import UnliftIO (MonadIO, MonadUnliftIO, fromEither, liftIO, throwIO)
import UnliftIO.Directory (listDirectory)
import UnliftIO.Exception (catch)
import UnliftIO.IO.File (writeBinaryFile)
import Util.HTTP (http2XXWithRetry)
import Util.Wallhaven.Exception (WallpaperSyncException (WallhavenMetaParseException))
import qualified Util.Wallhaven.Exception as Exception
import Util.Wallhaven.Logic (deleteWallpapers, parseCollectionID, unlikedWallpapers, wallhavenCollectionPageRequest, wallhavenCollectionsRequest, wallpaperName)

-- Deletes the local wallpapers that are not in the favorites anymore.
deleteUnlikedWallpapers ::
  (MonadIO m, MonadReader env m, HasLog env, HasWallpaperDir env) =>
  LocalWallpapers ->
  [FullWallpaperURL] ->
  m ()
deleteUnlikedWallpapers localWallpapers favURLs = do
  let unliked = unlikedWallpapers favURLs localWallpapers
  wallpaperDir <- asks getWallpaperDir
  unless (null unliked) $ do
    logLn $
      "Following "
        <> show (length unliked)
        <> " wallpapers are not in favorites anymore and will be deleted:\n"
        <> List.intercalate "\n" unliked
    liftIO $ deleteWallpapers wallpaperDir unliked

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
getWallpapersLastPage cid = do
  catch
    ( wallhavenCollectionPageReq cid 1
        >>= http2XXWithRetry
        >>= fromEither
          . first WallhavenMetaParseException
          . API.extractWallhavenMetaLastPage
    )
    (throwIO . Exception.CollectionWallpapersFetchException cid 1)

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
  wallhavenCollectionPageRequest
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
    wallhavenCollectionsRequest
    (asks getWallhavenUsername)
    (asks getWallhavenAPIKey)

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
  catch
    ( wallhavenCollectionsReq
        >>= http2XXWithRetry
        >>= fromEither . parseCollectionID label
    )
    (throwIO . Exception.CollectionsFetchException)

-- | Gets a list of all local wallpapers in the wallpaper directory.
getLocalWallpapers ::
  (MonadReader env m, HasWallpaperDir env, MonadIO m) => m [FilePath]
getLocalWallpapers = asks getWallpaperDir >>= listDirectory

syncWallpaper ::
  ( MonadUnliftIO m,
    MonadReader env m,
    HasRetryConfig env,
    HasWallpaperDir env
  ) =>
  LocalWallpapers ->
  FullWallpaperURL ->
  m ()
syncWallpaper localWallpapers url = do
  let name = wallpaperName url
  path <- asks ((</> name) . getWallpaperDir)
  unless (name `elem` localWallpapers) (downloadResource path url)

downloadResource ::
  ( MonadUnliftIO m,
    MonadReader env m,
    HasRetryConfig env
  ) =>
  FilePath ->
  String ->
  m ()
downloadResource filepath url =
  http2XXWithRetry (HTTP.parseRequest_ url) >>= writeBinaryFile filepath
