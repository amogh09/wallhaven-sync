-- Provides functions that implement Wallhaven interaction interface to be used when
-- constructing environments.
module Wallhaven.Implementation.API (getCollectionURLs, downloadWallpaper) where

import Data.ByteString (ByteString)
import qualified Network.HTTP.Client.Conduit as HTTP
import qualified Network.HTTP.Simple as HTTP
import Network.HTTP.Types.Status (notFound404, unauthorized401)
import UnliftIO (MonadUnliftIO, catch, throwIO)
import qualified Wallhaven.API.Action as WallhavenAPI
import Wallhaven.API.Exception (CollectionURLsFetchException (..))
import Wallhaven.API.Types (APIKey)
import Wallhaven.Exception
  ( WallhavenSyncException (CollectionFetchException, WallpaperDownloadException),
  )
import Wallhaven.Types (FullWallpaperURL, Label, Username)

getCollectionURLs ::
  (MonadUnliftIO m) => APIKey -> Username -> Label -> m [FullWallpaperURL]
getCollectionURLs apiKey username label =
  catch
    (WallhavenAPI.getAllCollectionURLs apiKey username label)
    (throwIO . collectionFetchExceptionHandler username label)

collectionFetchExceptionHandler ::
  Username -> Label -> CollectionURLsFetchException -> WallhavenSyncException
collectionFetchExceptionHandler
  username
  label
  ( UserCollectionsHTTPException
      (HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException res _))
    )
    | HTTP.getResponseStatus res == unauthorized401 =
        let oneLine =
              "not authorized to access user collections,"
                <> " is your API key valid?"
            verbose = show res
         in CollectionFetchException username label oneLine verbose
    | HTTP.getResponseStatus res == notFound404 =
        let oneLine =
              "no collections found for user "
                <> username
                <> ","
                <> " is the username valid?"
            verbose = show res
         in CollectionFetchException username label oneLine verbose
collectionFetchExceptionHandler
  username
  label
  (UserCollectionsHTTPException e) =
    let oneLine = "HTTP request to list user collections failed"
     in CollectionFetchException username label oneLine (show e)
collectionFetchExceptionHandler
  username
  label
  (UserCollectionsParseException jsonErr) =
    let oneLine = "failed to parse user collections API response"
     in CollectionFetchException username label oneLine jsonErr
collectionFetchExceptionHandler
  username
  label
  (CollectionNotFoundException _) =
    let oneLine = "collection " <> label <> " was not found"
     in CollectionFetchException username label oneLine oneLine
collectionFetchExceptionHandler
  username
  label
  (CollectionFetchHTTPException cid page e) =
    let oneLine =
          "HTTP request to fetch page "
            <> show page
            <> " of collection with ID "
            <> show cid
            <> " failed"
     in CollectionFetchException username label oneLine (show e)
collectionFetchExceptionHandler
  username
  label
  (MetaParseException cid jsonErr) =
    let oneLine =
          "failed to parse collection metadata from API response"
            <> " for collection with ID "
            <> show cid
     in CollectionFetchException username label oneLine jsonErr
collectionFetchExceptionHandler
  username
  label
  (WallpaperURLsParseException jsonErr) =
    let oneLine = "failed to parse wallpaper URLs from API response"
     in CollectionFetchException username label oneLine jsonErr

downloadWallpaper ::
  (MonadUnliftIO m) => FullWallpaperURL -> m ByteString
downloadWallpaper url =
  catch
    (WallhavenAPI.getFullWallpaper url)
    (throwIO . wallpaperDownloadExceptionHandler url)

wallpaperDownloadExceptionHandler ::
  FullWallpaperURL -> HTTP.HttpException -> WallhavenSyncException
wallpaperDownloadExceptionHandler url e =
  let oneLine = "HTTP request to download wallpaper failed"
   in WallpaperDownloadException url oneLine (show e)
