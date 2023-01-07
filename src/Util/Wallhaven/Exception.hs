module Util.Wallhaven.Exception
  ( WallpaperSyncException (..),
    displayException,
    displayExceptionVerbose,
  )
where

import qualified Data.ByteString.Char8 as BC8
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types as HTTP
import Types
import UnliftIO (Exception, Typeable, displayException)

data WallpaperSyncException
  = WallpaperDownloadException FullWallpaperURL HTTP.HttpException
  | CollectionsFetchException HTTP.HttpException
  | CollectionsParseException String
  | CollectionNotFoundException Label
  | CollectionWallpapersFetchException CollectionID Page HTTP.HttpException
  | WallpapersParseException String
  | WallhavenMetaParseException String
  deriving (Typeable, Show)

displayExceptionVerbose :: WallpaperSyncException -> String
displayExceptionVerbose (WallpaperDownloadException url e) =
  url
    <> ": "
    <> displayHTTPException e
    <> "\n"
    <> "Verbose exception: "
    <> displayException e
displayExceptionVerbose (CollectionsFetchException e) =
  "Failed to fetch collections: "
    <> displayHTTPException e
    <> "\n"
    <> "Verbose exception: "
    <> displayException e
displayExceptionVerbose (CollectionsParseException e) =
  "Failed to parse API response to collections: " <> e
displayExceptionVerbose (CollectionNotFoundException label) =
  "Collection with label " <> label <> " was not found"
displayExceptionVerbose (CollectionWallpapersFetchException collectionID page e) =
  "Failed to fetch wallpapers for collection "
    <> show collectionID
    <> " page "
    <> show page
    <> ": "
    <> displayHTTPException e
    <> "\n"
    <> "Verbose exception: "
    <> displayException e
displayExceptionVerbose (WallpapersParseException e) =
  "Failed to parse wallpapers response: " <> e
displayExceptionVerbose (WallhavenMetaParseException e) =
  "Failed to parse meta information from wallpapers response: " <> e

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
displayHTTPException (HTTP.HttpExceptionRequest _ (HTTP.ConnectionFailure _)) =
  "Failed to connect to Wallhaven"
displayHTTPException e = displayException e
