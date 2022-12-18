module Wallhaven.Favorites (downloadAllFavoriteWallpapers, Config (..), Error) where

import Control.Exception (catchJust)
import Data.ByteString (ByteString, writeFile)
import qualified Data.ByteString as B8
import qualified Data.ByteString.Char8 as BC8
import Data.List (find, isInfixOf)
import Data.Maybe (catMaybes, isJust)
import Data.String (IsString)
import Network.HTTP.Client.Conduit (HttpException (HttpExceptionRequest), HttpExceptionContent)
import Network.HTTP.Simple (Request, Response, addRequestHeader, getResponseBody, getResponseStatus, httpBS, parseRequest, parseRequest_)
import Network.HTTP.Types.Status (ok200)
import Retry (retryIO)
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.FilePath ((</>))
import Text.HTML.TagSoup (fromAttrib, parseTags, (~==))
import Text.Printf (printf)
import Text.StringLike (StringLike)
import Util.Batch (processBatches)
import Util.HTTP (getURL)
import Util.Time (seconds)
import Prelude hiding (writeFile)

-- Configuration structure.
data Config = Config
  { -- | The directory where the wallpapers will be saved.
    configWallpaperDir :: FilePath,
    -- | The number of wallpapers to download in parallel.
    configNumParallelDownloads :: Int,
    -- | The number of retries to perform when downloading a wallpaper.
    configNumRetries :: Int,
    -- | The number of seconds to wait between retries.
    configRetryDelay :: Int,
    -- | Cookie to use for authentication.
    configCookie :: ByteString
  }

type Error = String

favoritesRequest :: Config -> Int -> Request
favoritesRequest config page =
  addRequestHeader "Cookie" (configCookie config)
    . parseRequest_
    $ "https://wallhaven.cc/favorites?page=" <> show page

downloadAllFavoriteWallpapers :: Config -> IO [Error]
downloadAllFavoriteWallpapers config = do
  createDirectoryIfMissing True $ configWallpaperDir config
  localWallpapers <- loadLocalWallpapers $ configWallpaperDir config
  catMaybes <$> downloadFavWallpapersFromPage config localWallpapers 1

-- Downloads favorites wallpapers starting from the provided page number.
-- Skips wallpapers that already exist in the wallpapers directory.
downloadFavWallpapersFromPage :: Config -> [FilePath] -> Int -> IO [Maybe Error]
downloadFavWallpapersFromPage config localWallpapers page = do
  printf "Starting page %d\n" page
  previewURLsOrErr <- getPreviewURLs config page
  case previewURLsOrErr of
    Left err -> pure [Just err]
    Right previewURLs ->
      if null previewURLs
        then return []
        else
          (<>)
            <$> batchedDownload previewURLs
            <*> downloadFavWallpapersFromPage config localWallpapers (page + 1)
  where
    batchedDownload :: [PreviewURL] -> IO [Maybe Error]
    batchedDownload =
      processBatches
        (configNumParallelDownloads config)
        ( retryIO (configNumRetries config) (seconds $ configRetryDelay config) isJust
            . downloadWallpaperFromPreviewURL config
        )
        . filter (not . wallpaperExists localWallpapers)

-- Get preview URLs of all favorite wallpapers.
getPreviewURLs :: Config -> Int -> IO (Either Error [PreviewURL])
getPreviewURLs config page =
  catchJust
    httpExceptionRequest
    (parsePreviewURLs <$> httpBS (favoritesRequest config page))
    (\err -> pure . Left $ "Failed to get favorites page: " <> show err)
  where
    httpExceptionRequest :: HttpException -> Maybe HttpExceptionContent
    httpExceptionRequest (HttpExceptionRequest _ content) = Just content
    httpExceptionRequest _ = Nothing

    parsePreviewURLs :: Response ByteString -> Either Error [PreviewURL]
    parsePreviewURLs response = do
      let status = getResponseStatus response
      if status == ok200
        then pure . fmap BC8.unpack . extractFavoriteWallpaperLinks $ getResponseBody response
        else error $ "Received non-OK response when getting favorites page: " <> show status

type PreviewURL = String

loadLocalWallpapers :: FilePath -> IO [FilePath]
loadLocalWallpapers = listDirectory

wallpaperExists :: [FilePath] -> PreviewURL -> Bool
wallpaperExists wallpapers url = any contains wallpapers
  where
    contains wallpaper = wallpaperName url `isInfixOf` wallpaper

-- Attempts to download a wallhaven wallpaper from wallpaper preview
-- URL. Returns a Maybe error on any failure and Nothing if download
-- was successful.
downloadWallpaperFromPreviewURL :: Config -> PreviewURL -> IO (Maybe Error)
downloadWallpaperFromPreviewURL config url = do
  parseRequest url
    >>= fmap fullLinkFromPreviewResponse . httpBS
    >>= either
      (pure . Just)
      (\link -> downloadWallpaper (wallpaperPath link) (fullWallpaperLink link) >> pure Nothing)
  where
    fullLinkFromPreviewResponse :: Response ByteString -> Either Error String
    fullLinkFromPreviewResponse response = do
      let status = getResponseStatus response
      if status == ok200
        then
          maybe (Left "Failed to extract full wallpaper link from preview URL") (Right . BC8.unpack)
            . extractFullWallpaperLink
            $ getResponseBody response
        else Left $ "Received non-OK response when getting preview page: " <> show status

    wallpaperPath :: String -> FilePath
    wallpaperPath link = configWallpaperDir config </> wallpaperName link

    downloadWallpaper :: String -> String -> IO ()
    downloadWallpaper path link = do
      let name = wallpaperName link
      exists <- doesFileExist path
      if exists
        then printf "%s already exists, skipping download" name
        else do
          downloadResource path link
          B8.putStr ("Downloaded " <> BC8.pack name <> "\n")

    fullWallpaperLink :: (IsString str, Semigroup str) => str -> str
    fullWallpaperLink relativePath = "https://w.wallhaven.cc" <> relativePath

downloadResource :: FilePath -> String -> IO ()
downloadResource path url = parseRequest url >>= getURL >>= writeFile path

wallpaperName :: String -> String
wallpaperName = reverse . takeWhile (/= '/') . reverse

-- Extracts a list of favorites wallpapers from Wallhaven's favorites page
extractFavoriteWallpaperLinks :: (Show str, StringLike str) => str -> [str]
extractFavoriteWallpaperLinks =
  fmap (fromAttrib "href")
    . filter (~== ("<a class=\"preview\">" :: String))
    . parseTags

-- Extracts a link to the full wallpaper from wallpaper preview page
extractFullWallpaperLink :: (StringLike str, Show str) => str -> Maybe str
extractFullWallpaperLink =
  fmap (fromAttrib "src")
    . find (~== ("<img id=\"wallpaper\">" :: String))
    . parseTags
