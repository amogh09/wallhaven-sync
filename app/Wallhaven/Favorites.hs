module Wallhaven.Favorites (downloadAllFavoriteWallpapers, Config (..), Error) where

import Data.ByteString (ByteString, writeFile)
import qualified Data.ByteString as B8
import qualified Data.ByteString.Char8 as BC8
import Data.List (find, isInfixOf)
import Data.Maybe (catMaybes, isJust)
import Data.String (IsString)
import Network.HTTP.Simple (Request, Response, addRequestHeader, getResponseBody, getResponseStatus, httpBS, parseRequest, parseRequest_)
import Network.HTTP.Types (Status)
import Network.HTTP.Types.Status (ok200)
import Retry (retryIO)
import System.Directory (doesFileExist, listDirectory)
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
  localWallpapers <- loadLocalWallpapers $ configWallpaperDir config
  catMaybes <$> downloadFavWallpapersFromPage config localWallpapers 1

-- Downloads favorites wallpapers starting from the provided page number.
-- Skips wallpapers that already exist in the wallpapers directory.
downloadFavWallpapersFromPage :: Config -> [FilePath] -> Int -> IO [Maybe Error]
downloadFavWallpapersFromPage config localWallpapers page = do
  printf "Starting page %d\n" page
  previewURLs <- getPreviewURLs
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

    getPreviewURLs :: IO [PreviewURL]
    getPreviewURLs =
      fmap BC8.unpack
        . extractFavoriteWallpaperLinks
        <$> getURL (favoritesRequest config page)

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
  response <- parseRequest url >>= httpBS
  let status = getResponseStatus response
  if status == ok200
    then handle200 response
    else handleNon200 status
  where
    handleNon200 :: Status -> IO (Maybe Error)
    handleNon200 status = do
      printf "Received non-OK response %s for %s\n" (show status) url
      pure $ Just $ url <> " - received non-OK response " <> show status

    handle200 :: Response ByteString -> IO (Maybe Error)
    handle200 response = do
      let contents = getResponseBody response
      case extractFullWallpaperLink contents of
        Nothing -> do
          printf "Failed to extract full wallpaper link from %s" url
          B8.putStr $ contents <> BC8.pack "\n"
          pure $ Just url
        Just link -> do
          downloadWallpaper link
          pure Nothing

    downloadWallpaper :: ByteString -> IO ()
    downloadWallpaper wallpaperLink = do
      let wallHavenLink = BC8.unpack $ toFullWallHavenLink wallpaperLink
          name = wallpaperName wallHavenLink
          path = configWallpaperDir config </> wallpaperName name
      exists <- doesFileExist path
      if exists
        then printf "%s already exists, skipping download" name
        else do
          downloadResource path wallHavenLink
          B8.putStr ("Downloaded " <> BC8.pack name <> "\n")

toFullWallHavenLink :: (IsString str, Semigroup str) => str -> str
toFullWallHavenLink relativePath =
  "https://w.wallhaven.cc" <> relativePath

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
