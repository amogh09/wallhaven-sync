module Wallhaven.Favorites (downloadAllFavoriteWallpapers) where

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

favoritesRequest :: Int -> Request
favoritesRequest page =
  addRequestHeader "Cookie" cookie
    . parseRequest_
    $ "https://wallhaven.cc/favorites?page=" <> show page
  where
    cookie = "_pk_ref.1.01b8=%5B%22%22%2C%22%22%2C1670991299%2C%22https%3A%2F%2Fwww.google.com%2F%22%5D; _pk_id.1.01b8=f8b13398227e49a9.1661996195.; cf_clearance=V2JMHapfaF8hLGoMS5ggNx8mHVtxYzVOWlmZCguI5W8-1670981239-0-250; remember_web_59ba36addc2b2f9401580f014c7f58ea4e30989d=eyJpdiI6IndHK09OM0lTWm1wb0lneWx0SndnUGc9PSIsInZhbHVlIjoiZ1RjSDJvQUpXWXpSdnJlTmo3cjNkU2UxQ3Q4K3RtNnNXOTNLTTJ2OThicDJtOXBBZlYwYlpXNkRBVzF5RXR4T0V3ZmFmMEhxZFdVeHlKaEdnSFdyaDlGb25PR0xabCtTSXpabU9vNnpGMVdzKzJrSXBsdkFrb1BlTm5pSU5CWnBPOGVRS09vRllDWTJRSkRKZzV5a2JvQ3NNSFdldGhEZkI3UXFHR0dwc0t0MzNra3pscjVPRlZmSFpcL2NxWFErZCIsIm1hYyI6Ijg2ZWFkNmU2NjA2MTcxY2MwOGNjNTg4MDY0M2E5NjRlMWMzOWNhN2JmZTliYmYwOTI4NzNmOGQ3ZGIzMzVlMmQifQ%3D%3D"

downloadAllFavoriteWallpapers :: IO ()
downloadAllFavoriteWallpapers = do
  localWallpapers <- loadLocalWallpapers wallpaperDir
  errors <- catMaybes <$> downloadFavoriteWallpapersStartingPage localWallpapers 1
  if null errors
    then putStrLn "All wallpapers were synced successfully."
    else putStrLn "\nFailures: " >> mapM_ putStrLn errors

downloadFavoriteWallpapersStartingPage :: [FilePath] -> Int -> IO [Maybe Error]
downloadFavoriteWallpapersStartingPage localWallpapers page = do
  printf "Starting page %d\n" page
  previewURLs <- getPreviewURLs
  if null previewURLs
    then return []
    else
      (<>)
        <$> batchedDownload previewURLs
        <*> downloadFavoriteWallpapersStartingPage localWallpapers (page + 1)
  where
    batchedDownload :: [PreviewURL] -> IO [Maybe Error]
    batchedDownload =
      processBatches
        5
        (retryIO 5 (seconds 3) isJust . downloadWallpaperFromPreviewURL)
        . filter (not . wallpaperExists localWallpapers)

    getPreviewURLs :: IO [PreviewURL]
    getPreviewURLs =
      fmap BC8.unpack
        . extractFavoriteWallpaperLinks
        <$> getURL (favoritesRequest page)

type PreviewURL = String

type Error = String

wallpaperDir :: String
wallpaperDir = "/Users/home/stuff/wallpapers2"

loadLocalWallpapers :: FilePath -> IO [FilePath]
loadLocalWallpapers = listDirectory

wallpaperExists :: [FilePath] -> PreviewURL -> Bool
wallpaperExists wallpapers url = any contains wallpapers
  where
    contains wallpaper = wallpaperName url `isInfixOf` wallpaper

-- Attempts to download a wallhaven wallpaper from wallpaper preview
-- URL. Returns a Maybe error on any failure and Nothing if download
-- was successful.
downloadWallpaperFromPreviewURL :: PreviewURL -> IO (Maybe Error)
downloadWallpaperFromPreviewURL url = do
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
          path = wallpaperDir </> wallpaperName name
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
