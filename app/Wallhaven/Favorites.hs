module Wallhaven.Favorites (downloadAllFavoriteWallpapers) where

import Control.Monad (unless)
import Data.ByteString (ByteString, writeFile)
import qualified Data.ByteString as B8
import qualified Data.ByteString.Char8 as BC8
import Data.List (find)
import Data.Maybe (catMaybes)
import Data.String (IsString)
import Network.HTTP.Simple (Request, addRequestHeader, parseRequest, parseRequest_)
import Retry (retryIO)
import System.FilePath ((</>))
import Text.HTML.TagSoup (fromAttrib, parseTags, (~==))
import Text.Printf (printf)
import Text.StringLike (StringLike)
import Util.Batch (processBatches)
import Util.HTTP (getURL)
import Util.Time (seconds)
import Prelude hiding (writeFile)

favoritesRequest :: Request
favoritesRequest =
  addRequestHeader "Cookie" cookie $
    parseRequest_ "https://wallhaven.cc/favorites"
  where
    cookie = "_pk_ref.1.01b8=%5B%22%22%2C%22%22%2C1670991299%2C%22https%3A%2F%2Fwww.google.com%2F%22%5D; _pk_id.1.01b8=f8b13398227e49a9.1661996195.; cf_clearance=V2JMHapfaF8hLGoMS5ggNx8mHVtxYzVOWlmZCguI5W8-1670981239-0-250; remember_web_59ba36addc2b2f9401580f014c7f58ea4e30989d=eyJpdiI6IndHK09OM0lTWm1wb0lneWx0SndnUGc9PSIsInZhbHVlIjoiZ1RjSDJvQUpXWXpSdnJlTmo3cjNkU2UxQ3Q4K3RtNnNXOTNLTTJ2OThicDJtOXBBZlYwYlpXNkRBVzF5RXR4T0V3ZmFmMEhxZFdVeHlKaEdnSFdyaDlGb25PR0xabCtTSXpabU9vNnpGMVdzKzJrSXBsdkFrb1BlTm5pSU5CWnBPOGVRS09vRllDWTJRSkRKZzV5a2JvQ3NNSFdldGhEZkI3UXFHR0dwc0t0MzNra3pscjVPRlZmSFpcL2NxWFErZCIsIm1hYyI6Ijg2ZWFkNmU2NjA2MTcxY2MwOGNjNTg4MDY0M2E5NjRlMWMzOWNhN2JmZTliYmYwOTI4NzNmOGQ3ZGIzMzVlMmQifQ%3D%3D"

downloadAllFavoriteWallpapers :: IO ()
downloadAllFavoriteWallpapers = do
  errors <- getURL favoritesRequest >>= batchedDownload
  unless (null errors) $ putStrLn $ "Failures: " <> show errors
  where
    batchedDownload =
      fmap catMaybes
        . processBatches 5 (retryIO 5 (seconds 3) . downloadWallhavenWallpaper)
        . fmap BC8.unpack
        . extractFavoriteWallpaperLinks

downloadWallhavenWallpaper :: String -> IO (Maybe String)
downloadWallhavenWallpaper url = do
  maybeLink <- parseRequest url >>= fmap extractWallpaperLink . getURL
  case maybeLink of
    Nothing -> do
      printf "Failed to extract link for %s\n" url
      pure $ Just url
    Just link -> do
      downloadWallpaper link
      pure Nothing

downloadWallpaper :: ByteString -> IO ()
downloadWallpaper wallpaperLink = do
  let wallHavenLink = BC8.unpack $ toFullWallHavenLink wallpaperLink
      name = wallpaperName wallHavenLink
      path = "/Users/home/stuff/wallpapers" </> wallpaperName name
  B8.putStr ("Downloading " <> BC8.pack name <> "\n")
  downloadResource path wallHavenLink

extractWallpaperLink :: (StringLike str, Show str) => str -> Maybe str
extractWallpaperLink =
  fmap (fromAttrib "src")
    . find (~== ("<img id=\"wallpaper\">" :: String))
    . parseTags

toFullWallHavenLink :: (IsString str, Semigroup str) => str -> str
toFullWallHavenLink relativePath =
  "https://w.wallhaven.cc" <> relativePath

downloadResource :: FilePath -> String -> IO ()
downloadResource path url = parseRequest url >>= getURL >>= writeFile path

wallpaperName :: String -> String
wallpaperName = reverse . takeWhile (/= '/') . reverse

extractFavoriteWallpaperLinks :: (Show str, StringLike str) => str -> [str]
extractFavoriteWallpaperLinks =
  fmap (fromAttrib "href")
    . filter (~== ("<a class=\"preview\">" :: String))
    . parseTags
