module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, wait)
import Control.Monad (join, unless)
import Data.ByteString (ByteString, writeFile)
import qualified Data.ByteString as B8
import qualified Data.ByteString.Char8 as BC8
import Data.List (find)
import Data.Maybe (catMaybes, isJust)
import Data.String (IsString)
import Network.HTTP.Simple (Request, addRequestHeader, getResponseBody, httpBS, parseRequest, parseRequest_)
import System.FilePath ((</>))
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Text.HTML.TagSoup (Tag (TagClose, TagOpen), fromAttrib, innerText, parseTags, (~/=), (~==))
import Text.Printf (printf)
import Text.StringLike (StringLike)
import Prelude hiding (writeFile)

main :: IO ()
main = hSetBuffering stdout NoBuffering >> downloadAllFavoriteWallpapers

downloadAllFavoriteWallpapers :: IO ()
downloadAllFavoriteWallpapers = do
  favoritesPage <- getURL favoritesRequest
  errors <-
    fmap catMaybes
      . processBatches 5 (retryIO 5 (seconds 3) . downloadWallhavenWallpaper)
      . fmap BC8.unpack
      . extractFavoriteWallpaperLinks
      $ favoritesPage
  unless (null errors) $ putStrLn $ "Errors: " <> show errors

retryIO :: Int -> Int -> IO (Maybe a) -> IO (Maybe a)
retryIO 1 _ a = a
retryIO !attempts delay a = do
  res <- a
  case res of
    Just err -> threadDelay delay >> retryIO (attempts - 1) delay a
    Nothing -> pure res

seconds :: Int -> Int
seconds n = n * 10 ^ 6

batches :: Int -> [a] -> [[a]]
batches _ [] = []
batches n xs = take n xs : batches n (drop n xs)

processBatches :: Show a => Int -> (a -> IO b) -> [a] -> IO [b]
processBatches n f = fmap join . mapM (processBatch f) . batches n

processBatch :: Show a => (a -> IO b) -> [a] -> IO [b]
processBatch f xs = do
  -- printf "\nStarting to process a batch of %d items\n%s\n" (length xs) (show xs)
  mapM (async . f) xs >>= mapM wait

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

getURL :: Request -> IO ByteString
getURL url = getResponseBody <$> httpBS url

extractWallpaperLink :: (StringLike str, Show str) => str -> Maybe str
extractWallpaperLink =
  fmap (fromAttrib "src") . find (~== ("<img id=\"wallpaper\">" :: String)) . parseTags

toFullWallHavenLink :: (IsString str, Semigroup str) => str -> str
toFullWallHavenLink relativePath = "https://w.wallhaven.cc" <> relativePath

downloadResource :: FilePath -> String -> IO ()
downloadResource path url = parseRequest url >>= getURL >>= writeFile path

wallpaperName :: String -> String
wallpaperName = reverse . takeWhile (/= '/') . reverse

favoritesRequest :: Request
favoritesRequest =
  addRequestHeader "Cookie" cookie $
    parseRequest_ "https://wallhaven.cc/favorites"
  where
    cookie = "_pk_ref.1.01b8=%5B%22%22%2C%22%22%2C1670991299%2C%22https%3A%2F%2Fwww.google.com%2F%22%5D; _pk_id.1.01b8=f8b13398227e49a9.1661996195.; cf_clearance=V2JMHapfaF8hLGoMS5ggNx8mHVtxYzVOWlmZCguI5W8-1670981239-0-250; remember_web_59ba36addc2b2f9401580f014c7f58ea4e30989d=eyJpdiI6IndHK09OM0lTWm1wb0lneWx0SndnUGc9PSIsInZhbHVlIjoiZ1RjSDJvQUpXWXpSdnJlTmo3cjNkU2UxQ3Q4K3RtNnNXOTNLTTJ2OThicDJtOXBBZlYwYlpXNkRBVzF5RXR4T0V3ZmFmMEhxZFdVeHlKaEdnSFdyaDlGb25PR0xabCtTSXpabU9vNnpGMVdzKzJrSXBsdkFrb1BlTm5pSU5CWnBPOGVRS09vRllDWTJRSkRKZzV5a2JvQ3NNSFdldGhEZkI3UXFHR0dwc0t0MzNra3pscjVPRlZmSFpcL2NxWFErZCIsIm1hYyI6Ijg2ZWFkNmU2NjA2MTcxY2MwOGNjNTg4MDY0M2E5NjRlMWMzOWNhN2JmZTliYmYwOTI4NzNmOGQ3ZGIzMzVlMmQifQ%3D%3D"

extractFavoriteWallpaperLinks :: (Show str, StringLike str) => str -> [str]
extractFavoriteWallpaperLinks =
  fmap (fromAttrib "href") . filter (~== ("<a class=\"preview\">" :: String)) . parseTags
