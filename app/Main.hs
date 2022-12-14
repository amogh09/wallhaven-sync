module Main where

import Data.ByteString (ByteString, writeFile)
import qualified Data.ByteString as B8
import qualified Data.ByteString.Char8 as BC8
import Data.List (find)
import Data.String (IsString)
import Network.HTTP.Simple (Request, addRequestHeader, getResponseBody, httpBS, parseRequest, parseRequest_)
import System.FilePath ((</>))
import Text.HTML.TagSoup (Tag (TagClose, TagOpen), fromAttrib, innerText, parseTags, (~/=), (~==))
import Text.StringLike (StringLike)
import Prelude hiding (writeFile)

main :: IO ()
-- main = downloadWallhavenWallpaper "https://wallhaven.cc/w/zyxvqy"
main =
  getURL favoritesRequest
    >>= mapM_ downloadWallhavenWallpaper
      . fmap byteStringToString
      . extractFavoriteWallpapers

byteStringToString :: ByteString -> String
byteStringToString = BC8.unpack

downloadWallhavenWallpaper :: String -> IO ()
downloadWallhavenWallpaper url =
  parseRequest url
    >>= getURL
    >>= maybe (putStrLn "Failed to get wallpaper link") downloadWallpaper . extractWallpaperLink

downloadWallpaper :: ByteString -> IO ()
downloadWallpaper wallpaperLink = do
  B8.putStr ("Downloading " <> wallpaperLink <> "\n")
  downloadResource . BC8.unpack $ toFullWallHavenLink wallpaperLink

getURL :: Request -> IO ByteString
getURL url = getResponseBody <$> httpBS url

extractWallpaperLink :: (StringLike str, Show str) => str -> Maybe str
extractWallpaperLink =
  fmap (fromAttrib "src") . find (~== ("<img id=\"wallpaper\">" :: String)) . parseTags

toFullWallHavenLink :: (IsString str, Semigroup str) => str -> str
toFullWallHavenLink relativePath = "https://w.wallhaven.cc" <> relativePath

downloadResource :: String -> IO ()
downloadResource url = parseRequest url >>= getURL >>= writeFile (wallpapersDir </> wallpaperName url)

wallpaperName :: String -> String
wallpaperName = reverse . takeWhile (/= '/') . reverse

favoritesRequest :: Request
favoritesRequest =
  addRequestHeader "Cookie" cookie $
    parseRequest_ "https://wallhaven.cc/favorites"
  where
    cookie = "_pk_ref.1.01b8=%5B%22%22%2C%22%22%2C1670991299%2C%22https%3A%2F%2Fwww.google.com%2F%22%5D; _pk_id.1.01b8=f8b13398227e49a9.1661996195.; cf_clearance=V2JMHapfaF8hLGoMS5ggNx8mHVtxYzVOWlmZCguI5W8-1670981239-0-250; remember_web_59ba36addc2b2f9401580f014c7f58ea4e30989d=eyJpdiI6IndHK09OM0lTWm1wb0lneWx0SndnUGc9PSIsInZhbHVlIjoiZ1RjSDJvQUpXWXpSdnJlTmo3cjNkU2UxQ3Q4K3RtNnNXOTNLTTJ2OThicDJtOXBBZlYwYlpXNkRBVzF5RXR4T0V3ZmFmMEhxZFdVeHlKaEdnSFdyaDlGb25PR0xabCtTSXpabU9vNnpGMVdzKzJrSXBsdkFrb1BlTm5pSU5CWnBPOGVRS09vRllDWTJRSkRKZzV5a2JvQ3NNSFdldGhEZkI3UXFHR0dwc0t0MzNra3pscjVPRlZmSFpcL2NxWFErZCIsIm1hYyI6Ijg2ZWFkNmU2NjA2MTcxY2MwOGNjNTg4MDY0M2E5NjRlMWMzOWNhN2JmZTliYmYwOTI4NzNmOGQ3ZGIzMzVlMmQifQ%3D%3D"

extractFavoriteWallpapers :: (Show str, StringLike str) => str -> [str]
extractFavoriteWallpapers =
  fmap (fromAttrib "href") . filter (~== ("<a class=\"preview\">" :: String)) . parseTags

wallpapersDir :: FilePath
wallpapersDir = "/Users/home/stuff/wallpapers"
