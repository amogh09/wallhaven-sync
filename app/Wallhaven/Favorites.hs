module Wallhaven.Favorites (Config (..), SyncError) where

import Control.Exception (catchJust)
import qualified Control.Monad.Catch as MC
import Control.Monad.Except (ExceptT, MonadError, mapError, modifyError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT, reader)
import Data.ByteString (ByteString, writeFile)
import qualified Data.ByteString as B8
import qualified Data.ByteString.Char8 as BC8
import Data.List (find, isInfixOf)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Data.String (IsString)
import Network.HTTP.Client.Conduit (HttpException (HttpExceptionRequest), HttpExceptionContent)
import Network.HTTP.Simple (Request, Response, addRequestHeader, getResponseBody, getResponseStatus, httpBS, parseRequest, parseRequestThrow, parseRequestThrow_, parseRequest_)
import Network.HTTP.Types (tooManyRequests429)
import Network.HTTP.Types.Status (ok200)
import Retry (MaxAttempts, RetryDelayMicros, retryIO)
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.FilePath ((</>))
import Text.HTML.TagSoup (fromAttrib, parseTags, (~==))
import Text.Printf (printf)
import Text.StringLike (StringLike)
import Util.Batch (processBatches)
import Util.HTTP (httpBSWithRetryAndErrorHandling)
import Util.Time (seconds)
import Prelude hiding (writeFile)

data SyncError
  = FavoritesFetchError Page HttpException
  | WallpaperSyncError [WallpaperError]

data WallpaperError
  = PreviewFetchError WallpaperName HttpException
  | FullWallpaperDownloadError WallpaperName HttpException
  | FullWallpaperURLParseError WallpaperName FullWallpaperURL

type FavoritePageURL = String

type FullWallpaperURL = String

type PreviewURL = String

type WallpaperName = String

type NumParallelDownloads = Int

type AuthCookie = ByteString

type Page = Int

type RetryDelaySeconds = Int

-- Configuration structure.
data Config = Config
  { -- | The directory where the wallpapers will be saved.
    configWallpaperDir :: FilePath,
    -- | The number of wallpapers to download in parallel.
    configNumParallelDownloads :: NumParallelDownloads,
    -- | The number of retries to perform when downloading a wallpaper.
    configNumRetries :: MaxAttempts,
    -- | The number of seconds to wait between retries.
    configRetryDelay :: RetryDelaySeconds,
    -- | Cookie to use for authentication.
    configCookie :: AuthCookie
  }

newtype App e m a = App {runApp :: ExceptT e (ReaderT Config m) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadError e,
      MonadReader Config,
      MonadIO
    )

type WallhavenApp = App SyncError IO

-- | Syncs all wallpapers from the Wallhaven favorites page.
syncAllFavoriteWallpapers :: WallhavenApp ()
syncAllFavoriteWallpapers = undefined

-- downloadWallpapersFromPreviews :: [PreviewURL] -> m ()
-- downloadWallpapersFromPreviews = do
--   config <- ask
--   processBatches (configNumParallelDownloads config) downloadWallpaperFromPreviewURL

getFavoritePreviews :: Page -> WallhavenApp [PreviewURL]
getFavoritePreviews page =
  fmap BC8.unpack . parseFavoritePreviews <$> getFavoritesPage page

getFavoritesPage ::
  (MonadReader Config m, MonadError SyncError m, MonadIO m) => Page -> m ByteString
getFavoritesPage page =
  favoritesRequestReader page >>= httpBSApp (FavoritesFetchError page)

favoritesRequestReader :: MonadReader Config m => Page -> m Request
favoritesRequestReader page = reader (`favoritesRequest` page)

favoritesRequest :: Config -> Page -> Request
favoritesRequest config page =
  addRequestHeader "Cookie" (configCookie config)
    . parseRequest_
    $ "https://wallhaven.cc/favorites?page=" <> show page

-- Extracts a list of favorites wallpapers from Wallhaven's favorites page.
parseFavoritePreviews :: (Show str, StringLike str) => str -> [str]
parseFavoritePreviews =
  fmap (fromAttrib "href")
    . filter (~== ("<a class=\"preview\">" :: String))
    . parseTags

httpBSApp ::
  (MonadError e m, MonadReader Config m, MonadIO m) =>
  (HttpException -> e) ->
  Request ->
  m ByteString
httpBSApp errmap req = do
  config <- ask
  modifyError errmap $
    httpBSWithRetryAndErrorHandling
      (configNumRetries config)
      (seconds $ configRetryDelay config)
      req

-- Parses full wallpaper link from a preview page.
parseFullWallpaperURL :: (Show str, StringLike str) => str -> Maybe str
parseFullWallpaperURL =
  fmap (fromAttrib "href")
    . find (~== ("<a class=\"full\" href=\"#\">" :: String))
    . parseTags

downloadWallpaperFromFullURL ::
  (MonadError WallpaperError m, MonadReader Config m, MonadIO m) =>
  FullWallpaperURL ->
  m ()
downloadWallpaperFromFullURL url = do
  let name = wallpaperNameFromURL url
  path <- wallpaperPathFromURL name
  contents <- httpBSApp (FullWallpaperDownloadError name) (parseRequest_ url)
  liftIO $ B8.writeFile path contents

wallpaperNameFromURL :: FullWallpaperURL -> WallpaperName
wallpaperNameFromURL fullWallpaperURL = last $ splitOn "/" fullWallpaperURL

wallpaperPathFromURL :: MonadReader Config m => WallpaperName -> m FilePath
wallpaperPathFromURL name =
  reader (\config -> configWallpaperDir config </> name)

downloadWallpaperFromPreviewURL ::
  (MonadError WallpaperError m, MonadReader Config m, MonadIO m) =>
  PreviewURL ->
  m ()
downloadWallpaperFromPreviewURL previewURL = do
  let name = wallpaperNameFromURL previewURL
  httpBSApp (PreviewFetchError name) (parseRequestThrow_ previewURL)
    >>= maybe (throwError $ FullWallpaperURLParseError name previewURL) pure
      . parseFullWallpaperURL
    >>= downloadWallpaperFromFullURL . BC8.unpack

-- downloadAllFavoriteWallpapers :: Config -> IO [Error]
-- downloadAllFavoriteWallpapers config = do
--   createDirectoryIfMissing True $ configWallpaperDir config
--   localWallpapers <- loadLocalWallpapers $ configWallpaperDir config
--   catMaybes <$> downloadFavWallpapersFromPage config localWallpapers 1

-- -- Downloads favorites wallpapers starting from the provided page number.
-- -- Skips wallpapers that already exist in the wallpapers directory.
-- downloadFavWallpapersFromPage :: Config -> [FilePath] -> Int -> IO [Maybe Error]
-- downloadFavWallpapersFromPage config localWallpapers page = do
--   printf "Starting page %d\n" page
--   previewURLsOrErr <- getPreviewURLs config page
--   case previewURLsOrErr of
--     Left err -> pure [Just err]
--     Right previewURLs ->
--       if null previewURLs
--         then return []
--         else
--           _
--             <$> batchedDownload previewURLs
--             <*> downloadFavWallpapersFromPage config localWallpapers (page + 1)
--   where
--     batchedDownload :: [PreviewURL] -> IO [Maybe Error]
--     batchedDownload =
--       processBatches
--         (configNumParallelDownloads config)
--         (downloadWallpaperFromPreviewURL config)
--         . filter (not . wallpaperExists localWallpapers)

-- -- Get preview URLs of all favorite wallpapers.
-- getPreviewURLs :: Config -> Int -> IO (Either Error [PreviewURL])
-- getPreviewURLs config page =
--   catchJust
--     httpExceptionRequest
--     (parsePreviewURLs <$> httpBSWithRetry config (favoritesRequest config page))
--     (\err -> pure . Left $ "Failed to get favorites page: " <> show err)
--   where
--     httpExceptionRequest :: HttpException -> Maybe HttpExceptionContent
--     httpExceptionRequest (HttpExceptionRequest _ content) = Just content
--     httpExceptionRequest _ = Nothing

--     parsePreviewURLs :: Response ByteString -> Either Error [PreviewURL]
--     parsePreviewURLs response = do
--       let status = getResponseStatus response
--       if status == ok200
--         then pure . fmap BC8.unpack . extractFavoriteWallpaperLinks $ getResponseBody response
--         else
--           Left $
--             "Received non-OK response when getting favorites page "
--               <> show page
--               <> ": "
--               <> show status

-- httpBSWithRetry :: Config -> Request -> IO (Response ByteString)
-- httpBSWithRetry cfg =
--   retryIO
--     (configNumRetries cfg)
--     (seconds $ configRetryDelay cfg)
--     ((== tooManyRequests429) . getResponseStatus)
--     . httpBS

-- loadLocalWallpapers :: FilePath -> IO [FilePath]
-- loadLocalWallpapers = listDirectory

-- wallpaperExists :: [FilePath] -> PreviewURL -> Bool
-- wallpaperExists wallpapers url = any contains wallpapers
--   where
--     contains wallpaper = wallpaperName url `isInfixOf` wallpaper

-- -- Attempts to download a wallhaven wallpaper from wallpaper preview
-- -- URL. Returns a Maybe error on any failure and Nothing if download
-- -- was successful.
-- downloadWallpaperFromPreviewURL :: Config -> PreviewURL -> IO (Maybe Error)
-- downloadWallpaperFromPreviewURL config url =
--   parseRequest url
--     >>= fmap fullLinkFromPreviewResponse . httpBSWithRetry config
--     >>= either
--       (pure . Just)
--       (\link -> downloadWallpaper config (wallpaperPath link) link >> pure Nothing)
--   where
--     fullLinkFromPreviewResponse :: Response ByteString -> Either Error String
--     fullLinkFromPreviewResponse response = do
--       let status = getResponseStatus response
--       if status == ok200
--         then
--           maybe
--             (Left "Failed to extract full wallpaper link from preview URL")
--             (Right . BC8.unpack . fullWallpaperLink)
--             . extractFullWallpaperLink
--             $ getResponseBody response
--         else Left $ "Received non-OK response when getting preview page: " <> show status

--     wallpaperPath :: String -> FilePath
--     wallpaperPath link = configWallpaperDir config </> wallpaperName link

--     fullWallpaperLink :: (IsString str, Semigroup str) => str -> str
--     fullWallpaperLink relativePath = "https://w.wallhaven.cc" <> relativePath

-- downloadWallpaper :: Config -> FilePath -> String -> IO ()
-- downloadWallpaper config path link = do
--   let name = wallpaperName link
--   exists <- doesFileExist path
--   if exists
--     then printf "%s already exists, skipping download" name
--     else do
--       downloadResource link
--       B8.putStr $ "Downloaded " <> BC8.pack name <> "\n"
--   where
--     downloadResource :: String -> IO ()
--     downloadResource url =
--       parseRequest url >>= httpBSWithRetry config >>= writeFile path . getResponseBody

-- wallpaperName :: String -> String
-- wallpaperName = reverse . takeWhile (/= '/') . reverse

-- -- Extracts a list of favorites wallpapers from Wallhaven's favorites page
-- extractFavoriteWallpaperLinks :: (Show str, StringLike str) => str -> [str]
-- extractFavoriteWallpaperLinks =
--   fmap (fromAttrib "href")
--     . filter (~== ("<a class=\"preview\">" :: String))
--     . parseTags

-- -- Extracts a link to the full wallpaper from wallpaper preview page
-- extractFullWallpaperLink :: (StringLike str, Show str) => str -> Maybe str
-- extractFullWallpaperLink =
--   fmap (fromAttrib "src")
--     . find (~== ("<img id=\"wallpaper\">" :: String))
--     . parseTags
