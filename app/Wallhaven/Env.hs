module Wallhaven.Env (Env (..), Config (..)) where

import Control.Monad.Reader (ReaderT, asks)
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Simple as HTTP
import Network.HTTP.Types (unauthorized401)
import Network.HTTP.Types.Status (notFound404)
import System.FilePath ((</>))
import System.IO.Error (isPermissionError)
import Types (FullWallpaperURL, Label, Username, WallpaperName)
import qualified Types
import UnliftIO (MonadUnliftIO, catch, throwIO)
import UnliftIO.Directory (createDirectoryIfMissing, listDirectory)
import UnliftIO.IO.File (writeBinaryFile)
import Util.FileSystem (deleteFileIfExists)
import qualified Wallhaven.API.Action as WallhavenAPI
import Wallhaven.API.Exception (CollectionURLsFetchException (..))
import Wallhaven.Exception (WallhavenSyncException (..))
import Wallhaven.Monad
import Prelude hiding (log)

data Env = Env
  { envConfig :: !Config,
    envLog :: !(String -> IO ())
  }

-- Configuration structure.
data Config = Config
  { -- | The directory where the wallpapers will be saved.
    configWallpaperDir :: FilePath,
    -- | The number of wallpapers to download in parallel.
    configNumParallelDownloads :: Types.NumParallelDownloads,
    -- | Whether to delete unliked wallpapers.
    configDeleteUnliked :: Bool,
    -- | Wallhaven API Key
    configWallhavenAPIKey :: String,
    -- | Debug mode
    configDebug :: Bool
  }

instance HasDebug Env where
  getDebug = configDebug . envConfig

instance HasDeleteUnliked Env where
  getDeleteUnliked = configDeleteUnliked . envConfig

instance HasLog Env where
  getLog = envLog

instance HasSyncParallelization Env where
  getSyncParallelization = configNumParallelDownloads . envConfig

instance (MonadUnliftIO m) => MonadGetCollectionURLs (ReaderT Env m) where
  getCollectionURLs username label = do
    apiKey <- asks (configWallhavenAPIKey . envConfig)
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

instance MonadUnliftIO m => MonadDownloadWallpaper (ReaderT Env m) where
  downloadWallpaper url =
    catch
      (WallhavenAPI.getFullWallpaper url)
      (throwIO . wallpaperDownloadExceptionHandler url)

wallpaperDownloadExceptionHandler ::
  FullWallpaperURL -> HTTP.HttpException -> WallhavenSyncException
wallpaperDownloadExceptionHandler url e =
  let oneLine = "HTTP request to download wallpaper failed"
   in WallpaperDownloadException url oneLine (show e)

instance (MonadUnliftIO m) => MonadDeleteWallpaper (ReaderT Env m) where
  deleteWallpaper name = do
    dir <- asks (configWallpaperDir . envConfig)
    catch
      (deleteFileIfExists $ dir </> name)
      (throwIO . deleteWallpaperExceptionHandler dir name)

deleteWallpaperExceptionHandler ::
  FilePath -> WallpaperName -> IOError -> WallhavenSyncException
deleteWallpaperExceptionHandler dir name e
  | isPermissionError e =
      let oneLine =
            "no permission to delete wallpaper file " <> (dir </> name)
          verbose = show e
       in DeleteWallpaperException name oneLine verbose
  | otherwise =
      DeleteWallpaperException
        name
        ("failed to delete wallpaper file " <> (dir </> name))
        (show e)

instance (MonadUnliftIO m) => MonadSaveWallpaper (ReaderT Env m) where
  saveWallpaper name wallpaper = do
    dir <- asks (configWallpaperDir . envConfig)
    catch
      (writeBinaryFile (dir </> name) wallpaper)
      (throwIO . saveWallpaperExceptionHandler dir name)

saveWallpaperExceptionHandler ::
  FilePath -> WallpaperName -> IOError -> WallhavenSyncException
saveWallpaperExceptionHandler dir name e
  | isPermissionError e =
      let oneLine =
            "no permission to save wallpaper to file " <> (dir </> name)
          verbose = show e
       in SaveWallpaperException name oneLine verbose
  | otherwise =
      SaveWallpaperException
        name
        ("failed to save wallpaper to file " <> (dir </> name))
        (show e)

instance (MonadUnliftIO m) => MonadInitDB (ReaderT Env m) where
  initDB = do
    dir <- asks (configWallpaperDir . envConfig)
    catch
      (createDirectoryIfMissing True dir)
      (throwIO . initDBExceptionHandler dir)

initDBExceptionHandler :: FilePath -> IOError -> WallhavenSyncException
initDBExceptionHandler dir e
  | isPermissionError e =
      let oneLine =
            "no permission to create wallpaper directory '"
              <> dir
              <> "'"
          verbose = show e
       in InitDBException oneLine verbose
  | otherwise =
      InitDBException
        ("failed to create wallpaper directory '" <> dir <> "'")
        (show e)

instance
  (MonadUnliftIO m) =>
  MonadGetDownloadedWallpapers (ReaderT Env m)
  where
  getDownloadedWallpapers = do
    dir <- asks (configWallpaperDir . envConfig)
    catch
      (listDirectory dir)
      (throwIO . getDownloadedWallpapersExceptionHandler dir)

getDownloadedWallpapersExceptionHandler ::
  FilePath -> IOError -> WallhavenSyncException
getDownloadedWallpapersExceptionHandler dir e
  | isPermissionError e =
      let oneLine =
            "no permission to list contents of wallpaper directory '"
              <> dir
              <> "'"
          verbose = show e
       in InitDBException oneLine verbose
  | otherwise =
      InitDBException
        ("failed to list contents of wallpaper directory '" <> dir <> "'")
        (show e)
