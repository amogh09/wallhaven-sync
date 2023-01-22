-- Provides functions that implement Wallhaven Database interface to be used when
-- constructing environments.
module Wallhaven.Implementation.FileSystemDB
  ( deleteWallpaper,
    saveWallpaper,
    initDB,
    getDownloadedWallpapers,
  )
where

import System.FilePath ((</>))
import System.IO.Error (isPermissionError)
import UnliftIO
import UnliftIO.Directory
import UnliftIO.IO.File
import Util.FileSystem (deleteFileIfExists)
import Wallhaven.Exception
import Wallhaven.Types (Wallpaper, WallpaperName)

type WallpaperDir = FilePath

deleteWallpaper :: (MonadUnliftIO m) => WallpaperDir -> WallpaperName -> m ()
deleteWallpaper dir name =
  catch
    (deleteFileIfExists $ dir </> name)
    (throwIO . deleteWallpaperExceptionHandler dir name)

deleteWallpaperExceptionHandler ::
  WallpaperDir -> WallpaperName -> IOError -> WallhavenSyncException
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

saveWallpaper ::
  (MonadUnliftIO m) => WallpaperDir -> WallpaperName -> Wallpaper -> m ()
saveWallpaper dir name wallpaper = do
  catch
    (writeBinaryFile (dir </> name) wallpaper)
    (throwIO . saveWallpaperExceptionHandler dir name)

saveWallpaperExceptionHandler ::
  WallpaperDir -> WallpaperName -> IOError -> WallhavenSyncException
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

initDB :: (MonadUnliftIO m) => WallpaperDir -> m ()
initDB dir =
  catch
    (createDirectoryIfMissing True dir)
    (throwIO . initDBExceptionHandler dir)

initDBExceptionHandler :: WallpaperDir -> IOError -> WallhavenSyncException
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

getDownloadedWallpapers :: (MonadUnliftIO m) => WallpaperDir -> m [WallpaperName]
getDownloadedWallpapers dir =
  catch
    (listDirectory dir)
    (throwIO . getDownloadedWallpapersExceptionHandler dir)

getDownloadedWallpapersExceptionHandler ::
  WallpaperDir -> IOError -> WallhavenSyncException
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
