module Database.FileSystem.Action
  ( deleteWallpaper,
    saveWallpaper,
    getWallpaperNames,
    createWallpaperDir,
  )
where

import Control.Monad (when)
import Control.Monad.Reader (liftIO)
import System.FilePath ((</>))
import Types (Wallpaper, WallpaperName)
import UnliftIO (MonadIO)
import UnliftIO.Directory
  ( createDirectoryIfMissing,
    doesFileExist,
    listDirectory,
    removeFile,
  )
import UnliftIO.IO.File (writeBinaryFile)

type WallpaperDir = FilePath

createWallpaperDir :: MonadIO m => WallpaperDir -> m ()
createWallpaperDir = liftIO . createDirectoryIfMissing True

deleteWallpaper :: MonadIO m => WallpaperDir -> WallpaperName -> m ()
deleteWallpaper dir name = do
  let path = dir </> name
  exists <- doesFileExist path
  when exists $ removeFile path

getWallpaperNames :: MonadIO m => WallpaperDir -> m [WallpaperName]
getWallpaperNames = listDirectory

saveWallpaper ::
  MonadIO m => WallpaperDir -> WallpaperName -> Wallpaper -> m ()
saveWallpaper dir name = writeBinaryFile (dir </> name)
