module Database.FileSystem.Action
  ( deleteWallpaper,
    getWallpaperNames,
    HasWallpaperDir (..),
  )
where

import Control.Monad (when)
import Control.Monad.Reader (MonadIO, MonadReader, asks)
import System.FilePath ((</>))
import Types (WallpaperName)
import UnliftIO.Directory (doesFileExist, listDirectory, removeFile)

class HasWallpaperDir a where
  getWallpaperDir :: a -> FilePath

deleteWallpaper ::
  ( MonadReader env m,
    HasWallpaperDir env,
    MonadIO m
  ) =>
  WallpaperName ->
  m ()
deleteWallpaper name = do
  dir <- asks getWallpaperDir
  let path = dir </> name
  exists <- doesFileExist path
  when exists $ removeFile path

getWallpaperNames ::
  ( MonadReader env m,
    HasWallpaperDir env,
    MonadIO m
  ) =>
  m [WallpaperName]
getWallpaperNames = asks getWallpaperDir >>= listDirectory
