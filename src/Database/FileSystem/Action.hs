module Database.FileSystem.Action
  ( deleteWallpaper,
    saveWallpaper,
    getWallpaperNames,
    HasWallpaperDir (..),
    createWallpaperDir,
  )
where

import Control.Monad (when)
import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)
import Data.ByteString (ByteString)
import System.FilePath ((</>))
import Types (WallpaperName)
import UnliftIO.Directory
  ( createDirectoryIfMissing,
    doesFileExist,
    listDirectory,
    removeFile,
  )
import UnliftIO.IO.File (writeBinaryFile)

class HasWallpaperDir a where
  getWallpaperDir :: a -> FilePath

type DatabaseM env m =
  ( MonadReader env m,
    HasWallpaperDir env,
    MonadIO m
  )

createWallpaperDir :: DatabaseM env m => m ()
createWallpaperDir =
  asks getWallpaperDir >>= liftIO . createDirectoryIfMissing True

deleteWallpaper :: DatabaseM env m => WallpaperName -> m ()
deleteWallpaper name = do
  dir <- asks getWallpaperDir
  let path = dir </> name
  exists <- doesFileExist path
  when exists $ removeFile path

getWallpaperNames :: DatabaseM env m => m [WallpaperName]
getWallpaperNames = asks getWallpaperDir >>= listDirectory

saveWallpaper :: DatabaseM env m => WallpaperName -> ByteString -> m ()
saveWallpaper name wallpaper = do
  dir <- asks getWallpaperDir
  writeBinaryFile (dir </> name) wallpaper
