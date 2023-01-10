module Types where

import Control.Monad.Reader (MonadReader, asks, liftIO)
import UnliftIO (MonadIO, hFlush, stdout)
import Prelude hiding (log)

type FullWallpaperURL = String

type WallpaperName = String

type NumParallelDownloads = Int

type Page = Int

type LocalWallpapers = [FilePath]

type Label = String

type CollectionID = Int

-- Either local wallpaper path or preview or full wallpaper URL.
type WallpaperPath = String

type Username = String
