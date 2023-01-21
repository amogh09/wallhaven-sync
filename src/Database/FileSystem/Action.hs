module Database.FileSystem.Action (deleteFileIfExists) where

import Control.Monad (when)
import UnliftIO (MonadIO)
import UnliftIO.Directory (doesFileExist, removeFile)

deleteFileIfExists :: MonadIO m => FilePath -> m ()
deleteFileIfExists path = do
  exists <- doesFileExist path
  when exists $ removeFile path
