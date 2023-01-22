module Util.Batch (batchedM, batchedM_) where

import Control.Monad (void)
import UnliftIO (MonadUnliftIO, mapConcurrently)
import qualified Util.List as List

-- | Performs the given action parallely in batches of the given size.
-- The results are returned in the same order as the input list.
-- This is useful when the action accesses a limited resource.
batchedM ::
  (MonadUnliftIO m) => Int -> (a -> m b) -> [a] -> m [b]
batchedM size f =
  fmap concat
    . mapM (mapConcurrently f)
    . List.batches size

batchedM_ ::
  (MonadUnliftIO m) => Int -> (a -> m b) -> [a] -> m ()
batchedM_ size f = void . batchedM size f
