module Util.Batch (processBatches) where

import Control.Concurrent.Async (async, wait)
import Control.Monad (join)
import Util.List (batches)

processBatches :: Show a => Int -> (a -> IO b) -> [a] -> IO [b]
processBatches n f = fmap join . mapM (processBatch f) . batches n

processBatch :: Show a => (a -> IO b) -> [a] -> IO [b]
processBatch f xs = mapM (async . f) xs >>= mapM wait
