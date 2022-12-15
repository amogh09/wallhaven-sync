module Util.List (batches) where

batches :: Int -> [a] -> [[a]]
batches _ [] = []
batches n xs = take n xs : batches n (drop n xs)
