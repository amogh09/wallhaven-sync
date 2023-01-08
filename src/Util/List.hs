module Util.List (batches, interleave) where

batches :: Int -> [a] -> [[a]]
batches _ [] = []
batches n xs = take n xs : batches n (drop n xs)

interleave :: [a] -> [a] -> [a]
interleave xs [] = xs
interleave [] ys = ys
interleave (x : xs) (y : ys) = x : y : interleave xs ys
