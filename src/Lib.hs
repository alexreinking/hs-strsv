module Lib
    ( dotp
    , solve
    , solve2
    , solve3
    , solve4
    ) where

import Data.List

-- Straightforward dot product
dotp :: Num a => [a] -> [a] -> a
dotp x y = sum $ zipWith (*) x y

-- Direct implementation based on recursive structure
solve' :: Fractional a => [[a]] -> [a] -> [a]
solve' [] [] = []
solve' ((l:ls):_L) (b:_b) = (b - dotp ls prev) / l : prev
  where prev = solve' _L _b

solve :: Fractional a => [[a]] -> [a] -> [a]
solve _L b = reverse $ solve' _L' b'
  where _L' = reverse (map reverse _L)
        b' = reverse b

-- Simplified version of above, using foldr
solve2' :: Fractional a => [[a]] -> [a] -> [a]
solve2' _L _b = foldr f [] $ zip _L _b
  where f ((l:ls), b) prev = (b - dotp ls prev) / l : prev

solve2 :: Fractional a => [[a]] -> [a] -> [a]
solve2 _L b = reverse $ solve2' _L' b'
  where _L' = reverse (map reverse _L)
        b' = reverse b

-- Avoid pre-reversing by using foldl'.
solve3' :: Fractional a => [[a]] -> [a] -> [a]
solve3' _L _b = foldl' f [] $ zip _L _b
  where f prev ((l:ls), b) = (b - dotp ls prev) / l : prev

solve3 :: Fractional a => [[a]] -> [a] -> [a]
solve3 _L b = reverse $ solve3' _L' b
  where _L' = map reverse _L

-- Don't use reverse -- need different DS with O(1) append
solve4' :: Fractional a => [[a]] -> [a] -> [a]
solve4' _L _b = foldl' f [] $ zip _L _b
  where f prev (row, b) = prev ++ [let (l, ls) = (last row, init row) 
                                    in (b - dotp ls prev) / l]

solve4 :: Fractional a => [[a]] -> [a] -> [a]
solve4 _L b = solve4' _L b
