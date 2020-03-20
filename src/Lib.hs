module Lib
    ( dotp
    , solve
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

dotp :: Num a => [a] -> [a] -> a
dotp x y = sum $ zipWith (*) x y

solve' :: Fractional a => [[a]] -> [a] -> [a]
solve' [] [] = []
solve' ((l:ls):_L) (b:_b) =
  let prev = solve' _L _b
   in (b - dotp ls prev) / l : prev

solve :: Fractional a => [[a]] -> [a] -> [a]
solve _L b = reverse $ solve' _L' b'
  where _L' = reverse (map reverse _L)
        b' = reverse b
