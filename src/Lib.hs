module Lib where

-- | Find the sum of all multiples of 3 or 5 below 1000
p1 :: Int
p1 = sum $ filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0) [1 .. 999]

-- | By considering the terms in the Fibonacci sequence whose values do not exceed four million,
--  find the sum of the even-valued terms.
p2 :: Int
p2 = sum $ filter even $ fibSqUnder 4000000
  where
    fibSqUnder :: Int -> [Int]
    fibSqUnder n = takeWhile (<= n) fibs
      where
        fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
