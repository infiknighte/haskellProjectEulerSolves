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

-- | What is the largest prime factor of the number 600851475143?
--
-- This is the worst way that I could I have done it, it's coz I
-- misinterpreted the question, well, you will be like this for some time
p3 :: Int
p3 = head $ filter (\x -> 600851475143 `mod` x == 0) (primesIn [2 .. 100000] [])
  where
    primesIn :: [Int] -> [Int] -> [Int]
    primesIn [] ps = ps
    primesIn (n : ns) ps = primesIn (filter (\x -> x `mod` n /= 0) ns) (n : ps)
