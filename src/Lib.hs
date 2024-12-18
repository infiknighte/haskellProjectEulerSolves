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
p3 :: Int
p3 = last $ primeFactorsOf 600851475143
  where
    primeFactorsOf :: Int -> [Int]
    primeFactorsOf n = primeFactorsOf' n 2
      where
        primeFactorsOf' :: Int -> Int -> [Int]
        primeFactorsOf' m x
          | x * x > m = [m]
          | m `mod` x == 0 = x : primeFactorsOf' (m `div` x) x
          | otherwise = primeFactorsOf' m (x + 1)

-- | Find the largest palindrome made from the product of two 3-digit numbers.   |
p4 :: Int
p4 = maximum [n | x <- ns, y <- ns, let n = x * y, let s = show n, s == reverse s]
  where
    ns = [100 .. 999]

-- | What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
p5 :: Int
p5 = foldl lcm 2520 [11 .. 20]

-- | Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
p6 :: Int
p6 = (sum ns ^ (2 :: Integer)) - sum [n * n | n <- ns]
  where
    ns = [1 .. 100]

-- | What is the 10,001st prime number?
p7 :: Int
p7 = nthPrime 10001
  where
    nthPrime :: Int -> Int
    nthPrime = nthPrime' 2
      where
        nthPrime' :: Int -> Int -> Int
        nthPrime' n m
          | isPrime n = if m == 1 then n else nthPrime' (n + 1) (m - 1)
          | otherwise = nthPrime' (n + 1) m
          where
            isPrime :: Int -> Bool
            isPrime x
              | x < 2 = False
              | otherwise = isPrime' 2
              where
                isPrime' :: Int -> Bool
                isPrime' i
                  | i * i > x = True
                  | x `mod` i == 0 = False
                  | otherwise = isPrime' (i + 1)
