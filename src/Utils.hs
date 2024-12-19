module Utils where

-- | Returns a list of fibs sequence under n
fibSqUnder :: Int -> [Int]
fibSqUnder n = take n fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- | Returns the prime factors of n
primeFactorsOf :: Int -> [Int]
primeFactorsOf n = primeFactorsOf' n 2
  where
    primeFactorsOf' :: Int -> Int -> [Int]
    primeFactorsOf' m x
      | x * x > m = [m]
      | m `mod` x == 0 = x : primeFactorsOf' (m `div` x) x
      | otherwise = primeFactorsOf' m (x + 1)

-- | Returns the n-th prime number
nthPrime :: Int -> Int
nthPrime = nthPrime' 2
  where
    nthPrime' :: Int -> Int -> Int
    nthPrime' n m
      | isPrime n = if m == 1 then n else nthPrime' (n + 1) (m - 1)
      | otherwise = nthPrime' (n + 1) m

-- | Checks if a Int is a prime or not
isPrime :: Int -> Bool
isPrime x = (x >= 2) && isPrime' 2
  where
    isPrime' :: Int -> Bool
    isPrime' i
      | i * i > x = True
      | x `mod` i == 0 = False
      | otherwise = isPrime' (i + 1)
