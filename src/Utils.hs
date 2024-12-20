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
prime :: Int -> Int
prime = prime' 2
  where
    prime' :: Int -> Int -> Int
    prime' n m
      | isPrime n = if m == 1 then n else prime' (n + 1) (m - 1)
      | otherwise = prime' (n + 1) m

-- | Checks if a Int is a prime or not
isPrime :: Int -> Bool
isPrime x = (x >= 2) && isPrime' 2
  where
    isPrime' :: Int -> Bool
    isPrime' i
      | i * i > x = True
      | x `mod` i == 0 = False
      | otherwise = isPrime' (i + 1)

pythTriplet :: Int -> (Int, Int, Int)
pythTriplet x = pythTriplet' 0 2 1
  where
    pythTriplet' :: Int -> Int -> Int -> (Int, Int, Int)
    pythTriplet' i m n
      | n >= m + 1 = pythTriplet' i (m + 1) 1
      | otherwise =
          if i == (x - 1)
            then
              let a = m ^ 2 - n ^ 2
                  b = 2 * m * n
                  c = (m ^ 2) + (n ^ 2)
               in (a, b, c)
            else pythTriplet' (i + 1) m (n + 1)
