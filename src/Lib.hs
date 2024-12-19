module Lib where

import Utils

-- | Find the sum of all multiples of 3 or 5 below 1000
p1 :: Int
p1 = sum $ filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0) [1 .. 999]

-- | By considering the terms in the Fibonacci sequence whose values do not exceed four million,
--  find the sum of the even-valued terms.
p2 :: Int
p2 = sum $ filter even $ fibSqUnder 4000000

-- | What is the largest prime factor of the number 600851475143?
p3 :: Int
p3 = last $ primeFactorsOf 600851475143

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
p6 = (sum ns ^ (2 :: Int)) - sum [n * n | n <- ns]
  where
    ns = [1 .. 100]

-- | What is the 10,001st prime number?
p7 :: Int
p7 = nthPrime 10001

-- | Find the thirteen adjacent digits in the 1000-digit number that have the greatest product. What is the value of this product?
p8 :: Int
p8 = maximum $ map product $ combinations $ map (read . (: [])) number
  where
    combinations :: [Int] -> [[Int]]
    combinations [] = []
    combinations ns = take 13 ns : combinations (tail ns)
    number = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
