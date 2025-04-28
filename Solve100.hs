module Solve100 where

import Data.List qualified as L (transpose)
import Utils qualified as U

-- | Find the sum of all multiples of 3 or 5 below 1000
p1 :: Int
p1 = sum $ filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0) [1 .. 999]

-- | By considering the terms in the Fibonacci sequence whose values do not exceed four million,
--  find the sum of the even-valued terms.
p2 :: Int
p2 = sum $ filter even $ take 4000000 U.fibs

-- | What is the largest prime factor of the number 600851475143?
p3 :: Int
p3 = last $ U.primeFactorsOf 600851475143

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
p6 = (sum ns ^ 2) - sum [n * n | n <- ns]
  where
    ns = [1 .. 100]

-- | What is the 10,001st prime number?
p7 :: Int
p7 = U.primeOf 10001

-- | Find the thirteen adjacent digits in the 1000-digit number that have the greatest product. What is the value of this product?
p8 :: Int
p8 = maximum . map product . U.combinations 13 $ map (read . (: [])) number
  where
    number = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"

-- | There exists exactly one Pythagorean triplet for which a + b + c = 1000.Find the product abc.
p9 :: Int
p9 = (\(a, b, c) -> a * b * c) . head . filter (\(a, b, c) -> a + b + c == 1000) $ triplets
  where
    triplets = map U.euclidsFormula [1 ..]

p10 :: Int
p10 = sum $ U.sieveOfEratosthenes 2000000

p11 :: Int
p11 = maximum [maxHorizontal, maxVertical, maxDiagonal]
  where
    maxHorizontal = maximum $ map (maximum . map product) horizontal
    maxVertical = maximum $ map (maximum . map product) vertical
    maxDiagonal = maximum $ map (maximum . map product) diagonal
    horizontal = map combinations gridChars
    vertical = map combinations $ L.transpose gridChars
    diagonal = diagonal' 0 0
      where
        diagonal' i j
          | i == 20 = []
          | j == 20 = diagonal' (i + 1) 0
          | otherwise = U.crossAt 4 gridChars i j : diagonal' i (j + 1)
    combinations = U.combinations 4
    gridChars :: [[Int]]
    gridChars = map (map read . words) $ lines grid
    grid =
      "08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08\n\
      \49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00\n\
      \81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65\n\
      \52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91\n\
      \22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80\n\
      \24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50\n\
      \32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70\n\
      \67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21\n\
      \24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72\n\
      \21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95\n\
      \78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92\n\
      \16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57\n\
      \86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58\n\
      \19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40\n\
      \04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66\n\
      \88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69\n\
      \04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36\n\
      \20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16\n\
      \20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54\n\
      \01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"