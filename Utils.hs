module Utils where

import Data.Map (Map)
import Data.Map qualified as Map

-- | An infinite list of fibs
fibs :: [Int]
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
primeOf :: Int -> Int
primeOf = primeOf' 2
  where
    primeOf' :: Int -> Int -> Int
    primeOf' n m
      | isPrime n = if m == 1 then n else primeOf' (n + 1) (m - 1)
      | otherwise = primeOf' (n + 1) m
      where
        isPrime :: Int -> Bool
        isPrime x = (x >= 2) && isPrime' 2
          where
            isPrime' :: Int -> Bool
            isPrime' i
              | i * i > x = True
              | x `mod` i == 0 = False
              | otherwise = isPrime' (i + 1)

-- | Pythagorean Triple of n (including non-primitive)
euclidsFormula :: Int -> (Int, Int, Int)
euclidsFormula n = euclidsFormula' 0 2 1
  where
    euclidsFormula' :: Int -> Int -> Int -> (Int, Int, Int)
    euclidsFormula' i m n'
      | n' >= m + 1 = euclidsFormula' i (m + 1) 1
      | i == n - 1 = (a, b, c)
      | otherwise = euclidsFormula' (i + 1) m (n' + 1)
      where
        a = m ^ 2 - n' ^ 2
        b = 2 * m * n'
        c = m ^ 2 + n' ^ 2

sieveOfEratosthenes :: (Ord a, Num a, Enum a) => a -> [a]
sieveOfEratosthenes n = sieveOfEratosthenes' [2 .. n] Map.empty
  where
    sieveOfEratosthenes' :: (Ord a, Num a) => [a] -> Map a [a] -> [a]
    sieveOfEratosthenes' [] _ = []
    sieveOfEratosthenes' (x : xs) table = case Map.lookup x table of
      Nothing -> x : sieveOfEratosthenes' xs (Map.insert (x * x) [x] table)
      Just factors -> sieveOfEratosthenes' xs $ foldl reinsert (Map.delete x table) factors
      where
        reinsert table' prime = Map.insertWith (++) (x + prime) [prime] table'

combinations :: Int -> [a] -> [[a]]
combinations _ [] = []
combinations n as = if length as < n then [] else take n as : combinations n (tail as)

crossAt :: Int -> [[Int]] -> Int -> Int -> [[Int]]
crossAt r m x y = [extractDiagonal d | d <- deltas]
  where
    deltas = [(-1, -1), (1, 1), (-1, 1), (1, -1)]
    extractDiagonal :: (Int, Int) -> [Int]
    extractDiagonal (dX, dY) =
      [ m !! (x + k * dX) !! (y + k * dY)
      | k <- [0 .. r - 1],
        inBounds (x + k * dX) (y + k * dY)
      ]
    inBounds :: Int -> Int -> Bool
    inBounds x' y' = x' >= 0 && y' >= 0 && x' < w && y' < w
      where
        w = length m
