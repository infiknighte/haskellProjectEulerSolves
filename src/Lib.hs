module Lib (p1) where

p1 :: Int
p1 = sum $ filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0) [1 .. 999]
