module Main where

-- Chapter 2 Exercises

n = a `div` length ns
  where
    a = 10
    ns = [1, 2]

last1 ns = drop (length ns - 1) ns

last2 ns = ns !! (length ns - 1)

init1 ns = take (length ns - 1) ns

-- Chapter 3 Exercises

bools :: [Bool]
bools = [True, True]

nums :: [[Int]]
nums = [[1, 2, 3], [4, 5, 6]]

add :: Int -> Int -> Int -> Int
add a b c = a + b + c

copy :: a -> (a, a)
copy a = (a, a)

-- Idk?
apply :: (a -> b) -> a -> b
apply a b = a b

-- 3.5
-- Functions which are equal should not exist, because you can just
-- remove the second function and use the first.

-- Chapter 4

-- Scratch

isPositive n
  | n >= 0 = True
  | otherwise = False

-- Whatever

thrice x = [x, x, x]

sums (x : y : ys) = x : sums (x + y : ys)
sums xs = xs

pow2 :: Int -> Int
pow2 0 = 1
pow2 n = 2 * pow2 (n - 1)

pow3 :: Int -> Int -> Int
pow3 _ 0 = 1
pow3 x y = x * pow3 x (y - 1)

pow4 :: Int -> Int -> Int
pow4 x n
  | n == 0 = 1
  | even (n) = let y = pow4 x (n `div` 2) in y * y
  | otherwise = x * pow4 x (n - 1)

-- These two funcs are exactly the same.
f1 = (`replicate` '-')
f2 x = replicate x '-'
