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

take2 :: Int -> [a] -> [a]
take2 0 _ = []
take2 _ [] = []
take2 n (x : xs) = x : take2 (n - 1) xs

-- folds

concatMap2 :: (a -> [b]) -> [a] -> [b]
concatMap2 g xs = foldr f [] xs
  where
    f x r = (g x) ++ r

concatMap3 listGen xs = concat (map listGen xs)

concatMap4 f [] = []
concatMap4 f (x : xs) = f x ++ concatMap4 f xs

-- data types

data Nat = Zero | Succ Nat

toInt :: Nat -> Int
toInt Zero = 0
toInt (Succ x) = 1 + toInt x

fromInt :: Int -> Nat
fromInt 0 = Zero
fromInt n
  | n == 0 = Zero
  | n > 0 = Succ (fromInt $ n -1)
  | otherwise = error "No negative numbers pls."

data Tree a = Leaf | Node (Tree a) a (Tree a)

elemTree :: Ord a => Tree a -> a -> Bool
elemTree Leaf _ = False
elemTree (Node l x r) a
  | a < x = elemTree l x
  | a == x = True
  | otherwise = elemTree r x
