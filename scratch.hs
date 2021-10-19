import Data.List (intercalate)
import Data.Maybe (mapMaybe)

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

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
  | x < y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

halve :: [a] -> ([a], [a])
halve xs =
  let len = length xs `div` 2
   in (take len xs, drop len xs)

mergeSort :: Ord a => [a] -> [a]
mergeSort [a] = [a]
mergeSort xx =
  let (xs, ys) = halve xx
   in merge (mergeSort xs) (mergeSort ys)

data Pair a b = MkPair a b

getA :: Pair a b -> a
getA (MkPair a _) = a

getB :: Pair a b -> b
getB (MkPair _ b) = b

-- !!!!!!
instance Eq b => Eq (Pair a b) where
  MkPair _ a == MkPair _ b = a == b

instance Ord b => Ord (Pair a b) where
  MkPair _ a <= MkPair _ b = a <= b

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f xs = map getA (mergeSort (map makePair xs))
  where
    makePair x = MkPair x (f x)

any' :: (a -> Bool) -> [a] -> Bool
any' p = foldr f x0
  where
    f a b
      | p a = True
      | otherwise = b
    x0 = False

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs = (mapMaybe (f True) xs, mapMaybe (f False) xs)
  where
    f wanted x
      | p x == wanted = Just x
      | otherwise = Nothing

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x : xs)
  | f x = x : myFilter f xs
  | otherwise = myFilter f xs

elementAt :: [a] -> Int -> a
elementAt xs i = xs !! (i - 1)

allPairs :: [a] -> [(a, a)]
allPairs lst = allPairs' lst lst
  where
    allPairs' :: [a] -> [a] -> [(a, a)]
    allPairs' _ [] = []
    allPairs' x (y : ys) = map pair x ++ allPairs' x ys
      where
        pair z = (y, z)

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs y = take (y -1) xs ++ dropEvery (drop y xs) y

multipleFS :: Int -> Bool
multipleFS x
  | x `mod` 5 == 0 = True
  | x `mod` 7 == 0 = True
  | otherwise = False

em1 :: [Int] -> [Int]
em1 [] = []
em1 (x : xs)
  | multipleFS x = em1 xs
  | otherwise = x : em1 xs

em2 :: [Int] -> [Int]
em2 xs = [x | x <- xs, not $ multipleFS x]

em3 :: [Int] -> [Int]
em3 xs = filter (not . multipleFS) xs

data Op = Plus | Minus | Multiply | Divide deriving (Enum)

data Expr a = Const a | Binary Op (Expr a) (Expr a)

-- instance Show (Expr a) where
-- show (Const a) = show a
-- show (Binary op a b) = "(" ++ show a ++ show op ++ show b ++ ")"

data Example = Example Int

instance Eq Example where
  (Example x) == (Example y) = x == y

instance Show Example where
  show (Example a) = show a

splits :: [a] -> [a]
splits (x : y : ys) = undefined

plus' :: Nat -> Nat -> Nat
plus' Zero n = n
plus' (Succ m) n = Succ (plus' m n)

oneAfterEach :: (Int -> Bool) -> Int -> [Int] -> [Int]
oneAfterEach _ _ [] = []
oneAfterEach p v (x : xs)
  | p x = [x, v] ++ oneAfterEach p v xs
  | otherwise = x : oneAfterEach p v xs

putStr2 :: String -> IO ()
putStr2 s = sequence_ . map putChar $ s

-- putStr2 s = (sequence_ . map putChar) s

-- return (f a b)
-- return $ f a b

adder :: IO ()
adder = do
  count <- askNumber "How many numbers to add? "
  total <- askNumbers count 0
  putStrLn $ "Total: " ++ show total
  where
    askNumber :: String -> IO Int
    askNumber s = do
      putStr s
      line <- getLine
      pure (read line :: Int)
    askNumbers :: Int -> Int -> IO Int
    askNumbers 0 acc = pure acc
    askNumbers x acc = do
      number <- askNumber "Gimme a number plox: "
      askNumbers (x - 1) (acc + number)

adder2 :: IO ()
adder2 = do
  count <- askNumber "How many numbers to add? "
  x <- sequence $ replicate count (getLine >>= (pure . read))
  putStrLn $ "Total: " ++ show (sum x)
  where
    askNumber :: String -> IO Int
    askNumber s = do
      putStr s
      line <- getLine
      pure (read line :: Int)

    askNumber2 :: IO Int
    askNumber2 = do
      line <- getLine
      pure (read line :: Int)

mapTree :: (a->b) -> Tree a -> Tree b
mapTree f Leaf = Leaf (f a)
mapTree f (Node l v r) = Node (mapTree f l) (f v) (mapTree f r)

fmap :: (a->b) -> IO a -> IO b
fmap f a = do 
  a' <- a
  return $ f a
