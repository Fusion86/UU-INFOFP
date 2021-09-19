## head ([3, 2] ++ [2])
[a] -> a
Num a => [a] -> a
[Int] -> Int

## (+) 3
Num a => a -> a

## map even
map = (a -> b) -> [a] -> [b]
[Int] -> [Bool]

## map concat
map = (a -> b) -> [a] -> [b]
concat = [[a]] -> [a]

[[[a]]] -> [[b]]

## map head
head = [a] -> a
[[a]] -> [a]

## reverse . reverse
[a] -> [a] 

## foldr (+)
foldr: (a -> b -> b) -> b -> [a] -> b
        ^ (+)

(Int -> Int -> Int) -> Int -> [Int] -> Int


## foldr map
foldr: (a -> b -> b) -> b -> [a] -> b
        ^ Map

[a] -> [a -> a] -> [a]

## map . foldr
