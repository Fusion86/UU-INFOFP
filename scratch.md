```hs
reverse :: [a] -> [a]
reverse [] = []
reverse ...



(reverse . reverse) xs
=
...
=
id xs

-- case xs = []

(reverse . reverse) []
= -- def .
reverse (reverse [])
= -- def reverse
reverse []
= -- def reverse
[]
=

-- hypothesis that (reverse . reverse ) xs = id xs  (induction hypothesis)

(reverse . reverse) (x:xs)
= -- def .
reverse (reverse (x:xs))
= -- def of reverse
reverse (reverse xs ++ [x])
id (x:xs)
```
