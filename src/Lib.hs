module Lib
  ( someFunc,
    sumUpTo,
    greet,
    greet2,
  )
where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

sumUpTo :: Int -> Int
sumUpTo 0 = 0
sumUpTo n = n + sumUpTo (n - 1)

greet :: String -> String
greet name = "Hello, " ++ name ++ "!"

greet2 :: String -> String -> String
greet2 name timeOfDay = "Good " ++ timeOfDay ++ " " ++ name ++ "!"

