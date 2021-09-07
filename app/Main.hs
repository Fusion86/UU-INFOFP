module Main where

import Lib

main :: IO ()
main =
  print (sumUpTo 5)
    >> putStrLn (greet "Wouter")
    >> putStrLn (greet2 "Wouter" "Morning")
