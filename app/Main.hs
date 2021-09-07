module Main where

import Data.List (intercalate)
import Lib

-- main :: IO ()
-- main = putStrLn "Use 'stack ghci` to interactively call any of the functions."

main = interact (intercalate " / " . map reverse . lines)
