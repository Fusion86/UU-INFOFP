module Assignment0 where

import Data.List (intercalate)
import Lib

main :: IO ()
main = interact (intercalate " / " . map reverse . lines)
