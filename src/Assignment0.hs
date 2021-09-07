module Assignment0 where

import Data.List (intercalate)

main :: IO ()
main = interact (intercalate " / " . map reverse . lines)
