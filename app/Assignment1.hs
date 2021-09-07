module Assignment1 where

import Data.List (intercalate)
import Lib

type Field = String

type Row = [Field]

type Table = [Row]

main :: IO ()
main = interact (words . lines)
