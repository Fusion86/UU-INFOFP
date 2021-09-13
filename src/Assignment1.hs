module Assignment1 where

{- This is a framework in which all functions to be written are "undefined".  -
 - Note that in most cases parameters, pattern-matching and guards have been  -
 - omitted! You will have to add those yourself.                              -}

import Data.Char
import Data.List
import Data.Maybe

-- | Model
type Field = String

type Row = [Field]

type Table = [Row]

-- | Main
main :: IO ()
main = interact (unlines . exercise . lines)

exercise :: [String] -> [String]
exercise =
  printTable
    . project ["last", "first", "salary"]
    . select "gender" "male"
    . parseTable

-- | Parsing

-- * Exercise 1

parseTable :: [String] -> Table
parseTable = map words

-- | Printing

-- * Exercise 2

printLine :: [Int] -> String
-- 2a. 'Abuse' ++ operators
-- printLine = (++ "+") . (++) "+" . intercalate "+" . map (`replicate` '-')

-- 2b. Using replicate as infix
printLine x = "+" ++ intercalate "+" (map (`replicate` '-') x) ++ "+"

-- 2c. Using flip instead of infix
-- printLine x = "+" ++ intercalate "+" (map (flip replicate '-') x) ++ "+"

-- 2d. This is wrong, it misses the leading and trailing "+"
-- printLine = intercalate "+" . map f
--   where
--     f a = replicate a '-'

-- 2e. 'Easy' solution
-- printLine x = "+" ++ intercalate "+" (map f x) ++ "+"
--   where
--     f a = replicate a '-'

-- * Exercise 3

printField :: Int -> String -> String
printField i str
  | all isDigit str = padding ++ str
  | otherwise = str ++ padding
  where
    padLength = i - length str
    padding = concat (replicate padLength " ")

-- * Exercise 4

printRow :: [(Int, String)] -> String
printRow x = "|" ++ intercalate "|" (map (uncurry printField) x) ++ "|"

-- * Exercise 5

-- Table == [[String]]

columnWidths :: Table -> [Int]
columnWidths = map maximum . transpose . map (map length)

-- columnWidths table = map maximum (transpose (map (map length) table))

-- * Exercise 6

printTable :: Table -> [String]
printTable [] = error "Table is empty!"
printTable table@(header : rows) =
  let colWidth = columnWidths table
      separator = [printLine colWidth]
   in separator
        ++ [printRow (zip colWidth (strToUpper header))]
        ++ separator
        ++ map (printRow . zip colWidth) rows
        ++ separator
  where
    strToUpper = map (map toUpper)

-- | Querying

-- * Exercise 7

select :: Field -> Field -> Table -> Table
select column value table@(header : rows) =
  case elemIndex column header of
    Just idx -> header : filter (p idx) rows
    Nothing -> error ("Column " ++ value ++ " does not exist!")
  where
    p colIdx row = (row !! colIdx) == value
select _ _ _ = undefined -- Stop complaining about incomplete patterns.

-- * Exercise 8

project :: [Field] -> Table -> Table
project columns table@(header : _) =
  transpose (mapMaybe getColumn columns)
  where
    getColumn col = case elemIndex col header of
      Just idx -> Just (map (!! idx) table)
      Nothing -> Nothing
project _ _ = undefined -- Stop complaining about incomplete patterns.
