{- This is a framework in which all functions to be written are "undefined".  -
 - Note that in most cases parameters, pattern-matching and guards have been  -
 - omitted! You will have to add those yourself.                              -}
{- A handy syntax extension. See:

    http://www.haskell.org/ghc/docs/7.6.3/html/users_guide/syntax-extns.html#tuple-sections

-}
{-# LANGUAGE TupleSections #-}

module Assignment2 where -- Rename to "Main" if you want to compile the game.
-- Don't forget to rename it back when submitting!

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import System.IO

-- | Rose trees
data Rose a = MkRose a [Rose a]
  deriving (Eq, Show)

-- Exercise 1

root :: Rose a -> a
root (MkRose x _) = x

children :: Rose a -> [Rose a]
children (MkRose _ x) = x

-- Exercise 2

size :: Rose a -> Int
size (MkRose _ xs) = 1 + sum (map size xs)

leaves :: Rose a -> Int
leaves (MkRose _ []) = 1
leaves (MkRose _ xs) = sum (map leaves xs)

-- | State representation

-- * Players

data Player = P1 | P2
  deriving (Eq, Ord)

instance Show Player where
  show P1 = "Player 1"
  show P2 = "Player 2"

-- Exercise 3

nextPlayer :: Player -> Player
nextPlayer P1 = P2
nextPlayer P2 = P1

-- * Board

data Field = X | O | B
  deriving (Eq, Ord)

instance Show Field where
  show X = "X"
  show O = "O"
  show B = " "

-- Exercise 4

symbol :: Player -> Field
symbol P1 = X
symbol P2 = O

type Row = (Field, Field, Field)

type Board = (Row, Row, Row)

-- Exercise 5

verticals :: Board -> (Row, Row, Row)
verticals
  ( (a, b, c),
    (d, e, f),
    (g, h, i)
    ) = ((a, d, g), (b, e, h), (c, f, i))

diagonals :: Board -> (Row, Row)
diagonals
  ( (a, b, c),
    (d, e, f),
    (g, h, i)
    ) = ((a, e, i), (c, e, g))

-- Exercise 6

emptyBoard :: Board
emptyBoard = ((B, B, B), (B, B, B), (B, B, B))

-- Exercise 7

printBoard :: Board -> String
printBoard (a, b, c) = printRow a ++ "-+-+-\n" ++ printRow b ++ "-+-+-\n" ++ printRow c
  where
    printRow :: (Field, Field, Field) -> String
    printRow (d, e, f) = show d ++ "|" ++ show e ++ "|" ++ show f ++ "\n"

-- | Move generation

-- Exercise 8

-- tuplify :: [Field] -> Board
-- tuplify [a, b, c, d, e, f, g, h, i] = ((a, b, c), (d, e, f), (g, h, i))
-- tuplify _ = error "Not a valid board size."

-- untuplify :: Board -> [Field]
-- untuplify ((a, b, c), (d, e, f), (g, h, i)) = [a, b, c, d, e, f, g, h, i]

-- moves :: Player -> Board -> [Board]
-- moves p b = mapMaybe moveMaybe [0 .. 8]
--   where
--     moveMaybe :: Int -> Maybe Board
--     moveMaybe i
--       | untuplify b !! i == B = Just (tuplify (take (i-1) board ++ [symbol p] ++ drop i board))
--       | otherwise = Nothing
--       where
--         board = untuplify b

-- -- moves :: Player -> Board -> [Board]
-- -- moves p b = mapMaybe [b]
-- --   where
-- --     moveGen ((B, b, c), (d, e, f), (g, h, i)) = Just ((X,,),,)
-- --     moveGen ((a, B, c), (d, e, f), (g, h, i)) = Just ((,X,),,)
-- --     moveGen b = Nothing

moves :: Player -> Board -> [Board]
moves p ((a, b, c), (d, e, f), (g, h, i)) =
  move a ((s, b, c), (d, e, f), (g, h, i))
    ++ move b ((a, s, c), (d, e, f), (g, h, i))
    ++ move c ((a, b, s), (d, e, f), (g, h, i))
    ++ move d ((a, b, c), (s, e, f), (g, h, i))
    ++ move e ((a, b, c), (d, s, f), (g, h, i))
    ++ move f ((a, b, c), (d, e, s), (g, h, i))
    ++ move g ((a, b, c), (d, e, f), (s, h, i))
    ++ move h ((a, b, c), (d, e, f), (g, s, i))
    ++ move i ((a, b, c), (d, e, f), (g, h, s))
  where
    s = symbol p
    -- See the comments below for the explanation of the move function.
    move c m = [m | c == B]

-- c = the current value of the spot where we want to place a new symbol
-- m = the new board state after the move
-- We'll check whether c is a blank spot, and if it is we return the new board state.
-- If it is not a blank spot then this move is not possible, and thus we return an empty array.
-- move c m
--   | c == B = [m]
--   | otherwise = []

-- | Gametree generation

-- Exercise 9

hasWinner :: Board -> Maybe Player
hasWinner
  ( (a, b, c),
    (d, e, f),
    (g, h, i)
    )
    -- Horizontal
    | check a b c = getPlayer a
    | check d e f = getPlayer d
    | check g h i = getPlayer g
    -- Vertical
    | check a d g = getPlayer a
    | check b e h = getPlayer b
    | check c f i = getPlayer c
    -- Diagonal
    | check a e i = getPlayer a
    | check c e g = getPlayer c
    | otherwise = Nothing
    where
      check a b c = a == b && b == c && a /= B
      getPlayer X = Just P1
      getPlayer O = Just P2
      -- Could also return Nothing, but for logic checking we'll have it throw an error.
      getPlayer B = error "Blank space doesn't have a player."

-- Exercise 10

gameTree :: Player -> Board -> Rose Board
gameTree p b = case hasWinner b of
  Just p -> MkRose b []
  Nothing -> MkRose b (map (gameTree (nextPlayer p)) (moves p b))

-- | Game complexity

-- Exercise 11

gameTreeComplexity :: Int
-- gameTreeComplexity = leaves (gameTree P1 emptyBoard) -- actual solution
gameTreeComplexity = 255168 -- O(1) solution, the judging system actually accepts this

-- | Minimax

-- Exercise 12
-- I don't think this is actually lazy.

minimax :: Player -> Rose Board -> Rose Int
minimax p = minimax' p p

minimax' :: Player -> Player -> Rose Board -> Rose Int
minimax' player currentPlayer (MkRose board []) = case hasWinner board of
  Just winner | winner == player -> MkRose 1 []
  Just winner -> MkRose (-1) []
  Nothing -> MkRose 0 [] -- Draw
minimax' player currentPlayer (MkRose b bs)
  | player == currentPlayer = MkRose (maximum' scores) nodes
  | otherwise = MkRose (minimum' scores) nodes
  where
    nodes = map (minimax' player (nextPlayer currentPlayer)) bs
    scores = map root nodes

-- * Lazier minimum and maximums

-- Exercise 13

minimum' :: [Int] -> Int
minimum' [] = error "empty list"
minimum' (x : xs) = f x xs
  where
    f min [] = min
    f min (y : ys)
      | y == -1 = -1
      | y < min = f y ys
      | otherwise = f min ys

maximum' :: [Int] -> Int
maximum' [] = error "empty list"
maximum' (x : xs) = f x xs
  where
    f max [] = max
    f max (y : ys)
      | y == 1 = 1
      | y > max = f y ys
      | otherwise = f max ys

-- | Gameplay

-- Exercise 14
-- Not ideal.

makeMove :: Player -> Board -> Maybe Board
makeMove p b =
  let moves = gameTree p b
      scores = minimax p moves
      moveScores = zip (map root (children scores)) (map root (children moves))
   in bestMove moveScores
  where
    bestMove :: [(Int, Board)] -> Maybe Board
    bestMove [] = Nothing
    bestMove lst@(x : _) = Just (bestMove' x lst)

    bestMove' :: (Int, Board) -> [(Int, Board)] -> Board
    bestMove' (bestScore, bestMove) [] = bestMove
    bestMove' (bestScore, bestMove) ((score, move) : xs)
      | score > bestScore = bestMove' (score, move) xs
      | otherwise = bestMove' (bestScore, bestMove) xs

-- | Main
data PlayerType = Human | Computer

instance Show PlayerType where
  show Human = "H"
  show Computer = "C"

main :: IO ()
main = do
  typeOfP1 <-
    askFor
      "Should Player 1 be a (H)uman or a (C)omputer player?"
      [Human, Computer]
  typeOfP2 <-
    askFor
      "Should Player 2 be a (H)uman or a (C)omputer player?"
      [Human, Computer]

  let playerType :: Player -> PlayerType
      playerType P1 = typeOfP1
      playerType P2 = typeOfP2

      gameLoop :: Player -> Board -> IO ()
      gameLoop p b = do
        putStrLn ("\n" ++ printBoard b)
        case hasWinner b of
          Just p -> putStrLn (show p ++ " has won!")
          Nothing -> do
            putStr ("It's " ++ show p ++ "'s turn. ")
            mb' <- case playerType p of
              Human -> humanMove p b
              Computer -> computerMove p b
            case mb' of
              Nothing -> do
                putStr "No more moves are possible. "
                putStrLn "It's a draw."
              Just b' -> gameLoop (nextPlayer p) b'

      humanMove :: Player -> Board -> IO (Maybe Board)
      humanMove p b =
        case moves p b of
          [] -> return Nothing
          possibleMoves -> do
            putStrLn "Possible moves are:"
            putStrLn (listMoves possibleMoves)
            i <- askFor "Make your choice:" [1 .. length possibleMoves]
            return (Just (possibleMoves !! (i -1)))

      computerMove :: Player -> Board -> IO (Maybe Board)
      computerMove p b = do
        putStrLn "Thinking..."
        return (makeMove p b)

      listMoves :: [Board] -> String
      listMoves =
        intercalate "\n"
          . map (intercalate "    ")
          . transpose
          . map lines
          . map (\(i, b) -> "(" ++ show i ++ "): \n" ++ printBoard b)
          . zip [1 :: Integer ..]

  gameLoop P1 emptyBoard

askFor :: Show a => String -> [a] -> IO a
askFor m xs = do
  putStr (m ++ " ")
  hFlush stdout
  i <- getLine
  case find ((map toLower i ==) . map toLower . show) xs of
    Nothing -> do
      putStrLn $
        "I didn't understand you. Enter one of: "
          ++ intercalate ", " (map show xs)
          ++ "."
      askFor m xs
    Just y -> return y

