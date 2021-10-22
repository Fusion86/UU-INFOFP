{-# LANGUAGE CPP #-}

{- This is a framework in which all functions to be written are "undefined".  -
 - Note that in most cases parameters, pattern-matching and guards have been  -
 - omitted! You will have to add those yourself.                              -}

module Assignment4 where

#if __GLASGOW_HASKELL__ >= 804
import Prelude hiding (Monoid, mempty, foldMap, Foldable, (<>))
#elif __GLASGOW_HASKELL__ >= 710
import Prelude hiding (Monoid, mempty, foldMap, Foldable)
#endif

import Data.List (foldl', group, sort)
import Data.Set (Set, empty, insert)

-- | Containers
data Rose a = MkRose a [Rose a]
  deriving (Eq, Show)

-- * Exercise 1

instance Functor Rose where
  fmap f (MkRose x xs) = MkRose (f x) (map (fmap f) xs)

class Monoid a where
  mempty :: a
  (<>) :: a -> a -> a

instance Monoid [a] where
  mempty = []
  (<>) = (++)

newtype Sum a = Sum {unSum :: a} deriving (Eq, Show)

newtype Product a = Product {unProduct :: a} deriving (Eq, Show)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  Sum n1 <> Sum n2 = Sum (n1 + n2)

-- * Exercise 2

instance Num a => Monoid (Product a) where
  mempty = Product 1
  Product a <> Product b = Product (a * b)

class Functor f => Foldable f where
  fold :: Monoid m => f m -> m
  foldMap :: Monoid m => (a -> m) -> f a -> m
  -- * Exercise 4
  foldMap = undefined

instance Foldable [] where
  fold = foldr (<>) mempty

-- * Exercise 3

instance Foldable Rose where
  fold (MkRose x []) = x
  fold (MkRose x (y : ys)) = x <> fold y <> foldMap fold ys

-- * Exercise 5

fsum, fproduct :: (Foldable f, Num a) => f a -> a
fsum = undefined
fproduct = undefined

-- | Poker
data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | J | Q | K | A
  deriving (Bounded, Enum, Eq, Ord)

-- * Exercise 6

instance Show Rank where
  show R2 = "2"
  show R3 = "3"
  show R4 = "4"
  show R5 = "5"
  show R6 = "6"
  show R7 = "7"
  show R8 = "8"
  show R9 = "9"
  show R10 = "10"
  show J = "J"
  show Q = "Q"
  show K = "K"
  show A = "A"

data Suit = S | H | D | C
  deriving (Bounded, Enum, Eq, Ord, Show)

data Card = Card {rank :: Rank, suit :: Suit}
  deriving (Eq, Ord)

-- * Exercise 7

instance Show Card where
  show Card {rank = r, suit = s} = show r ++ show s

type Deck = [Card]

-- * Exercise 8

fullDeck :: Deck
fullDeck = [Card r s | r <- ranks, s <- suits]
  where
    suits = [minBound :: Suit .. maxBound :: Suit]
    ranks = [minBound :: Rank .. maxBound :: Rank]

piquetDeck :: Deck
piquetDeck = filter ((>= R7) . rank) fullDeck

newtype Hand = Hand {unHand :: [Card]} deriving (Eq, Show)

data HandCategory
  = HighCard [Rank]
  | OnePair Rank [Rank]
  | TwoPair Rank Rank Rank
  | ThreeOfAKind Rank Rank Rank
  | Straight Rank
  | Flush [Rank]
  | FullHouse Rank Rank
  | FourOfAKind Rank Rank
  | StraightFlush Rank
  deriving (Eq, Ord, Show)

-- * Exercise 9

sameSuits :: Hand -> Bool
sameSuits (Hand []) = True
sameSuits (Hand (Card _ s : cs)) = all ((== s) . suit) cs

-- * Exercise 10

-- Answer is based on https://gist.github.com/halter73/750588/ee28475e82a3ffa3cc21f23a408223745733ca1f
-- because I couldn't figure it out on my own.

isStraight :: [Rank] -> Maybe Rank
isStraight cs
  | sorted == [R2, R3, R4, R5, A] = Just R5
  | sorted == take 5 [head sorted ..] = Just $ sorted !! 4
  | otherwise = Nothing
  where
    sorted = sort cs

-- * Exercise 11

ranks :: Hand -> [Rank]
ranks = reverse . sort . map rank . unHand

-- * Exercise 12

groupToTupList :: [[a]] -> [(Int, a)]
groupToTupList = map f
  where
    f :: [a] -> (Int, a)
    f x = (length x, head x)

order :: Hand -> [(Int, Rank)]
order = reverse . sort . groupToTupList . group . ranks

-- * Exercise 13

-- Terrible code, but it works.

handCategory :: Hand -> HandCategory
handCategory h@(Hand cs)
  -- Straight flush
  | Just rank <- isStraight rks, sameSuits h = StraightFlush rank
  -- Four of a kind
  | Just x <- fourOfAKind = x
  -- Full house
  | fst (head ord) == 3 && fst (last ord) == 2 = FullHouse (snd (head ord)) (snd (last ord))
  -- Flush
  | fst (head suitTupList) == 5 = Flush rks
  -- Straight
  | Just rank <- isStraight rks = Straight rank
  -- Three of a kind
  | fst (head ord) == 3 = ThreeOfAKind (snd (head ord)) (snd (ord !! 1)) (snd (last ord))
  -- Two pair
  | Just x <- twoPair = x
  -- One pair
  | Just x <- onePair = x
  -- High card
  | otherwise = HighCard rks -- TODO: Sort
  where
    rks = ranks h
    ord = order h
    sts = map suit cs
    suitTupList = groupToTupList (group sts)

    fourOfAKind
      | fst (head ord) == 4 = Just $ FourOfAKind (snd (head ord)) (snd (last ord))
      | otherwise = Nothing

    twoPair
      | length ord == 3 = Just $ TwoPair (snd $ head ord) (snd $ ord !! 1) (snd $ ord !! 2)
      | otherwise = Nothing

    onePair
      | fst (head ord) == 2 = Just $ OnePair (snd (head ord)) (map snd (drop 1 ord))
      | otherwise = Nothing

-- * Exercise 14

instance Ord Hand where
  a `compare` b = handCategory a `compare` handCategory b

-- * Exercise 15

combs :: Int -> [a] -> [[a]]
combs 0 _ = [[]]
combs _ [] = []
combs n (x : xs) = map (x :) (combs (n - 1) xs) ++ combs n xs

-- * Exercise 16

allHands :: Deck -> [Hand]
allHands = map Hand . combs 5

-- * Exercise 17

distinctHands :: Deck -> Set Hand
distinctHands = undefined

-- * Question 1

{- ANSWER -}

-- * Question 2

{- ANSWER -}
