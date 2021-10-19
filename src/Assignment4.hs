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
  show = undefined

data Suit = S | H | D | C
  deriving (Bounded, Enum, Eq, Ord, Show)

data Card = Card {rank :: Rank, suit :: Suit}
  deriving (Eq, Ord)

-- * Exercise 7

instance Show Card where
  show (Card {rank = r, suit = s}) = undefined

type Deck = [Card]

-- * Exercise 8

fullDeck, piquetDeck :: Deck
fullDeck = undefined
piquetDeck = undefined

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
sameSuits = undefined

-- * Exercise 10

isStraight :: [Rank] -> Maybe Rank
isStraight = undefined

-- * Exercise 11

ranks :: Hand -> [Rank]
ranks = undefined

-- * Exercise 12

order :: Hand -> [(Int, Rank)]
order = undefined

-- * Exercise 13

handCategory :: Hand -> HandCategory
handCategory = undefined

-- * Exercise 14

instance Ord Hand where
  compare = undefined

-- * Exercise 15

combs :: Int -> [a] -> [[a]]
combs = undefined

-- * Exercise 16

allHands :: Deck -> [Hand]
allHands = undefined

-- * Exercise 17

distinctHands :: Deck -> Set Hand
distinctHands = undefined

-- * Question 1

{- ANSWER -}

-- * Question 2

{- ANSWER -}
