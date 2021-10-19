module Pain where

data Deque a
  = Empty
  | Single a
  | Multiple (Access a) (Deque (a, a)) (Access a)
  deriving (Show, Eq)

data Access a = One a | Two a a deriving (Show, Eq)

mySmallDeque :: Deque Int
mySmallDeque =
  Multiple
    (One 1)
    (Single (2, 3))
    (Two 4 5)

myDeque :: Deque Int
myDeque =
  Multiple
    (Two 1 2)
    ( Multiple
        (One (3, 4))
        (Single ((5, 6), (7, 8)))
        (One (9, 10))
    )
    (One 11)

-- a

dequeToList :: Deque a -> [a]
dequeToList Empty = []
dequeToList (Single a) = [a]
dequeToList (Multiple l q r) = accessToList l ++ flatten (dequeToList q) ++ accessToList r
  where
    flatten :: [(a, a)] -> [a]
    flatten [] = []
    flatten ((a, b) : xs) = [a, b] ++ flatten xs

    accessToList (One a) = [a]
    accessToList (Two a b) = [a, b]

-- b

safeLast :: Deque a -> Maybe a
safeLast Empty = Nothing
safeLast (Single a) = Just a
safeLast (Multiple l _ r) = Just $ lastAccess r
  where
    lastAccess (One a) = a
    lastAccess (Two a b) = b

-- c

-- cons :: a -> Deque a -> Deque a
-- cons x Empty = Single x
-- cons x (Single a) = Multiple (One x) Empty (One a)
-- cons x (Multiple l q r) = undefined

cons :: a -> Deque a -> Deque a
cons a Empty = Single a
cons a (Single b) = Multiple (One a) Empty (One b)
cons a (Multiple (One b) d e) = Multiple (Two a b) d e
cons a (Multiple (Two b c) d e) = Multiple (One a) (cons (b, c) d) e

--                                                   ^^ I don't get this `cons` here.

-- d -- you may assume Access is already an instance of Functor
-- fmap :: Functor f => (a -> b) -> f a -> f b

instance Functor Access where
  fmap f (One a) = One (f a)
  fmap f (Two a a') = Two (f a) (f a')

instance Functor Deque where
  fmap f Empty = Empty
  fmap f (Single b) = Single $ f b
  fmap f (Multiple l q r) = Multiple (fmap f l) (fmap ff q) (fmap f r)
    where
      ff (x, y) = (f x, f y)
