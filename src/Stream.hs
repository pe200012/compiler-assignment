{-# LANGUAGE DeriveFunctor #-}

module Stream
    ( Stream
    , Direction(..)
    , createStream
    , current
    , setCurrent
    , getRight
    , getLeft
    , pushRight
    , pushLeft
    ) where

import           Control.Comonad                ( Comonad(..) )

data Stream a = Bi (Stream a) a (Stream a)

instance Functor Stream where
    fmap f (Bi l x r) = Bi (fmap f l) (f x) (fmap f r)

instance Comonad Stream where
    extract (Bi _ a _) = a
    extend f (Bi l a r) = Bi (extend f l) (f (Bi l a r)) (extend f r)

data Direction = LEFT | RIGHT

-- | create a stream from an element and a generator function
createStream :: a -> (a -> Direction -> a) -> Stream a
createStream x f = Bi (extend (flip f LEFT . extract) self) x (extend (flip f RIGHT . extract) self) where self = createStream x f

-- | take current element from stream
current :: Stream a -> a
current (Bi _ x _) = x

-- | set current element of stream
setCurrent :: a -> Stream a -> Stream a
setCurrent x (Bi l _ r) = self where self = Bi (cngL (setRight self l)) x (cngR (setLeft self r))

cngL :: Stream a -> Stream a
cngL (Bi l' x' r') = self where self = Bi (cngL (setRight self l')) x' r'

cngR :: Stream a -> Stream a
cngR (Bi l' x' r') = self where self = Bi l' x' (cngR (setLeft self r'))

-- | get right stream
getRight :: Stream a -> Stream a
getRight (Bi _ _ r) = r

-- | get left stream
getLeft :: Stream a -> Stream a
getLeft (Bi l _ _) = l

setRight :: Stream a -> Stream a -> Stream a
setRight r (Bi l x _) = Bi l x r

setLeft :: Stream a -> Stream a -> Stream a
setLeft l (Bi _ x r) = Bi l x r

-- | given a new element i, return l i (x<:r) where l is the left stream and r is the right stream
-- and x is the original element
pushRight :: a -> Stream a -> Stream a
pushRight newX (Bi l x r) = self
  where
    self = Bi l1 newX r1
    r1   = setLeft self (pushRight x r)
    l1   = cngL (setRight self l)

-- | given a new element i, return (l:>x) i r where l is the left stream and r is the right stream
-- and x is the original element
pushLeft :: a -> Stream a -> Stream a
pushLeft newX (Bi l x r) = self
  where
    self = Bi l1 newX r1
    l1   = setRight self (pushLeft x l)
    r1   = cngR (setLeft self r)

-- | augmenting int stream, i.e. ... -1 0 1 2 3 4 5 6 ...
intStream :: Stream Int
intStream = createStream
    0
    (\x d -> case d of
        LEFT  -> x - 1
        RIGHT -> x + 1
    )

-- | create a stream from a list, the first element is the current element
-- and the rest are the elements in the right direction
fromList :: [a] -> Stream (Maybe a)
fromList [] = createStream Nothing (const (const Nothing))
fromList xs = (\i -> if i < 0 || i >= length xs then Nothing else Just (xs !! i)) <$> intStream

{-

>>> f = Generator (const (const (0, f)))

>>> current (createStream 1 f)
1

>>> current (getLeft (createStream 1 f))
0

>>> current (getRight (createStream 1 f))
0

>>> current (getRight (getLeft (createStream 1 f)))
1

>>> g = Generator $ \x d -> case d of { LEFT -> (x-1, g); RIGHT -> (x+1, g) }

>>> current (createStream 1 g)
1

>>> current (getLeft (createStream 1 g))
0

>>> current (getLeft (getLeft (createStream 1 g)))
-1

>>> current (getRight (createStream 1 g))
2

>>> current (getRight (getRight (createStream 1 g)))
3

>>> current (getRight (getLeft (createStream 1 g)))
1

>>> current (getRight (getLeft (setCurrent 2 (createStream 1 g))))
2

-}
