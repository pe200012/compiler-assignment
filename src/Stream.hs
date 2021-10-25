{-# LANGUAGE DeriveFunctor #-}

module Stream
    ( Stream(..)
    , Generator(..)
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
    deriving Functor

instance Comonad Stream where
    extract (Bi _ a _) = a
    extend f (Bi l a r) = Bi (extend f l) (f (Bi l a r)) (extend f r)

newtype Generator a = Generator { runGenerator :: a -> Direction -> (a, Generator a) }

data Direction = LEFT | RIGHT

-- | create a stream from an element and a generator function
createStream :: a -> Generator a -> Stream a
createStream x f = Bi l x r
  where
    r = setLeft (pushLeft x l) (uncurry createStream (runGenerator f x RIGHT))
    l = setRight (pushRight x r) (uncurry createStream (runGenerator f x LEFT))

-- | take current element from stream
current :: Stream a -> a
current (Bi _ x _) = x

-- | set current element of stream
setCurrent :: a -> Stream a -> Stream a
setCurrent x (Bi l _ r) = Bi l x r -- FIXME: this is not correct

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
pushRight newX (Bi l x r) = Bi (setRight (pushRight newX r1) l) newX r1 where r1 = pushRight x r

-- | given a new element i, return (l:>x) i r where l is the left stream and r is the right stream
-- and x is the original element
pushLeft :: a -> Stream a -> Stream a
pushLeft newX (Bi l x r) = Bi l1 newX (setLeft (pushLeft newX l1) r) where l1 = pushLeft x l


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
