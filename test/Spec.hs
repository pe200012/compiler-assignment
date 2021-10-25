{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Comonad
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Stream

prop_functorStream :: Property
prop_functorStream = property $ do
    x     <- forAll $ Gen.int (Range.linear 0 100)
    times <- forAll $ Gen.int (Range.linear 0 100)
    let f = \x d -> case d of
            LEFT  -> x - 1
            RIGHT -> x + 1
        s = createStream x f
    current (iterate getRight (fmap succ s) !! times) === 1 + current (iterate getRight s !! times)
    current (iterate getLeft (pred <$> s) !! times) === current (iterate getLeft s !! times) - 1

prop_comonadStream :: Property
prop_comonadStream = property $ do
    let f = \x d -> case d of
            LEFT  -> x - 1
            RIGHT -> x + 1
        s = createStream 1 f
    extract s === 1
    extract (extend extract s) === 1
    extract (extract (extend getLeft s)) === 0


prop_staticStream :: Property
prop_staticStream = property $ do
    x     <- forAll $ Gen.int (Range.linear 0 100)
    y     <- forAll $ Gen.int (Range.linear 0 100)
    times <- forAll $ Gen.int (Range.linear 1 100)
    let f = const (const x)
        s = createStream x f
        t = pushRight (x + y) s
        w = pushLeft (x + y) s
    current (iterate getRight s !! times) === x
    current (iterate getLeft s !! times) === x
    current (setCurrent y s) === y
    current (iterate getLeft (iterate getRight (setCurrent 0 s) !! 1) !! 1) === 0
    current (iterate getLeft (iterate getRight (setCurrent 0 s) !! 2) !! 2) === 0
    current (iterate getLeft (iterate getRight (setCurrent 0 s) !! times) !! times) === 0
    current (iterate getRight (iterate getLeft (setCurrent 0 s) !! times) !! times) === 0
    current t === x + y
    current (getRight t) === x
    current (getLeft t) === x
    current (getRight (getLeft t)) === x + y
    current (getLeft (getRight t)) === x + y
    current (getLeft (getLeft t)) === x
    current w === x + y
    current (getRight w) === x
    current (getLeft w) === x
    current (getRight (getLeft w)) === x + y
    current (getLeft (getRight w)) === x + y
    current (getRight (getRight w)) === x

prop_dynamicStream :: Property
prop_dynamicStream = property $ do
    x     <- forAll $ Gen.int (Range.linear 0 100)
    times <- forAll $ Gen.int (Range.linear 0 100)
    let f = \x d -> case d of
            LEFT  -> x - 1
            RIGHT -> x + 1
        s = createStream x f
    current (iterate getRight s !! times) === x + times
    current (iterate getLeft s !! times) === x - times

tests :: IO Bool
tests = checkParallel $$(discover)

main :: IO ()
main = tests >> pure ()
