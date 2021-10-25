{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Comonad
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Stream

prop_comonadStream :: Property
prop_comonadStream = property $ do
    let f = Generator $ \x d -> case d of
            LEFT  -> (x - 1, f)
            RIGHT -> (x + 1, f)
        s = createStream 1 f
    extract s === 1
    extract (extend extract s) === 1

prop_staticStream :: Property
prop_staticStream = property $ do
    x     <- forAll $ Gen.int (Range.linear 0 100)
    y     <- forAll $ Gen.int (Range.linear 0 100)
    times <- forAll $ Gen.int (Range.linear 1 100)
    let f = Generator (const (const (x, f)))
        s = createStream x f
    current (iterate getRight s !! times) === x
    current (iterate getLeft s !! times) === x
    current (setCurrent y s) === y
    current (iterate getLeft (iterate getRight (setCurrent 0 s) !! 1) !! 1) === 0
    current (iterate getLeft (iterate getRight (setCurrent 0 s) !! 2) !! 2) === 0
    current (iterate getLeft (iterate getRight (setCurrent 0 s) !! times) !! times) === 0

prop_dynamicStream :: Property
prop_dynamicStream = property $ do
    x     <- forAll $ Gen.int (Range.linear 0 100)
    times <- forAll $ Gen.int (Range.linear 0 100)
    let f = Generator $ \x d -> case d of
            LEFT  -> (x - 1, f)
            RIGHT -> (x + 1, f)
        s = createStream x f
    current (iterate getRight s !! times) === x + times
    current (iterate getLeft s !! times) === x - times

tests :: IO Bool
tests = checkParallel $$(discover)

main :: IO ()
main = tests >> pure ()
