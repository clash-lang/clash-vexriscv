module Tests.VexRiscv.Random where

import Clash.Prelude

import Control.Monad
import Data.Bits
import Hedgehog
import Numeric.Natural
import Test.Tasty
import Test.Tasty.Hedgehog

import VexRiscv.Random

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
tests = testGroup "VexRiscv.Random"
  [ testProperty "genNatural" prop_genNatural
  ]

prop_genNatural :: Property
prop_genNatural = property $ do
  lo <- forAll $ Gen.integral (Range.linear 0 (shiftL 1 1024))
  hi <- forAll $ Gen.integral (Range.linear lo (shiftL 1 1024))
  n <- evalIO $ genNatural (lo, hi)
  assert ((n >= lo) && (n <= hi))
