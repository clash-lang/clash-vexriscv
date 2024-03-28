module VexRiscv.Random where

import Clash.Prelude
import Numeric.Natural
import Clash.Sized.Internal.BitVector
import System.Random

class DefinedRandom a where
  makeDefinedRandom :: a -> IO a

instance DefinedRandom Bool where
  makeDefinedRandom b
    | hasUndefined b = randomIO
    | otherwise = pure b

instance DefinedRandom Bit where
  makeDefinedRandom b@(Bit 0 _) = pure b
  makeDefinedRandom _ = do
    d <- randomRIO (0, 1)
    pure (Bit 1 d)

instance KnownNat n => DefinedRandom (BitVector n) where
  makeDefinedRandom :: KnownNat n => BitVector n -> IO (BitVector n)
  makeDefinedRandom (BV mask dat) = do
    let
      (BV _ maxVal) = (maxBound :: BitVector n)
      (BV maxMask _) = deepErrorX "" :: BitVector n
    randomInt <- genNatural (0, fromIntegral maxVal)

    pure $ BV 0 ((dat .&. (maxMask `xor` mask)) .|. (randomInt .&. mask))

genNatural :: (Natural, Natural) -> IO Natural
genNatural (lo, hi)
  | lo > hi = error "genNatural: lower bound > upper bound"
  | otherwise = (lo +) <$> go (hi - lo)
 where
  intMax = fromIntegral (maxBound :: Int)
  intBits = finiteBitSize (0 :: Int)

  go :: Natural -> IO Natural
  go h
    | h <= intMax =  fmap fromIntegral (randomRIO (0, fromIntegral h) :: IO Int)
    | otherwise = do
    x <- fmap fromIntegral (randomRIO (0, maxBound) :: IO Int)
    y <- go (shiftR h intBits)
    pure (x + shiftL y intBits)
