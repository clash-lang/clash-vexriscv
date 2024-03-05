-- SPDX-FileCopyrightText: 2022-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module VexRiscv where

import Clash.Prelude

import Clash.Annotations.Primitive
import Clash.Signal.Internal
import Data.Bifunctor (first)
import Data.String.Interpolate (__i)
import Data.Word (Word64)
import Foreign (Ptr)
import Foreign.Marshal (alloca)
import Foreign.Storable
import GHC.IO (unsafePerformIO)
import GHC.Stack (HasCallStack)
import Language.Haskell.TH.Syntax
import Protocols.Wishbone

import VexRiscv.BlackBox (vexRiscvBBF)
import VexRiscv.ClockTicks
import VexRiscv.FFI
import VexRiscv.TH

import qualified VexRiscv.FFI as FFI

data Input = Input
  { timerInterrupt :: "TIMER_INTERRUPT" ::: Bit
  , externalInterrupt :: "EXTERNAL_INTERRUPT" ::: Bit
  , softwareInterrupt :: "SOFTWARE_INTERRUPT" ::: Bit
  , iBusWbS2M :: "IBUS_IN_" ::: WishboneS2M (BitVector 32)
  , dBusWbS2M :: "DBUS_IN_" ::: WishboneS2M (BitVector 32)
  }
  deriving (Generic, NFDataX, ShowX, Eq, BitPack)

inputToNonCombInput :: Bool -> Input -> NON_COMB_INPUT
inputToNonCombInput reset Input{..} = NON_COMB_INPUT
  { FFI.reset = boolToBit reset
  , FFI.timerInterrupt = timerInterrupt
  , FFI.externalInterrupt = externalInterrupt
  , FFI.softwareInterrupt = softwareInterrupt
  }

inputToCombInput :: Input -> COMB_INPUT
inputToCombInput Input{iBusWbS2M, dBusWbS2M} = makeDefined $ COMB_INPUT
  { FFI.iBusWishbone_ACK = boolToBit (acknowledge iBusWbS2M)
  , FFI.iBusWishbone_DAT_MISO = unpack (readData iBusWbS2M)
  , FFI.iBusWishbone_ERR = boolToBit (err iBusWbS2M)

  , FFI.dBusWishbone_ACK = boolToBit (acknowledge dBusWbS2M)
  , FFI.dBusWishbone_DAT_MISO = unpack (readData dBusWbS2M)
  , FFI.dBusWishbone_ERR = boolToBit (err dBusWbS2M)
  }

data Output = Output
  { iBusWbM2S :: "IBUS_OUT_" ::: WishboneM2S 30 4 (BitVector 32)
  , dBusWbM2S :: "DBUS_OUT_" ::: WishboneM2S 30 4 (BitVector 32)
  }
  deriving (Generic, NFDataX, ShowX, Eq, BitPack)

outputToOutput :: OUTPUT -> Output
outputToOutput OUTPUT{..} = Output
  { iBusWbM2S = WishboneM2S
    { addr = truncateB (pack iBusWishbone_ADR)
    , writeData = pack (iBusWishbone_DAT_MOSI)
    , busSelect = unpack (truncateB (pack iBusWishbone_SEL))
    , lock = False
    , busCycle = bitToBool iBusWishbone_CYC
    , strobe = bitToBool iBusWishbone_STB
    , writeEnable = bitToBool iBusWishbone_WE
    , cycleTypeIdentifier = unpack (truncateB (pack iBusWishbone_CTI))
    , burstTypeExtension = unpack (truncateB (pack iBusWishbone_BTE))
    }
  , dBusWbM2S = WishboneM2S
    { addr = truncateB (pack dBusWishbone_ADR)
    , writeData = pack (dBusWishbone_DAT_MOSI)
    , busSelect = unpack (truncateB (pack dBusWishbone_SEL))
    , lock = False
    , busCycle = bitToBool dBusWishbone_CYC
    , strobe = bitToBool dBusWishbone_STB
    , writeEnable = bitToBool dBusWishbone_WE
    , cycleTypeIdentifier = unpack (truncateB (pack dBusWishbone_CTI))
    , burstTypeExtension = unpack (truncateB (pack dBusWishbone_BTE))
    }
  }

-- When passing S2M values from Haskell to VexRiscv over the FFI, undefined
-- bits/values cause errors when forcing their evaluation to something that can
-- be passed through the FFI.
--
-- This function makes sure the Wishbone S2M values are free from undefined bits.
makeDefined :: COMB_INPUT -> COMB_INPUT
makeDefined ci@COMB_INPUT{iBusWishbone_DAT_MISO, dBusWishbone_DAT_MISO} = ci
  { FFI.iBusWishbone_DAT_MISO = defaultX 0 iBusWishbone_DAT_MISO
  , FFI.dBusWishbone_DAT_MISO = defaultX 0 dBusWishbone_DAT_MISO
  }

defaultX :: (NFDataX a) => a -> a -> a
defaultX dflt val
  | hasUndefined val = dflt
  | otherwise = val

vexRiscv :: (HasCallStack, HiddenClockResetEnable dom) => Signal dom Input -> Signal dom Output
vexRiscv = vexRiscv# sourcePath hasClock hasReset
  where
    sourcePath = $(do
      cpuSrcPath <- runIO $ getPackageRelFilePath "example-cpu/VexRiscv.v"
      pure $ LitE $ StringL cpuSrcPath)

vexRiscv#
  :: KnownDomain dom
  => String
  -> Clock dom
  -> Reset dom
  -> Signal dom Input
  -> Signal dom Output
vexRiscv# !_sourcePath clk rst input =
  fmap outputToOutput $
    simInitThenCycles
      (inputToNonCombInput <$> unsafeToActiveHigh rst <*> input)
      (inputToCombInput <$> input)
 where
  (v, initStage1, initStage2, stepRising, stepFalling, _shutDown) = unsafePerformIO vexCPU

  simInitThenCycles ::
    Signal dom NON_COMB_INPUT ->
    Signal dom COMB_INPUT ->
    Signal dom OUTPUT
  simInitThenCycles (cnc :- cncs) ~(cc :- ccs) =
    let
      -- Note: we don't need @ticks@ for the initialization stages, because this
      -- first cycle of a 'Signal' is meant to model what happens _before_ a
      -- clock edge.
      out0 = unsafePerformIO (initStage1 v cnc)
      stage2Out = unsafePerformIO (initStage2 v cc)
      ticks = first (fromInteger . toInteger) <$> singleClockEdgesRelative clk
      out1 = simCycles ticks cncs ccs
    in
      out0 :- (out0 `seq` (stage2Out `seq` out1))

  simCycles ::
    [(Word64, ActiveEdge)] ->
    Signal dom NON_COMB_INPUT ->
    Signal dom COMB_INPUT ->
    Signal dom OUTPUT
  simCycles ((fsSinceLastEvent, Rising) : ts) (cnc :- cncs) ccs =
    let
      out0 = unsafePerformIO (stepRising v fsSinceLastEvent cnc)
      out1 = simCycles ts cncs ccs
    in
      out0 :- (out0 `seq` out1)

  simCycles ((fsSinceLastEvent, Falling) : ts) cncs (cc :- ccs) =
    let !() = unsafePerformIO (stepFalling v fsSinceLastEvent cc)
    in  simCycles ts cncs ccs

  simCycles [] _ _ = error "Empty ticks: should never happen"
{-# CLASH_OPAQUE vexRiscv# #-}
{-# ANN vexRiscv# hasBlackBox #-}
{-# ANN vexRiscv# (
    let primName = 'vexRiscv#
        tfName = 'vexRiscvBBF
    in InlineYamlPrimitive [Verilog] [__i|
         BlackBoxHaskell:
             name: #{primName}
             templateFunction: #{tfName}
             workInfo: Always
         |]) #-}

-- | Return a function that performs an execution step and a function to free
-- the internal CPU state
vexCPU :: IO
  ( Ptr VexRiscv
  , Ptr VexRiscv -> NON_COMB_INPUT -> IO OUTPUT           -- initStage1
  , Ptr VexRiscv -> COMB_INPUT -> IO ()                   -- initStage2
  , Ptr VexRiscv -> Word64 -> NON_COMB_INPUT -> IO OUTPUT -- rising
  , Ptr VexRiscv -> Word64 -> COMB_INPUT -> IO ()         -- falling
  , Ptr VexRiscv -> IO ()
  )
vexCPU = do
  v <- vexrInit

  let
    {-# NOINLINE initStage1 #-}
    initStage1 vPtr nonCombInput =
      alloca $ \nonCombInputFFI -> alloca $ \outputFFI -> do
        poke nonCombInputFFI nonCombInput
        vexrInitStage1 vPtr nonCombInputFFI outputFFI
        output <- peek outputFFI
        pure output

    {-# NOINLINE initStage2 #-}
    initStage2 vPtr combInput =
      alloca $ \combInputFFI -> do
        poke combInputFFI combInput
        vexrInitStage2 vPtr combInputFFI

    {-# NOINLINE stepRising #-}
    stepRising vPtr fsSinceLastEvent nonCombInput =
      alloca $ \nonCombInputFFI -> alloca $ \outputFFI -> do
        poke nonCombInputFFI nonCombInput
        vexrStepRisingEdge vPtr fsSinceLastEvent nonCombInputFFI outputFFI
        output <- peek outputFFI
        pure output

    {-# NOINLINE stepFalling #-}
    stepFalling vPtr fsSinceLastEvent combInput =
      alloca $ \combInputFFI -> do
        poke combInputFFI combInput
        vexrStepFallingEdge vPtr fsSinceLastEvent combInputFFI

    shutDown = vexrShutdown

  pure (v, initStage1, initStage2, stepRising, stepFalling, shutDown)
