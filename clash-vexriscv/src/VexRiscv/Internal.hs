-- SPDX-FileCopyrightText: 2022-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
{-# OPTIONS_HADDOCK hide #-}

module VexRiscv.Internal where

import Clash.Prelude
import Protocols
import Protocols.Idle
import Protocols.Wishbone

import VexRiscv.FFI
import VexRiscv.Random (unsafeMakeDefinedRandom)

import Clash.Annotations.Primitive (HDL (Verilog), Primitive (InlineYamlPrimitive), hasBlackBox)
import Clash.Signal.Internal (Signal ((:-)), hzToFs, unFemtoseconds)
import Data.Bifunctor (first)
import Data.String.Interpolate (i)
import Data.Word (Word64)
import Foreign.C.String (CString, newCString)
import Foreign.Marshal (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)
import GHC.IO (unsafeInterleaveIO, unsafePerformIO)
import GHC.Stack (HasCallStack)
import VexRiscv.BlackBox (vexRiscvBBF)
import VexRiscv.ClockTicks (singleClockEdgesRelative)
import VexRiscv.Reset (MinCyclesReset, fromMinCycles)

-- Opaque types for Verilator C++ model
data VexRiscv
data VerilatedVcdC

data JtagIn = JtagIn
  { testClock :: "TCK" ::: Bit
  , testModeSelect :: "TMS" ::: Bit
  , testDataIn :: "TDI" ::: Bit
  }
  deriving (Generic, Eq, NFDataX, ShowX, BitPack)

data JtagOut = JtagOut
  { testDataOut :: "TDO" ::: Bit
  }
  deriving (Generic, NFDataX, ShowX, Eq, BitPack)

data CpuIn = CpuIn
  { timerInterrupt :: "TIMER_INTERRUPT" ::: Bit
  , externalInterrupt :: "EXTERNAL_INTERRUPT" ::: Bit
  , softwareInterrupt :: "SOFTWARE_INTERRUPT" ::: Bit
  , iBusWbS2M :: "IBUS_IN_" ::: WishboneS2M 4
  , dBusWbS2M :: "DBUS_IN_" ::: WishboneS2M 4
  }
  deriving (Generic, NFDataX, ShowX, Eq, BitPack)

data CpuOut = CpuOut
  { iBusWbM2S :: "IBUS_OUT_" ::: WishboneM2S 30 4
  , dBusWbM2S :: "DBUS_OUT_" ::: WishboneM2S 30 4
  , ndmreset :: "RST" ::: Bit
  -- ^ Peripheral reset produced by `EmbeddedRiscvJtag` plugin.
  , stoptime :: "STOPTIME" ::: Bit
  -- ^ Seems to be some kind of debug signal produced by the `CsrPlugin`.
  }
  deriving (Generic, NFDataX, ShowX, Eq, BitPack)

inputToNonCombInput :: Bool -> CpuIn -> NON_COMB_INPUT
inputToNonCombInput rst cpuIn =
  NON_COMB_INPUT
    { reset = boolToBit (unsafeMakeDefinedRandom rst)
    , timerInterrupt = unsafeMakeDefinedRandom timerInterrupt
    , externalInterrupt = unsafeMakeDefinedRandom externalInterrupt
    , softwareInterrupt = unsafeMakeDefinedRandom softwareInterrupt
    }
 where
  CpuIn{timerInterrupt, externalInterrupt, softwareInterrupt} = cpuIn

inputToCombInput :: CpuIn -> JtagIn -> COMB_INPUT
inputToCombInput cpuIn jtagIn =
  COMB_INPUT
    { iBusWishbone_ACK = boolToBit (unsafeMakeDefinedRandom iBusAck)
    , iBusWishbone_DAT_MISO = unpack (unsafeMakeDefinedRandom iBusDat)
    , iBusWishbone_ERR = boolToBit (unsafeMakeDefinedRandom iBusErr)
    , dBusWishbone_ACK = boolToBit (unsafeMakeDefinedRandom dBusAck)
    , dBusWishbone_DAT_MISO = unpack (unsafeMakeDefinedRandom dBusDat)
    , dBusWishbone_ERR = boolToBit (unsafeMakeDefinedRandom dBusErr)
    , jtag_TCK = (unsafeMakeDefinedRandom testClock)
    , jtag_TMS = (unsafeMakeDefinedRandom testModeSelect)
    , jtag_TDI = (unsafeMakeDefinedRandom testDataIn)
    }
 where
  CpuIn{iBusWbS2M, dBusWbS2M} = cpuIn
  JtagIn{testClock, testModeSelect, testDataIn} = jtagIn
  WishboneS2M{acknowledge = iBusAck, readData = iBusDat, err = iBusErr} = iBusWbS2M
  WishboneS2M{acknowledge = dBusAck, readData = dBusDat, err = dBusErr} = dBusWbS2M

outputToCpuOut :: OUTPUT -> CpuOut
outputToCpuOut OUTPUT{..} =
  CpuOut
    { iBusWbM2S =
        WishboneM2S
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
    , dBusWbM2S =
        WishboneM2S
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
    , ndmreset = ndmreset
    , stoptime = stoptime
    }

outputToJtagOut :: OUTPUT -> JtagOut
outputToJtagOut OUTPUT{jtag_TDO} = JtagOut{testDataOut = jtag_TDO}

data DumpVcd = DumpVcd FilePath | NoDumpVcd

data Jtag (dom :: Domain)

instance Protocol (Jtag dom) where
  type Fwd (Jtag dom) = Signal dom JtagIn
  type Bwd (Jtag dom) = Signal dom JtagOut

instance IdleCircuit (Jtag dom) where
  idleFwd _ = pure $ JtagIn 0 0 0
  idleBwd _ = pure $ JtagOut 0

{- | Generic simulation function that takes FFI function pointers
This allows different generated modules to use the same simulation logic
-}
vexRiscvSim ::
  forall dom.
  (HasCallStack, KnownDomain dom) =>
  -- | c_init
  IO (Ptr VexRiscv) ->
  -- | c_init_vcd
  (Ptr VexRiscv -> CString -> IO (Ptr VerilatedVcdC)) ->
  -- | c_init_stage1
  (Ptr VerilatedVcdC -> Word64 -> Ptr VexRiscv -> Ptr NON_COMB_INPUT -> Ptr OUTPUT -> IO ()) ->
  -- | c_init_stage2
  (Ptr VexRiscv -> Ptr COMB_INPUT -> IO ()) ->
  -- | c_step_rising
  (Ptr VerilatedVcdC -> Ptr VexRiscv -> Word64 -> Ptr NON_COMB_INPUT -> Ptr OUTPUT -> IO ()) ->
  -- | c_step_falling
  (Ptr VerilatedVcdC -> Ptr VexRiscv -> Word64 -> Ptr COMB_INPUT -> IO ()) ->
  -- Simulation inputs
  DumpVcd ->
  Clock dom ->
  MinCyclesReset dom 2 ->
  Signal dom CpuIn ->
  Signal dom JtagIn ->
  -- Simulation outputs
  (Signal dom CpuOut, Signal dom JtagOut)
vexRiscvSim c_init c_init_vcd c_init_stage1 c_init_stage2 c_step_rising c_step_falling dumpVcd clk rst cpuInput jtagInput = unsafePerformIO $ do
  let
    domPeriodFs = hzToFs (natToNum @(PeriodToHz (Max 1 (DomainPeriod dom))))
    domPeriodFsWord64 = fromIntegral (unFemtoseconds domPeriodFs) :: Word64

  -- Initialize the CPU
  v <- c_init
  vcd <- case dumpVcd of
    NoDumpVcd -> pure nullPtr
    DumpVcd path -> do
      vcdPath <- newCString path
      c_init_vcd v vcdPath

  let
    initStage1 vPtr nonCombInput =
      alloca $ \nonCombInputFFI -> alloca $ \outputFFI -> do
        poke nonCombInputFFI nonCombInput
        c_init_stage1 vcd domPeriodFsWord64 vPtr nonCombInputFFI outputFFI
        peek outputFFI
    {-# NOINLINE initStage1 #-}

    initStage2 vPtr combInput =
      alloca $ \combInputFFI -> do
        poke combInputFFI combInput
        c_init_stage2 vPtr combInputFFI
    {-# NOINLINE initStage2 #-}

    stepRising vPtr fsSinceLastEvent nonCombInput =
      alloca $ \nonCombInputFFI -> alloca $ \outputFFI -> do
        poke nonCombInputFFI nonCombInput
        c_step_rising vcd vPtr fsSinceLastEvent nonCombInputFFI outputFFI
        peek outputFFI
    {-# NOINLINE stepRising #-}

    stepFalling vPtr fsSinceLastEvent combInput =
      alloca $ \combInputFFI -> do
        poke combInputFFI combInput
        c_step_falling vcd vPtr fsSinceLastEvent combInputFFI
    {-# NOINLINE stepFalling #-}

    simInitThenCycles :: Signal dom NON_COMB_INPUT -> Signal dom COMB_INPUT -> IO (Signal dom OUTPUT)
    simInitThenCycles (cnc :- cncs) ~(cc :- ccs) = do
      let ticks = first fromIntegral <$> singleClockEdgesRelative clk
      out0 <- initStage1 v cnc
      stage2Out <- unsafeInterleaveIO (initStage2 v cc)
      out1 <- unsafeInterleaveIO (simCycles ticks cncs ccs)
      pure $ out0 :- (stage2Out `seq` out1)

    simCycles :: [(Word64, ActiveEdge)] -> Signal dom NON_COMB_INPUT -> Signal dom COMB_INPUT -> IO (Signal dom OUTPUT)
    simCycles ((fsSinceLastEvent, Rising) : ts) (cnc :- cncs) ccs = do
      out0 <- stepRising v fsSinceLastEvent cnc
      out1 <- unsafeInterleaveIO (simCycles ts cncs ccs)
      pure $ out0 :- out1
    simCycles ((fsSinceLastEvent, Falling) : ts) cncs (cc :- ccs) = do
      stepFalling v fsSinceLastEvent cc
      simCycles ts cncs ccs
    simCycles [] _ _ = error "Empty ticks: should never happen"

  output <-
    simInitThenCycles
      (inputToNonCombInput <$> unsafeToActiveHigh (fromMinCycles rst) <*> cpuInput)
      (inputToCombInput <$> cpuInput <*> jtagInput)
  pure
    ( outputToCpuOut <$> output
    , outputToJtagOut <$> output
    )
{-# INLINE vexRiscvSim #-}

vexRiscvSynth ::
  -- | Verilog module name
  String ->
  -- | Verilog source
  String ->
  Clock dom ->
  MinCyclesReset dom 2 ->
  Signal dom CpuIn ->
  Signal dom JtagIn ->
  (Signal dom CpuOut, Signal dom JtagOut)
vexRiscvSynth !_ !_ !_ !_ !_ !_ = (error msg, error msg)
 where
  msg = "vexRiscvSynth: not implemented"
{-# OPAQUE vexRiscvSynth #-}
{-# ANN vexRiscvSynth hasBlackBox #-}
{-# ANN
  vexRiscvSynth
  ( let primName = 'vexRiscvSynth
        tfName = 'vexRiscvBBF
     in InlineYamlPrimitive
          [Verilog]
          [i|BlackBoxHaskell:
              name: #{primName}
              templateFunction: #{tfName}
              workInfo: Always
         |]
  )
  #-}
