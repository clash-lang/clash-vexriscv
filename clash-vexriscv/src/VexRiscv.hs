-- SPDX-FileCopyrightText: 2022-2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE QuasiQuotes #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module VexRiscv where

import Clash.Prelude

import Clash.Annotations.Primitive
import Clash.Signal.Internal
import Data.Bifunctor (first)
import Data.String.Interpolate (__i)
import Data.Word
import Foreign.Marshal (alloca)
import Foreign.Storable
import GHC.IO (unsafePerformIO)
import GHC.Stack (HasCallStack)
import Language.Haskell.TH.Syntax
import Protocols
import Protocols.Wishbone
import VexRiscv.ClockTicks
import VexRiscv.FFI
import VexRiscv.TH
import Foreign (Ptr)

import qualified VexRiscv.FFI as FFI

data JtagIn = JtagIn
  { testClock :: "TCK" ::: Bit
  , testModeSelect :: "TMS" ::: Bit
  , testDataIn :: "TDI" ::: Bit
  }
  deriving (Generic, Eq, NFDataX, ShowX, BitPack)

data JtagOut = JtagOut
  { testDataOut :: "TDO" ::: Bit
  , debugReset :: "RST" ::: Bit
  }
  deriving (Generic, NFDataX, ShowX, Eq, BitPack)

data CpuIn = CpuIn
  { timerInterrupt :: "TIMER_INTERRUPT" ::: Bit
  , externalInterrupt :: "EXTERNAL_INTERRUPT" ::: Bit
  , softwareInterrupt :: "SOFTWARE_INTERRUPT" ::: Bit
  , iBusWbS2M :: "IBUS_IN_" ::: WishboneS2M (BitVector 32)
  , dBusWbS2M :: "DBUS_IN_" ::: WishboneS2M (BitVector 32)
  }
  deriving (Generic, NFDataX, ShowX, Eq, BitPack)

data CpuOut = CpuOut
  { iBusWbM2S :: "IBUS_OUT_" ::: WishboneM2S 30 4 (BitVector 32)
  , dBusWbM2S :: "DBUS_OUT_" ::: WishboneM2S 30 4 (BitVector 32)
  }
  deriving (Generic, NFDataX, ShowX, Eq, BitPack)


data Jtag (dom :: Domain)

instance Protocol (Jtag dom) where
  type Fwd (Jtag dom) = Signal dom JtagOut
  type Bwd (Jtag dom) = Signal dom JtagIn

-- When passing S2M values from Haskell to VexRiscv over the FFI, undefined
-- bits/values cause errors when forcing their evaluation to something that can
-- be passed through the FFI.
--
-- This function makes sure the Wishbone S2M values are free from undefined bits.
makeDefined :: WishboneS2M (BitVector 32) -> WishboneS2M (BitVector 32)
makeDefined wb = wb {readData = defaultX 0 (readData wb)}

defaultX :: (NFDataX a) => a -> a -> a
defaultX dflt val
  | hasUndefined val = dflt
  | otherwise = val

vexRiscv ::
  forall dom .
  ( HasCallStack
  , KnownDomain dom) =>
  Clock dom ->
  Reset dom ->
  Signal dom CpuIn ->
  Signal dom JtagIn ->
  ( Signal dom CpuOut
  , Signal dom JtagOut
  )
vexRiscv clk rst cpuInput jtagInput =
  ( CpuOut <$>
    (WishboneM2S
      <$> iBus_ADR
      <*> iBus_DAT_MOSI
      <*> iBus_SEL
      <*> pure False
      <*> iBus_CYC
      <*> iBus_STB
      <*> iBus_WE
      <*> (unpack <$> iBus_CTI)
      <*> (unpack <$> iBus_BTE)
    )
    <*>
    (WishboneM2S
      <$> dBus_ADR
      <*> dBus_DAT_MOSI
      <*> dBus_SEL
      <*> pure False
      <*> dBus_CYC
      <*> dBus_STB
      <*> dBus_WE
      <*> (unpack <$> dBus_CTI)
      <*> (unpack <$> dBus_BTE)
    )
  , JtagOut <$> jtag_TDO1 <*> debug_resetOut1
  )

  where

    jtag_TDO1 =
          jtag_TDO

    debug_resetOut1 =
          debug_resetOut

    (unbundle -> (timerInterrupt, externalInterrupt, softwareInterrupt, iBusS2M, dBusS2M))
      = (\(CpuIn a b c d e) -> (a, b, c, d, e)) <$> cpuInput

    (unbundle -> (iBus_DAT_MISO, iBus_ACK, iBus_ERR))
      = (\(WishboneS2M a b c _ _) -> (a, b, c))
      -- A hack that enables us to both generate synthesizable HDL and simulate vexRisc in Haskell/Clash
      . (if clashSimulation then makeDefined else id)
      <$> iBusS2M

    (unbundle -> (dBus_DAT_MISO, dBus_ACK, dBus_ERR))
      = (\(WishboneS2M a b c _ _) -> (a, b, c))
      -- A hack that enables us to both generate synthesizable HDL and simulate vexRisc in Haskell/Clash
      . (if clashSimulation then makeDefined else id)
      <$> dBusS2M

    (unbundle -> (jtag_TCK, jtag_TMS, jtag_TDI))
      = bitCoerce <$> jtagInput

    sourcePath = $(do
          cpuSrcPath <- runIO $ getPackageRelFilePath "example-cpu/VexRiscv.v"
          pure $ LitE $ StringL cpuSrcPath
        )

    -- TODO: Remove need for 'vexRiscv#' by doing construction / deconstruction of
    --       product types in HDL using BlackBoxHaskell functions.
    ( iBus_CYC
      , iBus_STB
      , iBus_WE
      , iBus_ADR
      , iBus_DAT_MOSI
      , iBus_SEL
      , iBus_CTI
      , iBus_BTE
      , dBus_CYC
      , dBus_STB
      , dBus_WE
      , dBus_ADR
      , dBus_DAT_MOSI
      , dBus_SEL
      , dBus_CTI
      , dBus_BTE
      , debug_resetOut
      , jtag_TDO
      ) = vexRiscv# sourcePath clk rst
          timerInterrupt
          externalInterrupt
          softwareInterrupt

          iBus_ACK
          iBus_ERR
          iBus_DAT_MISO

          dBus_ACK
          dBus_ERR
          dBus_DAT_MISO

          jtag_TCK
          jtag_TMS
          jtag_TDI


vexRiscv#
  :: KnownDomain dom
  => String
  -> Clock dom
  -> Reset dom
  -- input signals
  -> Signal dom Bit  -- ^ timerInterrupt
  -> Signal dom Bit  -- ^ externalInterrupt
  -> Signal dom Bit  -- ^ softwareInterrupt
    -- iBusWbS2M
  -> Signal dom Bool           -- ^ iBus_ACK
  -> Signal dom Bool           -- ^ iBus_ERR
  -> Signal dom (BitVector 32) -- ^ iBus_DAT_MISO
    -- dBusWbS2M
  -> Signal dom Bool           -- ^ dBus_ACK
  -> Signal dom Bool           -- ^ dBus_ERR
  -> Signal dom (BitVector 32) -- ^ dBus_DAT_MISO

  -> Signal dom Bit -- ^ jtag_TCK
  -> Signal dom Bit -- ^ jtag_TMS
  -> Signal dom Bit -- ^ jtag_TDI


  -- output signals
  ->
    (
      -- iBus M2S
      Signal dom Bool           -- ^ iBus_CYC
    , Signal dom Bool           -- ^ iBus_STB
    , Signal dom Bool           -- ^ iBus_WE
    , Signal dom (BitVector 30) -- ^ iBus_ADR
    , Signal dom (BitVector 32) -- ^ iBus_DAT_MOSI
    , Signal dom (BitVector 4)  -- ^ iBus_SEL
    , Signal dom (BitVector 3)  -- ^ iBus_CTI
    , Signal dom (BitVector 2)  -- ^ iBus_BTE

    -- dBus M2S
    , Signal dom Bool           -- ^ dBus_CYC
    , Signal dom Bool           -- ^ dBus_STB
    , Signal dom Bool           -- ^ dBus_WE
    , Signal dom (BitVector 30) -- ^ dBus_ADR
    , Signal dom (BitVector 32) -- ^ dBus_DAT_MOSI
    , Signal dom (BitVector 4)  -- ^ dBus_SEL
    , Signal dom (BitVector 3)  -- ^ dBus_CTI
    , Signal dom (BitVector 2)  -- ^ dBus_BTE

    , Signal dom Bit -- ^ debug_resetOut
    , Signal dom Bit -- ^ jtag_TDO
    )
vexRiscv# !_sourcePath clk rst0
  timerInterrupt
  externalInterrupt
  softwareInterrupt
  iBus_ACK
  iBus_ERR
  iBus_DAT_MISO

  dBus_ACK
  dBus_ERR
  dBus_DAT_MISO

  jtag_TCK
  jtag_TMS
  jtag_TDI

  =
    let
      (v, initStage1, initStage2, stepRising, stepFalling, _shutDown) = unsafePerformIO vexCPU

      cpuNonCombInput = CPU_NON_COMB_INPUT
        <$> (boolToBit <$> unsafeToActiveHigh rst0)
        <*> timerInterrupt
        <*> externalInterrupt
        <*> softwareInterrupt

      cpuCombInput = CPU_COMB_INPUT
        <$> (boolToBit <$> iBus_ACK)
        <*> (unpack    <$> iBus_DAT_MISO)
        <*> (boolToBit <$> iBus_ERR)
        <*> (boolToBit <$> dBus_ACK)
        <*> (unpack    <$> dBus_DAT_MISO)
        <*> (boolToBit <$> dBus_ERR)

      jtagCombInput = JTAG_COMB_INPUT
        <$> jtag_TCK
        <*> jtag_TMS
        <*> jtag_TDI

      wordCast = fromInteger . toInteger

      simInitThenCycles ::
        Signal dom CPU_NON_COMB_INPUT ->
        Signal dom CPU_COMB_INPUT ->
        Signal dom JTAG_COMB_INPUT ->
        Signal dom (CPU_OUTPUT, JTAG_OUTPUT)
      simInitThenCycles (cnc :- cncs) ~(cc :- ccs) ~(jc :- jcs) =
        let
          -- Note: we don't need @ticks@ for the initialization stages, because this
          -- first cycle of a Clash Signal is meant to model what happens _before_ a
          -- clock edge.
          out0 = unsafePerformIO (initStage1 v cnc)
          stage2Out = unsafePerformIO (initStage2 v cc jc)
          ticks = first wordCast <$> singleClockEdgesRelative clk
          out1 = simCycles ticks cncs ccs jcs
        in
          out0 :- (out0 `seq` (stage2Out `seq` out1))

      simCycles ::
        [(Word64, ActiveEdge)] ->
        Signal dom CPU_NON_COMB_INPUT ->
        Signal dom CPU_COMB_INPUT ->
        Signal dom JTAG_COMB_INPUT ->
        Signal dom (CPU_OUTPUT, JTAG_OUTPUT)
      simCycles ((fsSinceLastEvent, Rising) : ts) (cnc :- cncs) ccs jcs =
        let
          out0 = unsafePerformIO (stepRising v fsSinceLastEvent cnc)
          out1 = simCycles ts cncs ccs jcs
        in
          out0 :- (out0 `seq` out1)

      simCycles ((fsSinceLastEvent, Falling) : ts) cncs (cc :- ccs) (jc :- jcs) =
        let !() = unsafePerformIO (stepFalling v fsSinceLastEvent cc jc)
        in  simCycles ts cncs ccs jcs

      simCycles [] _ _ _ = error "Empty ticks: should never happen"

      (cpuOutput, jtagOutput) = unbundle $
        simInitThenCycles cpuNonCombInput cpuCombInput jtagCombInput

      iBus_CYC      = FFI.iBusWishbone_CYC      <$> cpuOutput
      iBus_STB      = FFI.iBusWishbone_STB      <$> cpuOutput
      iBus_WE       = FFI.iBusWishbone_WE       <$> cpuOutput
      iBus_ADR      = FFI.iBusWishbone_ADR      <$> cpuOutput
      iBus_DAT_MOSI = FFI.iBusWishbone_DAT_MOSI <$> cpuOutput
      iBus_SEL      = FFI.iBusWishbone_SEL      <$> cpuOutput
      iBus_CTI      = FFI.iBusWishbone_CTI      <$> cpuOutput
      iBus_BTE      = FFI.iBusWishbone_BTE      <$> cpuOutput

      dBus_CYC      = FFI.dBusWishbone_CYC      <$> cpuOutput
      dBus_STB      = FFI.dBusWishbone_STB      <$> cpuOutput
      dBus_WE       = FFI.dBusWishbone_WE       <$> cpuOutput
      dBus_ADR      = FFI.dBusWishbone_ADR      <$> cpuOutput
      dBus_DAT_MOSI = FFI.dBusWishbone_DAT_MOSI <$> cpuOutput
      dBus_SEL      = FFI.dBusWishbone_SEL      <$> cpuOutput
      dBus_CTI      = FFI.dBusWishbone_CTI      <$> cpuOutput
      dBus_BTE      = FFI.dBusWishbone_BTE      <$> cpuOutput

      debug_resetOut = FFI.debug_resetOut <$> jtagOutput
      jtag_TDO = FFI.jtag_TDO <$> jtagOutput

    in
      ( -- iBus
        bitToBool <$> iBus_CYC
      , bitToBool <$> iBus_STB
      , bitToBool <$> iBus_WE
      , truncateB . pack <$> iBus_ADR
      , pack <$> iBus_DAT_MOSI
      , truncateB . pack <$> iBus_SEL
      , truncateB . pack <$> iBus_CTI
      , truncateB . pack <$> iBus_BTE

      -- dBus
      , bitToBool <$> dBus_CYC
      , bitToBool <$> dBus_STB
      , bitToBool <$> dBus_WE
      , truncateB . pack <$> dBus_ADR
      , pack <$> dBus_DAT_MOSI
      , truncateB . pack <$> dBus_SEL
      , truncateB . pack <$> dBus_CTI
      , truncateB . pack <$> dBus_BTE

      -- JTAG
      , debug_resetOut
      , jtag_TDO
      )

{-# NOINLINE vexRiscv# #-}
{-# ANN vexRiscv# (
    let
      primName = 'vexRiscv#


      (
       -- ARGs
       _
       :> srcPath
       :> clk
       :> rst
       :> timerInterrupt
       :> externalInterrupt
       :> softwareInterrupt
       :> iBus_ACK
       :> iBus_ERR
       :> iBus_DAT_MISO
       :> dBus_ACK
       :> dBus_ERR
       :> dBus_DAT_MISO
       :> jtag_TCK
       :> jtag_TMS
       :> jtag_TDI

       -- GENSYMs
       :> iBus_CYC
       :> iBus_STB
       :> iBus_WE
       :> iBus_ADR
       :> iBus_DAT_MOSI
       :> iBus_SEL
       :> iBus_CTI
       :> iBus_BTE
       :> dBus_CYC
       :> dBus_STB
       :> dBus_WE
       :> dBus_ADR
       :> dBus_DAT_MOSI
       :> dBus_SEL
       :> dBus_CTI
       :> dBus_BTE
       :> debug_resetOut
       :> jtag_TDO

       :> cpu
       :> Nil
       ) = indicesI @35
    in
      InlineYamlPrimitive [Verilog] [__i|
  BlackBox:
    name: #{primName}
    kind: Declaration
    template: |-
      // vexRiscv begin

      ~DEVNULL[~FILE[~LIT[#{srcPath}]]]

      wire ~GENSYM[iBus_CYC][#{iBus_CYC}];
      wire ~GENSYM[iBus_STB][#{iBus_STB}];
      wire ~GENSYM[iBus_WE][#{iBus_WE}];
      wire [29:0] ~GENSYM[iBus_ADR][#{iBus_ADR}];
      wire [31:0] ~GENSYM[iBus_DAT_MOSI][#{iBus_DAT_MOSI}];
      wire [3:0] ~GENSYM[iBus_SEL][#{iBus_SEL}];
      wire [2:0] ~GENSYM[iBus_CTI][#{iBus_CTI}];
      wire [1:0] ~GENSYM[iBus_BTE][#{iBus_BTE}];

      wire ~GENSYM[dBus_CYC][#{dBus_CYC}];
      wire ~GENSYM[dBus_STB][#{dBus_STB}];
      wire ~GENSYM[dBus_WE][#{dBus_WE}];
      wire [29:0] ~GENSYM[dBus_ADR][#{dBus_ADR}];
      wire [31:0] ~GENSYM[dBus_DAT_MOSI][#{dBus_DAT_MOSI}];
      wire [3:0] ~GENSYM[dBus_SEL][#{dBus_SEL}];
      wire [2:0] ~GENSYM[dBus_CTI][#{dBus_CTI}];
      wire [1:0] ~GENSYM[dBus_BTE][#{dBus_BTE}];

      wire ~GENSYM[debug_resetOut][#{debug_resetOut}];
      wire ~GENSYM[jtag_TDO][#{jtag_TDO}];

      VexRiscv ~GENSYM[cpu][#{cpu}] (
        .timerInterrupt    ( ~ARG[#{timerInterrupt}] ),
        .externalInterrupt ( ~ARG[#{externalInterrupt}] ),
        .softwareInterrupt ( ~ARG[#{softwareInterrupt}] ),

        .iBusWishbone_CYC      ( ~SYM[#{iBus_CYC}] ),
        .iBusWishbone_STB      ( ~SYM[#{iBus_STB}] ),
        .iBusWishbone_ACK      ( ~ARG[#{iBus_ACK}] ),
        .iBusWishbone_WE       ( ~SYM[#{iBus_WE}] ),
        .iBusWishbone_ADR      ( ~SYM[#{iBus_ADR}] ),
        .iBusWishbone_DAT_MISO ( ~ARG[#{iBus_DAT_MISO}] ),
        .iBusWishbone_DAT_MOSI ( ~SYM[#{iBus_DAT_MOSI}] ),
        .iBusWishbone_SEL      ( ~SYM[#{iBus_SEL}] ),
        .iBusWishbone_ERR      ( ~ARG[#{iBus_ERR}] ),
        .iBusWishbone_CTI      ( ~SYM[#{iBus_CTI}] ),
        .iBusWishbone_BTE      ( ~SYM[#{iBus_BTE}] ),

        .dBusWishbone_CYC      ( ~SYM[#{dBus_CYC}] ),
        .dBusWishbone_STB      ( ~SYM[#{dBus_STB}] ),
        .dBusWishbone_ACK      ( ~ARG[#{dBus_ACK}] ),
        .dBusWishbone_WE       ( ~SYM[#{dBus_WE}] ),
        .dBusWishbone_ADR      ( ~SYM[#{dBus_ADR}] ),
        .dBusWishbone_DAT_MISO ( ~ARG[#{dBus_DAT_MISO}] ),
        .dBusWishbone_DAT_MOSI ( ~SYM[#{dBus_DAT_MOSI}] ),
        .dBusWishbone_SEL      ( ~SYM[#{dBus_SEL}] ),
        .dBusWishbone_ERR      ( ~ARG[#{dBus_ERR}] ),
        .dBusWishbone_CTI      ( ~SYM[#{dBus_CTI}] ),
        .dBusWishbone_BTE      ( ~SYM[#{dBus_BTE}] ),

        .jtag_tms       ( ~ARG[#{jtag_TMS}]),
        .jtag_tdi       ( ~ARG[#{jtag_TDI}]),
        .jtag_tck       ( ~ARG[#{jtag_TCK}]),
        .jtag_tdo       ( ~SYM[#{jtag_TDO}] ),

        .debug_resetOut ( ~SYM[#{debug_resetOut}] ),

        .clk   ( ~ARG[#{clk}] ),
        .reset ( ~ARG[#{rst}] )
      );

      assign ~RESULT = {
        ~SYM[#{iBus_CYC}],
        ~SYM[#{iBus_STB}],
        ~SYM[#{iBus_WE}],
        ~SYM[#{iBus_ADR}],
        ~SYM[#{iBus_DAT_MOSI}],
        ~SYM[#{iBus_SEL}],
        ~SYM[#{iBus_CTI}],
        ~SYM[#{iBus_BTE}],
        ~SYM[#{dBus_CYC}],
        ~SYM[#{dBus_STB}],
        ~SYM[#{dBus_WE}],
        ~SYM[#{dBus_ADR}],
        ~SYM[#{dBus_DAT_MOSI}],
        ~SYM[#{dBus_SEL}],
        ~SYM[#{dBus_CTI}],
        ~SYM[#{dBus_BTE}],
        ~SYM[#{debug_resetOut}],
        ~SYM[#{jtag_TDO}]
      };

      // vexRiscv end

    |] ) #-}


-- | Return a function that performs an execution step and a function to free
-- the internal CPU state
vexCPU :: IO
  ( Ptr VexRiscv
  , Ptr VexRiscv -> CPU_NON_COMB_INPUT -> IO (CPU_OUTPUT, JTAG_OUTPUT)           -- initStage1
  , Ptr VexRiscv -> CPU_COMB_INPUT -> JTAG_COMB_INPUT -> IO ()                   -- initStage2
  , Ptr VexRiscv -> Word64 -> CPU_NON_COMB_INPUT -> IO (CPU_OUTPUT, JTAG_OUTPUT) -- rising
  , Ptr VexRiscv -> Word64 -> CPU_COMB_INPUT -> JTAG_COMB_INPUT -> IO ()         -- falling
  , Ptr VexRiscv -> IO ()
  )
vexCPU = do
  v <- vexrInit

  let
    {-# NOINLINE initStage1 #-}
    initStage1 vPtr cpuNonCombInput =
      alloca $ \cpuNonCombInputFFI -> alloca $ \cpuOutputFFI -> alloca $ \jtagOutputFFI -> do
        poke cpuNonCombInputFFI cpuNonCombInput
        vexrInitStage1 vPtr cpuNonCombInputFFI cpuOutputFFI jtagOutputFFI
        cpuOutput <- peek cpuOutputFFI
        jtagOutput <- peek jtagOutputFFI
        pure (cpuOutput, jtagOutput)

    {-# NOINLINE initStage2 #-}
    initStage2 vPtr cpuCombInput jtagCombInput =
      alloca $ \cpuCombInputFFI -> alloca $ \jtagCombInputFFI -> do
        poke cpuCombInputFFI cpuCombInput
        poke jtagCombInputFFI jtagCombInput
        vexrInitStage2 vPtr cpuCombInputFFI jtagCombInputFFI

    {-# NOINLINE stepRising #-}
    stepRising vPtr fsSinceLastEvent cpuNonCombInput =
      alloca $ \cpuNonCombInputFFI -> alloca $ \cpuOutputFFI -> alloca $ \jtagOutputFFI -> do
        poke cpuNonCombInputFFI cpuNonCombInput
        vexrStepRisingEdge vPtr fsSinceLastEvent cpuNonCombInputFFI cpuOutputFFI jtagOutputFFI
        cpuOutput <- peek cpuOutputFFI
        jtagOutput <- peek jtagOutputFFI
        pure (cpuOutput, jtagOutput)

    {-# NOINLINE stepFalling #-}
    stepFalling vPtr fsSinceLastEvent cpuCombInput jtagCombInput =
      alloca $ \cpuCombInputFFI -> alloca $ \jtagCombInputFFI -> do
        poke cpuCombInputFFI cpuCombInput
        poke jtagCombInputFFI jtagCombInput
        vexrStepFallingEdge vPtr fsSinceLastEvent cpuCombInputFFI jtagCombInputFFI

    shutDown = vexrShutdown

  pure (v, initStage1, initStage2, stepRising, stepFalling, shutDown)
