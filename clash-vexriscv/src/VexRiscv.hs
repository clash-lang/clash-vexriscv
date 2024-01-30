-- SPDX-FileCopyrightText: 2022-2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}


{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module VexRiscv where

import Clash.Prelude

import Clash.Annotations.Primitive
import Clash.Signal.Internal
import Clash.Signal (ActiveEdge(..))
import Control.Monad (when)
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
import Debug.Trace (trace)
import Foreign (Ptr)

data JtagIn = JtagIn
  { testModeSelect :: "TMS" ::: Bit
  , testDataIn :: "TDI" ::: Bit
  }
  deriving (Generic, Eq, NFDataX, ShowX, BitPack)

data JtagOut = JtagOut
  { testDataOut :: "TDO" ::: Bit
  , debugReset :: "RST" ::: Bit
  }
  deriving (Generic, NFDataX, ShowX, Eq, BitPack)

data Input = Input
  { timerInterrupt :: "TIMER_INTERRUPT" ::: Bit
  , externalInterrupt :: "EXTERNAL_INTERRUPT" ::: Bit
  , softwareInterrupt :: "SOFTWARE_INTERRUPT" ::: Bit
  , iBusWbS2M :: "IBUS_IN_" ::: WishboneS2M (BitVector 32)
  , dBusWbS2M :: "DBUS_IN_" ::: WishboneS2M (BitVector 32)
  }
  deriving (Generic, NFDataX, ShowX, Eq, BitPack)

data Output = Output
  { iBusWbM2S :: "IBUS_OUT_" ::: WishboneM2S 30 4 (BitVector 32)
  , dBusWbM2S :: "DBUS_OUT_" ::: WishboneM2S 30 4 (BitVector 32)
  }
  deriving (Generic, NFDataX, ShowX, Eq, BitPack)


data Jtag (dom :: Domain)

instance Protocol (Jtag dom) where
  type Fwd (Jtag dom) = Signal dom JtagOut
  type Bwd (Jtag dom) = Signal dom JtagIn



inputToFFI :: Bool -> Input -> INPUT
inputToFFI reset Input {..} =
  INPUT
    { reset = boolToBit reset
    , timerInterrupt
    , externalInterrupt
    , softwareInterrupt

    , iBusWishbone_ACK = boolToBit $ acknowledge iBusWbS2M
    , iBusWishbone_DAT_MISO = unpack $ readData iBusWbS2M
    , iBusWishbone_ERR = boolToBit $ err iBusWbS2M

    , dBusWishbone_ACK = boolToBit $ acknowledge dBusWbS2M
    , dBusWishbone_DAT_MISO = unpack $ readData dBusWbS2M
    , dBusWishbone_ERR = boolToBit $ err dBusWbS2M
    }

outputFromFFI :: OUTPUT -> Output
outputFromFFI OUTPUT {..} = output
  where
    output = Output
      { iBusWbM2S =
          (emptyWishboneM2S @30 @(BitVector 32))
            { busCycle = bitToBool iBusWishbone_CYC,
              strobe = bitToBool iBusWishbone_STB,
              writeEnable = bitToBool iBusWishbone_WE,
              addr = truncateB $ pack iBusWishbone_ADR,
              writeData = pack iBusWishbone_DAT_MOSI,
              busSelect = resize $ pack iBusWishbone_SEL,
              cycleTypeIdentifier = unpack $ resize $ pack iBusWishbone_CTI,
              burstTypeExtension = unpack $ resize $ pack iBusWishbone_BTE
            },
        dBusWbM2S =
          (emptyWishboneM2S @30 @(BitVector 32))
            { busCycle = bitToBool dBusWishbone_CYC,
              strobe = bitToBool dBusWishbone_STB,
              writeEnable = bitToBool dBusWishbone_WE,
              addr = truncateB $ pack dBusWishbone_ADR,
              writeData = pack dBusWishbone_DAT_MOSI,
              busSelect = resize $ pack dBusWishbone_SEL,
              cycleTypeIdentifier = unpack $ resize $ pack dBusWishbone_CTI,
              burstTypeExtension = unpack $ resize $ pack dBusWishbone_BTE
            }
      }

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

simPrepend :: a -> Signal dom a -> Signal dom a
simPrepend l sig =
  if clashSimulation
    then l :- sig
    else sig

vexRiscv ::
  forall domCpu domJtag .
  ( HasCallStack
  , KnownDomain domCpu
  , KnownDomain domJtag) =>
  Clock domCpu ->
  Reset domCpu ->
  Clock domJtag ->
  Enable domJtag ->
  Signal domCpu Input ->
  Signal domJtag JtagIn ->
  ( Signal domCpu Output
  , Signal domJtag JtagOut
  )
vexRiscv clk rst jtagClk jtag_EN0 cpuInput jtagInput =
  ( Output <$>
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
      -- A hack that enables us to both generate synthesizable HDL and simulate vexRisc in Haskell/Clash
      = (<$> simPrepend (unpack 0) cpuInput)
        $ \(Input a b c d e) -> (a, b, c, d, e)

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

    (unbundle -> (jtag_TMS, jtag_TDI))
      -- A hack that enables us to both generate synthesizable HDL and simulate vexRisc in Haskell/Clash
      = bitCoerce <$> jtagInput
        -- simPrepend (unpack 0) jtagInput

    jtag_EN1 =
      toEnable $ simPrepend False (fromEnable jtag_EN0)
        -- if clashSimulation
        -- then toEnable $ False :- fromEnable jtag_EN0
        -- else jtag_EN0

    sourcePath = $(do
          cpuSrcPath <- runIO $ getPackageRelFilePath "example-cpu/VexRiscv.v"
          pure $ LitE $ StringL cpuSrcPath
        )

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

          jtagClk
          jtag_EN1
          jtag_TMS
          jtag_TDI
          


vexRiscv#
  :: KnownDomain domCpu
  => KnownDomain domJtag
  => String
  -> Clock domCpu
  -> Reset domCpu
  -- input signals
  -> Signal domCpu Bit  -- ^ timerInterrupt
  -> Signal domCpu Bit  -- ^ externalInterrupt
  -> Signal domCpu Bit  -- ^ softwareInterrupt
    -- iBusWbS2M
  -> Signal domCpu Bool           -- ^ iBus_ACK
  -> Signal domCpu Bool           -- ^ iBus_ERR
  -> Signal domCpu (BitVector 32) -- ^ iBus_DAT_MISO
    -- dBusWbS2M
  -> Signal domCpu Bool           -- ^ dBus_ACK
  -> Signal domCpu Bool           -- ^ dBus_ERR
  -> Signal domCpu (BitVector 32) -- ^ dBus_DAT_MISO

  -> Clock domJtag -- ^ jtag_TCK
  -> Enable domJtag -- ^ only used in simulation, ignored in synthesis
  -> Signal domJtag Bit -- ^ jtag_TMS
  -> Signal domJtag Bit -- ^ jtag_TDI


  -- output signals
  ->
    (
      -- iBus M2S
      Signal domCpu Bool           -- ^ iBus_CYC
    , Signal domCpu Bool           -- ^ iBus_STB
    , Signal domCpu Bool           -- ^ iBus_WE
    , Signal domCpu (BitVector 30) -- ^ iBus_ADR
    , Signal domCpu (BitVector 32) -- ^ iBus_DAT_MOSI
    , Signal domCpu (BitVector 4)  -- ^ iBus_SEL
    , Signal domCpu (BitVector 3)  -- ^ iBus_CTI
    , Signal domCpu (BitVector 2)  -- ^ iBus_BTE

    -- dBus M2S
    , Signal domCpu Bool           -- ^ dBus_CYC
    , Signal domCpu Bool           -- ^ dBus_STB
    , Signal domCpu Bool           -- ^ dBus_WE
    , Signal domCpu (BitVector 30) -- ^ dBus_ADR
    , Signal domCpu (BitVector 32) -- ^ dBus_DAT_MOSI
    , Signal domCpu (BitVector 4)  -- ^ dBus_SEL
    , Signal domCpu (BitVector 3)  -- ^ dBus_CTI
    , Signal domCpu (BitVector 2)  -- ^ dBus_BTE

    , Signal domJtag Bit -- ^ debug_resetOut
    , Signal domJtag Bit -- ^ jtag_TDO
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
  jtag_EN
  jtag_TMS
  jtag_TDI
  
  =
    let
      (v, cpuStepRising, cpuStepFalling, jtagStepRising, jtagStepFalling, _) = unsafePerformIO vexCPU

      iBusS2M = WishboneS2M <$> iBus_DAT_MISO <*> iBus_ACK <*> iBus_ERR <*> pure False <*> pure False
      dBusS2M = WishboneS2M <$> dBus_DAT_MISO <*> dBus_ACK <*> dBus_ERR <*> pure False <*> pure False

      ticks = go $ clockEdgesRelative clk jtag_TCK
        where
          go [] = error "Clock tick list should be infinite"
          go ((wordCast -> t, ClockEdgeA edge) : rest) = cpuEdge edge : AdvanceTime t : go rest
          go ((wordCast -> t, ClockEdgeB edge) : rest) = jtagEdge edge : AdvanceTime t : go rest
          go ((wordCast -> t, ClockEdgeAB edgeA edgeB) : rest) = cpuEdge edgeA : jtagEdge edgeB : AdvanceTime t : go rest

      wordCast x = fromInteger $ toInteger x

      cpuEdge Rising = CpuRising
      cpuEdge Falling = CpuFalling

      jtagEdge Rising = JtagRising
      jtagEdge Falling = JtagFalling

      -- The `[ClockSimOrder]` gets consumed twice as fast as the signals produce
      -- and consume values.
      --
      -- That means that values in the output signal will be produced in the
      -- "falling" cases. Values in the input signals will also be consued in the
      -- "falling" cases.
      bothOutputs ::
        [ClockSimOrder] ->
        Bool -> -- ^ JTAG_ENABLED asserted
        Signal domCpu Bool -> -- ^ reset
        Signal domCpu Input ->
        Signal domJtag Bool -> -- ^ enable
        Signal domJtag JtagIn ->
        (Signal domCpu Output, Signal domJtag JtagOut)
      bothOutputs (CpuRising:ticks) tckAsserted (r :- rsts) (c :- cpuIn) jtagEn jtagIn =
        let (cpuSig, jtagSig) = bothOutputs ticks tckAsserted (r :- rsts) (c :- cpuIn) jtagEn jtagIn in
          unsafePerformIO (cpuStepRising v r c) `deepseqX`
            (cpuSig, jtagSig)

      bothOutputs (CpuFalling:ticks) tckAsserted (r :- rsts) (c :- cpuIn) jtagEn jtagIn =
        let (cpuSig, jtagSig) = bothOutputs ticks tckAsserted rsts cpuIn jtagEn jtagIn
            !cpuOut = unsafePerformIO $ cpuStepFalling v
        in
          cpuOut `deepseqX` (cpuOut :- (cpuOut `deepseqX` cpuSig), jtagSig)
   
      bothOutputs (JtagRising:ticks) tckAsserted rsts cpuIn (en :- enables) (j :- jtagIn)
        | tckAsserted && en =
            -- do nothing
            bothOutputs ticks tckAsserted rsts cpuIn (en :- enables) (j :- jtagIn)
        | tckAsserted && not en =
            -- do nothing, pass to falling edge
            bothOutputs ticks tckAsserted rsts cpuIn (en :- enables) (j :- jtagIn)
        | not tckAsserted && en =
            -- do rising edge
            let (cpuSig, jtagSig) = bothOutputs ticks True rsts cpuIn (en :- enables) (j :- jtagIn) in
              
            -- trace "!TCK" $
            unsafePerformIO (jtagStepRising v j) `deepseqX` (cpuSig, jtagSig)

        | not tckAsserted && not en =
            -- do nothing
            bothOutputs ticks tckAsserted rsts cpuIn (en :- enables) (j :- jtagIn)
      bothOutputs (JtagFalling:ticks) tckAsserted rsts cpuIn (en :- enables) (j :- jtagIn)
        | not tckAsserted && en =
            -- shouldn't happen, if en then there should be a rising edge => TCK
            error "!TCK && EN shouldn't happen at a falling edge"
        | not tckAsserted && not en =
            -- don't need to do anything
            let (cpuSig, jtagSig) = bothOutputs ticks tckAsserted rsts cpuIn enables jtagIn
            in (cpuSig, JtagOut low low :- jtagSig)
        | tckAsserted && en =
            -- don't need to do anything
            let (cpuSig, jtagSig) = bothOutputs ticks tckAsserted rsts cpuIn enables jtagIn
            in (cpuSig, JtagOut low low :- jtagSig)
        | tckAsserted && not en =
            -- actually do falling edge
            let (cpuSig, jtagSig) = bothOutputs ticks False rsts cpuIn enables jtagIn
                !jtagOut = unsafePerformIO $ jtagStepFalling v
            in 
              -- trace " TCK" $
              (cpuSig, jtagOut :- (jtagOut `deepseqX` jtagSig))

      bothOutputs (AdvanceTime t:ticks) tckAsserted rsts cpuIn jtagEn jtagIn =
        unsafePerformIO (vexrSimTimeStep t) `deepseqX`
          bothOutputs ticks tckAsserted rsts cpuIn jtagEn jtagIn

      bothOutputs [] _ _ _ _ _ = error "Clock tick list should be infinite"

      
      cpuInput = Input <$> timerInterrupt <*> externalInterrupt <*> softwareInterrupt <*> iBusS2M <*> dBusS2M
      
      jtagInput = JtagIn <$> jtag_TMS <*> jtag_TDI

      (cpuOutput, jtagOutput) =
        bothOutputs
          ticks
          False
          (unsafeToActiveHigh rst0)
          cpuInput
          (fromEnable jtag_EN)
          jtagInput


      (unbundle -> (iBusM2S, dBusM2S)) = (<$> cpuOutput) $ \(Output iBus dBus) -> (iBus, dBus)
      (unbundle -> (jtag_TDO, debug_resetOut)) = bitCoerce <$> jtagOutput

      (unbundle -> ( iBus_ADR
                   , iBus_DAT_MOSI
                   , iBus_SEL
                   , iBus_CYC
                   , iBus_STB
                   , iBus_WE
                   , iBus_CTI
                   , iBus_BTE
                   )) =
        (<$> iBusM2S) $ \(WishboneM2S a b c _ e f g h i) -> (a, b, c, e, f, g, h, i)

      (unbundle -> ( dBus_ADR
                   , dBus_DAT_MOSI
                   , dBus_SEL
                   , dBus_CYC
                   , dBus_STB
                   , dBus_WE
                   , dBus_CTI
                   , dBus_BTE)) =
        (<$> dBusM2S) $ \(WishboneM2S a b c _ e f g h i) -> (a, b, c, e, f, g, h, i)
    in
      ( -- iBus
        iBus_CYC
      , iBus_STB
      , iBus_WE
      , iBus_ADR
      , iBus_DAT_MOSI
      , iBus_SEL
      , pack <$> iBus_CTI
      , pack <$> iBus_BTE

      -- dBus
      , dBus_CYC
      , dBus_STB
      , dBus_WE
      , dBus_ADR
      , dBus_DAT_MOSI
      , dBus_SEL
      , pack <$> dBus_CTI
      , pack <$> dBus_BTE

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
       :> _
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
       ) = indicesI @36
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


data ClockSimOrder = CpuRising
                   | CpuFalling
                   | JtagRising
                   | JtagFalling
                   | AdvanceTime Word64

-- | Return a function that performs an execution step and a function to free
-- the internal CPU state
vexCPU :: IO
  ( Ptr VexRiscv,
    Ptr VexRiscv -> Bool -> Input -> IO (),
    Ptr VexRiscv -> IO Output,
    Ptr VexRiscv -> JtagIn -> IO (),
    Ptr VexRiscv -> IO JtagOut,
    Ptr VexRiscv -> IO ()
  )
vexCPU = do
  v <- vexrInit
  let
    {-# NOINLINE cpuStepRising #-}
    cpuStepRising v reset input = alloca $ \inputFFI -> do
      poke inputFFI (inputToFFI reset input)
      -- putStrLn "CPU rising edge"
      vexrCpuStepRisingEdge v inputFFI
      pure ()
  
    {-# NOINLINE cpuStepFalling #-}
    cpuStepFalling v = alloca $ \outputFFI -> do
      -- putStrLn "CPU falling edge"
      vexrCpuStepFallingEdge v outputFFI
      outVal <- peek outputFFI
      pure $ outputFromFFI outVal

    {-# NOINLINE jtagStepRising #-}
    jtagStepRising v JtagIn{..} = alloca $ \inputFFI -> do
          poke inputFFI (JTAG_INPUT { jtag_TMS = testModeSelect, jtag_TDI = testDataIn })
          -- putStrLn "JTAG rising edge"
          vexrJtagStepRisingEdge v inputFFI
          pure ()

    {-# NOINLINE jtagStepFalling #-}
    jtagStepFalling v = alloca $ \outputFFI -> do
          -- putStrLn "JTAG falling edge"
          vexrJtagStepFallingEdge v outputFFI
          JTAG_OUTPUT{..} <- peek outputFFI
          pure $ JtagOut { testDataOut = jtag_TDO, debugReset = debug_resetOut }

    shutDown = vexrShutdown
  pure (v, cpuStepRising, cpuStepFalling, jtagStepRising, jtagStepFalling, shutDown)
