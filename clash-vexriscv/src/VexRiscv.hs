-- SPDX-FileCopyrightText: 2022-2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module VexRiscv where

import Clash.Prelude

import Clash.Annotations.Primitive
import Clash.Signal.Internal
import Data.String.Interpolate (__i)
import Foreign.Marshal (alloca)
import Foreign.Storable
import GHC.IO (unsafePerformIO)
import GHC.Stack (HasCallStack)
import Language.Haskell.TH.Syntax
import Protocols
import Protocols.Wishbone
import VexRiscv.FFI
import VexRiscv.TH

data JtagIn = JtagIn
  { testModeSelect :: "TMS" ::: Bit
  , testDataIn :: "TDI" ::: Bit
  , testClock :: "TCK" ::: Bit
  }
  deriving (Generic, Eq, NFDataX, ShowX)

newtype JtagOut = JtagOut { testDataOut :: "TDO" ::: Bit }
  deriving (Generic, NFDataX, ShowX, Eq)

data Input = Input
  { timerInterrupt :: "TIMER_INTERRUPT" ::: Bit
  , externalInterrupt :: "EXTERNAL_INTERRUPT" ::: Bit
  , softwareInterrupt :: "SOFTWARE_INTERRUPT" ::: Bit
  , iBusWbS2M :: "IBUS_IN_" ::: WishboneS2M (BitVector 32)
  , dBusWbS2M :: "DBUS_IN_" ::: WishboneS2M (BitVector 32)
  , jtagIn :: "JTAG_IN_" ::: JtagIn
  }
  deriving (Generic, NFDataX, ShowX, Eq, BitPack)

data Output = Output
  { iBusWbM2S :: "IBUS_OUT_" ::: WishboneM2S 30 4 (BitVector 32)
  , dBusWbM2S :: "DBUS_OUT_" ::: WishboneM2S 30 4 (BitVector 32)
  , jtagOut :: "JTAG_OUT_" ::: JtagOut
  , debugReset :: "DEBUG_RESET" ::: Bit
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

    , jtag_TMS = testModeSelect jtagIn
    , jtag_TDI = testDataIn jtagIn
    , jtag_TCK = testClock jtagIn
    }

outputFromFFI :: OUTPUT -> Output
outputFromFFI OUTPUT {..} =
  Output
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
          },
      
      debugReset = debug_resetOut,
      jtagOut = JtagOut { testDataOut = jtag_TDO }
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

vexRiscv :: (HasCallStack, HiddenClockResetEnable dom) => Signal dom Input -> Signal dom Output
vexRiscv input =
  Output <$>
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
    <*> (JtagOut <$> jtag_TDO)
    <*> debug_resetOut

  where
    (unbundle -> (timerInterrupt, externalInterrupt, softwareInterrupt, iBusS2M, dBusS2M, jtagIn))
      -- A hack that enables us to both generate synthesizable HDL and simulate vexRisc in Haskell/Clash
      = (<$> if clashSimulation then unpack 0 :- input else input)
        $ \(Input a b c d e f) -> (a, b, c, d, e, f)

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

    (unbundle -> (jtag_TMS, jtag_TDI, jtag_TCK))
      = (<$> jtagIn) $ \(JtagIn a b c) -> (a, b ,c)

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
      ) = vexRiscv# sourcePath hasClock hasReset
          timerInterrupt
          externalInterrupt
          softwareInterrupt

          iBus_ACK
          iBus_ERR
          iBus_DAT_MISO

          dBus_ACK
          dBus_ERR
          dBus_DAT_MISO

          jtag_TMS
          jtag_TDI
          jtag_TCK





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

  -> Signal dom Bit -- ^ jtag_TMS
  -> Signal dom Bit -- ^ jtag_TDI
  -> Signal dom Bit -- ^ jtag_TCK

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
vexRiscv# !_sourcePath !_clk rst0
  timerInterrupt
  externalInterrupt
  softwareInterrupt
  iBus_ACK
  iBus_ERR
  iBus_DAT_MISO

  dBus_ACK
  dBus_ERR
  dBus_DAT_MISO

  jtag_TMS
  jtag_TDI
  jtag_TCK

  =
    let
      iBusS2M = WishboneS2M <$> iBus_DAT_MISO <*> iBus_ACK <*> iBus_ERR <*> pure False <*> pure False
      dBusS2M = WishboneS2M <$> dBus_DAT_MISO <*> dBus_ACK <*> dBus_ERR <*> pure False <*> pure False

      jtagIn = JtagIn <$> jtag_TMS <*> jtag_TDI <*> jtag_TCK

      input = Input <$> timerInterrupt <*> externalInterrupt <*> softwareInterrupt <*> iBusS2M <*> dBusS2M <*> jtagIn

      output = unsafePerformIO $ do
        (step, _) <- vexCPU
        pure $ go step (unsafeFromReset rst0) input

      (unbundle -> (iBusM2S, dBusM2S, jtagOut, debug_resetOut)) = (<$> output) $ \(Output iBus dBus jtag debugResetOut) -> (iBus, dBus, jtag, debugResetOut)
      jtag_TDO = (\(JtagOut x) -> x) <$> jtagOut

      (unbundle -> (iBus_ADR, iBus_DAT_MOSI, iBus_SEL, iBus_CYC, iBus_STB, iBus_WE, iBus_CTI, iBus_BTE)) =
        (<$> iBusM2S) $ \(WishboneM2S a b c _ e f g h i) -> (a, b, c, e, f, g, h, i)

      (unbundle -> (dBus_ADR, dBus_DAT_MOSI, dBus_SEL, dBus_CYC, dBus_STB, dBus_WE, dBus_CTI, dBus_BTE)) =
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
  where
    {-# NOINLINE go #-}
    go step (rst :- rsts) (input :- inputs) = unsafePerformIO $ do
      out <- step rst input
      pure $ out :- go step rsts inputs
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
       :> jtag_TMS
       :> jtag_TDI
       :> jtag_TCK

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
vexCPU :: IO (Bool -> Input -> IO Output, IO ())
vexCPU = do
  v <- vexrInit
  let
    step reset input = alloca $ \inputFFI -> alloca $ \outputFFI -> do
      poke inputFFI (inputToFFI reset input)
      vexrStep v inputFFI outputFFI
      outVal <- peek outputFFI
      pure $ outputFromFFI outVal
    shutDown = vexrShutdown v
  pure (step, shutDown)
