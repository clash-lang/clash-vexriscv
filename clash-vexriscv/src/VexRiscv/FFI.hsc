-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module VexRiscv.FFI where

import Foreign.Storable
import Foreign.Ptr
import Prelude
import Clash.Prelude
import Data.Word

#include "ffi/interface.h"

data VexRiscv

data VexRiscvJtagBridge

foreign import ccall unsafe "vexr_init" vexrInit :: IO (Ptr VexRiscv)
foreign import ccall unsafe "vexr_shutdown" vexrShutdown :: Ptr VexRiscv -> IO ()

foreign import ccall unsafe "vexr_init_stage1" vexrInitStage1 :: Ptr VexRiscv -> Ptr CPU_NON_COMB_INPUT -> Ptr CPU_OUTPUT -> Ptr JTAG_OUTPUT -> IO ()
foreign import ccall unsafe "vexr_init_stage2" vexrInitStage2 :: Ptr VexRiscv -> Ptr CPU_COMB_INPUT -> Ptr JTAG_COMB_INPUT -> IO ()
foreign import ccall unsafe "vexr_step_rising_edge" vexrStepRisingEdge :: Ptr VexRiscv -> Word64 -> Ptr CPU_NON_COMB_INPUT -> Ptr CPU_OUTPUT -> Ptr JTAG_OUTPUT -> IO ()
foreign import ccall unsafe "vexr_step_falling_edge" vexrStepFallingEdge :: Ptr VexRiscv -> Word64 -> Ptr CPU_COMB_INPUT -> Ptr JTAG_COMB_INPUT -> IO ()

foreign import ccall unsafe "vexr_jtag_bridge_init" vexrJtagBridgeInit :: Word16 -> IO (Ptr VexRiscvJtagBridge)
foreign import ccall unsafe "vexr_jtag_bridge_step" vexrJtagBridgeStep :: Ptr VexRiscvJtagBridge -> Ptr JTAG_OUTPUT -> Ptr JTAG_COMB_INPUT -> IO ()
foreign import ccall unsafe "vexr_jtag_bridge_shutdown" vexrJtagBridgeShutdown :: Ptr VexRiscvJtagBridge -> IO ()

-- | CPU input that cannot combinatorially depend on the CPU output
data CPU_NON_COMB_INPUT = CPU_NON_COMB_INPUT
  { reset :: Bit
  , timerInterrupt :: Bit
  , externalInterrupt :: Bit
  , softwareInterrupt :: Bit
  }

-- | CPU input that can combinatorially depend on the CPU output
data CPU_COMB_INPUT = CPU_COMB_INPUT
  { iBusWishbone_ACK :: Bit
  , iBusWishbone_DAT_MISO :: Word32
  , iBusWishbone_ERR :: Bit

  , dBusWishbone_ACK :: Bit
  , dBusWishbone_DAT_MISO :: Word32
  , dBusWishbone_ERR :: Bit
  }
  deriving (Show)

data CPU_OUTPUT = CPU_OUTPUT
  { iBusWishbone_CYC :: Bit
  , iBusWishbone_STB :: Bit
  , iBusWishbone_WE :: Bit
  , iBusWishbone_ADR :: Word32
  , iBusWishbone_DAT_MOSI :: Word32
  , iBusWishbone_SEL :: Word8
  , iBusWishbone_CTI :: Word8
  , iBusWishbone_BTE :: Word8

  , dBusWishbone_CYC :: Bit
  , dBusWishbone_STB :: Bit
  , dBusWishbone_WE :: Bit
  , dBusWishbone_ADR :: Word32
  , dBusWishbone_DAT_MOSI :: Word32
  , dBusWishbone_SEL :: Word8
  , dBusWishbone_CTI :: Word8
  , dBusWishbone_BTE :: Word8
  }
  deriving (Show)

data JTAG_COMB_INPUT = JTAG_COMB_INPUT
  { jtag_TCK :: Bit
  , jtag_TMS :: Bit
  , jtag_TDI :: Bit
  }
  deriving (Show)

data JTAG_OUTPUT = JTAG_OUTPUT
  { debug_resetOut :: Bit
  , jtag_TDO :: Bit
  }
  deriving (Show)

instance Storable Bit where
    alignment = alignment . bitToBool
    sizeOf = sizeOf . bitToBool
    peek = fmap boolToBit . peek . castPtr
    poke ptr = poke (castPtr ptr) . bitToBool

instance Storable CPU_NON_COMB_INPUT where
    alignment _ = #alignment CPU_NON_COMB_INPUT
    sizeOf _ = #size CPU_NON_COMB_INPUT
    {-# INLINE peek #-}
    peek ptr = const CPU_NON_COMB_INPUT <$> pure ()
      <*> (#peek CPU_NON_COMB_INPUT, reset) ptr
      <*> (#peek CPU_NON_COMB_INPUT, timerInterrupt) ptr
      <*> (#peek CPU_NON_COMB_INPUT, externalInterrupt) ptr
      <*> (#peek CPU_NON_COMB_INPUT, softwareInterrupt) ptr

    {-# INLINE poke #-}
    poke ptr this = do
      (#poke CPU_NON_COMB_INPUT, reset) ptr (reset this)
      (#poke CPU_NON_COMB_INPUT, timerInterrupt) ptr (timerInterrupt this)
      (#poke CPU_NON_COMB_INPUT, externalInterrupt) ptr (externalInterrupt this)
      (#poke CPU_NON_COMB_INPUT, softwareInterrupt) ptr (softwareInterrupt this)
      return ()

instance Storable CPU_COMB_INPUT where
    alignment _ = #alignment CPU_COMB_INPUT
    sizeOf _ = #size CPU_COMB_INPUT
    {-# INLINE peek #-}
    peek ptr = const CPU_COMB_INPUT <$> pure ()
      <*> (#peek CPU_COMB_INPUT, iBusWishbone_ACK) ptr
      <*> (#peek CPU_COMB_INPUT, iBusWishbone_DAT_MISO) ptr
      <*> (#peek CPU_COMB_INPUT, iBusWishbone_ERR) ptr
      <*> (#peek CPU_COMB_INPUT, dBusWishbone_ACK) ptr
      <*> (#peek CPU_COMB_INPUT, dBusWishbone_DAT_MISO) ptr
      <*> (#peek CPU_COMB_INPUT, dBusWishbone_ERR) ptr

    {-# INLINE poke #-}
    poke ptr this = do
      (#poke CPU_COMB_INPUT, iBusWishbone_ACK) ptr (iBusWishbone_ACK this)
      (#poke CPU_COMB_INPUT, iBusWishbone_DAT_MISO) ptr (iBusWishbone_DAT_MISO this)
      (#poke CPU_COMB_INPUT, iBusWishbone_ERR) ptr (iBusWishbone_ERR this)

      (#poke CPU_COMB_INPUT, dBusWishbone_ACK) ptr (dBusWishbone_ACK this)
      (#poke CPU_COMB_INPUT, dBusWishbone_DAT_MISO) ptr (dBusWishbone_DAT_MISO this)
      (#poke CPU_COMB_INPUT, dBusWishbone_ERR) ptr (dBusWishbone_ERR this)
      return ()

instance Storable JTAG_COMB_INPUT where
    alignment _ = #alignment JTAG_COMB_INPUT
    sizeOf _ = #size JTAG_COMB_INPUT
    {-# INLINE peek #-}
    peek ptr = const JTAG_COMB_INPUT <$> pure ()
      <*> (#peek JTAG_COMB_INPUT, jtag_TCK) ptr
      <*> (#peek JTAG_COMB_INPUT, jtag_TMS) ptr
      <*> (#peek JTAG_COMB_INPUT, jtag_TDI) ptr

    {-# INLINE poke #-}
    poke ptr this = do
      (#poke JTAG_COMB_INPUT, jtag_TCK) ptr (jtag_TCK this)
      (#poke JTAG_COMB_INPUT, jtag_TMS) ptr (jtag_TMS this)
      (#poke JTAG_COMB_INPUT, jtag_TDI) ptr (jtag_TDI this)
      return ()

instance Storable CPU_OUTPUT where
    alignment _ = #alignment CPU_OUTPUT
    sizeOf _ = #size CPU_OUTPUT
    {-# INLINE peek #-}
    peek ptr = const CPU_OUTPUT <$> pure ()
      <*> (#peek CPU_OUTPUT, iBusWishbone_CYC) ptr
      <*> (#peek CPU_OUTPUT, iBusWishbone_STB) ptr
      <*> (#peek CPU_OUTPUT, iBusWishbone_WE) ptr
      <*> (#peek CPU_OUTPUT, iBusWishbone_ADR) ptr
      <*> (#peek CPU_OUTPUT, iBusWishbone_DAT_MOSI) ptr
      <*> (#peek CPU_OUTPUT, iBusWishbone_SEL) ptr
      <*> (#peek CPU_OUTPUT, iBusWishbone_CTI) ptr
      <*> (#peek CPU_OUTPUT, iBusWishbone_BTE) ptr

      <*> (#peek CPU_OUTPUT, dBusWishbone_CYC) ptr
      <*> (#peek CPU_OUTPUT, dBusWishbone_STB) ptr
      <*> (#peek CPU_OUTPUT, dBusWishbone_WE) ptr
      <*> (#peek CPU_OUTPUT, dBusWishbone_ADR) ptr
      <*> (#peek CPU_OUTPUT, dBusWishbone_DAT_MOSI) ptr
      <*> (#peek CPU_OUTPUT, dBusWishbone_SEL) ptr
      <*> (#peek CPU_OUTPUT, dBusWishbone_CTI) ptr
      <*> (#peek CPU_OUTPUT, dBusWishbone_BTE) ptr

    {-# INLINE poke #-}
    poke ptr this = do
      (#poke CPU_OUTPUT, iBusWishbone_CYC) ptr (iBusWishbone_CYC this)
      (#poke CPU_OUTPUT, iBusWishbone_STB) ptr (iBusWishbone_STB this)
      (#poke CPU_OUTPUT, iBusWishbone_WE) ptr (iBusWishbone_WE this)
      (#poke CPU_OUTPUT, iBusWishbone_ADR) ptr (iBusWishbone_ADR this)
      (#poke CPU_OUTPUT, iBusWishbone_DAT_MOSI) ptr (iBusWishbone_DAT_MOSI this)
      (#poke CPU_OUTPUT, iBusWishbone_SEL) ptr (iBusWishbone_SEL this)
      (#poke CPU_OUTPUT, iBusWishbone_CTI) ptr (iBusWishbone_CTI this)
      (#poke CPU_OUTPUT, iBusWishbone_BTE) ptr (iBusWishbone_BTE this)

      (#poke CPU_OUTPUT, dBusWishbone_CYC) ptr (dBusWishbone_CYC this)
      (#poke CPU_OUTPUT, dBusWishbone_STB) ptr (dBusWishbone_STB this)
      (#poke CPU_OUTPUT, dBusWishbone_WE) ptr (dBusWishbone_WE this)
      (#poke CPU_OUTPUT, dBusWishbone_ADR) ptr (dBusWishbone_ADR this)
      (#poke CPU_OUTPUT, dBusWishbone_DAT_MOSI) ptr (dBusWishbone_DAT_MOSI this)
      (#poke CPU_OUTPUT, dBusWishbone_SEL) ptr (dBusWishbone_SEL this)
      (#poke CPU_OUTPUT, dBusWishbone_CTI) ptr (dBusWishbone_CTI this)
      (#poke CPU_OUTPUT, dBusWishbone_BTE) ptr (dBusWishbone_BTE this)
      return ()

instance Storable JTAG_OUTPUT where
    alignment _ = #alignment JTAG_OUTPUT
    sizeOf _ = #size JTAG_OUTPUT
    {-# INLINE peek #-}
    peek ptr = const JTAG_OUTPUT <$> pure ()
      <*> (#peek JTAG_OUTPUT, debug_resetOut) ptr
      <*> (#peek JTAG_OUTPUT, jtag_TDO) ptr

    {-# INLINE poke #-}
    poke ptr this = do
      (#poke JTAG_OUTPUT, debug_resetOut) ptr (debug_resetOut this)
      (#poke JTAG_OUTPUT, jtag_TDO) ptr (jtag_TDO this)
      return ()
