-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RecordWildCards #-}
module VexRiscv.JtagTcpBridge (vexrJtagBridge, defaultIn) where

import Clash.Prelude

import Clash.Signal.Internal

import VexRiscv
import VexRiscv.FFI

import Foreign
import System.IO.Unsafe (unsafePerformIO)
import Network.Socket (PortNumber)

defaultIn :: JtagIn
defaultIn = JtagIn { testModeSelect = low, testDataIn = low }

vexrJtagBridge :: PortNumber -> Signal dom JtagOut -> (Signal dom JtagIn, Enable dom)
vexrJtagBridge port out =
    let (_, jtagBridgeStep) = unsafePerformIO $ vexrJtagBridge' port

        {-# NOINLINE inner #-}
        inner (o :- outs) = unsafePerformIO $ do
            (in', en) <- jtagBridgeStep o
            let (ins', ens') = inner outs
            pure $ (in' :- (in' `deepseqX` ins'), en :- (en `deepseqX` ens'))
    
        (ins, ens) = inner out
    in (ins, toEnable ens)

vexrJtagBridge' ::
    PortNumber ->
    IO ( IO () -- ^ delete function
       , JtagOut -> IO (JtagIn, Bool) -- ^ step, last value is enable
       )
vexrJtagBridge' port = do
    bridge <- vexrJtagBridgeInit (fromIntegral port)
    let
        shutDown = vexrJtagBridgeShutdown bridge

        step JtagOut{..} = alloca $ \outFFI -> alloca $ \inFFI -> alloca $ \enFFI -> do
            poke outFFI (JTAG_OUTPUT testDataOut debugReset)
            vexrJtagBridgeStep bridge outFFI inFFI enFFI
            JTAG_INPUT{..} <- peek inFFI
            en <- peek enFFI
            let input = JtagIn { testModeSelect = jtag_TMS, testDataIn = jtag_TDI }
            pure (input, bitToBool en)
    pure (shutDown, step)