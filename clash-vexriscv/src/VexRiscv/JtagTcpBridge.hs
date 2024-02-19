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
defaultIn = JtagIn { testClock = low, testModeSelect = low, testDataIn = low }

vexrJtagBridge :: PortNumber -> Signal dom JtagOut -> Signal dom JtagIn
vexrJtagBridge port out =
    let (_, jtagBridgeStep) = unsafePerformIO $ vexrJtagBridge' port

        {-# NOINLINE inner #-}
        inner (o :- outs) = unsafePerformIO $ do
            in' <- jtagBridgeStep o
            let ins' = inner outs
            pure $ in' :- (in' `deepseqX` ins')
    in inner out

vexrJtagBridge' ::
    PortNumber ->
    IO ( IO () -- ^ delete function
       , JtagOut -> IO JtagIn -- ^ step function
       )
vexrJtagBridge' port = do
    bridge <- vexrJtagBridgeInit (fromIntegral port)
    let
        shutDown = vexrJtagBridgeShutdown bridge

        step JtagOut{..} = alloca $ \outFFI -> alloca $ \inFFI -> do
            poke outFFI (JTAG_OUTPUT debugReset testDataOut)
            vexrJtagBridgeStep bridge outFFI inFFI
            JTAG_INPUT{..} <- peek inFFI
            let input = JtagIn { testClock = jtag_TCK, testModeSelect = jtag_TMS, testDataIn = jtag_TDI }
            pure input
    pure (shutDown, step)
