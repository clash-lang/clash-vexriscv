-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

import Clash.Prelude

import Protocols.Wishbone
import VexRiscv (Output(iBusWbM2S, dBusWbM2S))

import qualified Data.List as L

import Control.Monad (forM_, when)
import Data.Char (chr)
import Data.Maybe (catMaybes)
import System.Environment (getArgs)
import System.IO (putChar, hFlush, stdout)
import Text.Printf (printf)


import Utils.ProgramLoad (loadProgram)
import Utils.Cpu (cpu)
import System.Exit (exitFailure)
import System.Directory.Internal.Prelude (exitFailure)

--------------------------------------
--
-- Debugging configuration
--
--------------------------------------

data DebugConfiguration where
  -- | Run a program and only output what the program writes to the
  --   character-device
  RunCharacterDevice :: DebugConfiguration
  -- | Run a program and print detailed information of CPU bus interactions
  InspectBusses ::
    -- | # of cycles the program takes to initialise the instruction memory
    Int ->
    -- | # of "uninteresting" cycles to skip, such as runtime setup code
    Int ->
    -- | # of "interesting" cycles to inspect
    Maybe Int ->
    -- | inspect instruct-bus interactions
    Bool ->
    -- | inspect data-bus interactions
    Bool ->
    DebugConfiguration
  -- | Run a program and output all the write operations
  InspectWrites :: DebugConfiguration

-- change this variable to the configuration you want to use

debugConfig :: DebugConfiguration
debugConfig =
  -- InspectWrites
  RunCharacterDevice

--
{-
  InspectBusses
    0
    0
    (Just 100)
    True
    True
-- -}

--------------------------------------


main :: IO ()
main = do
  elfFile <- L.head <$> getArgs

  (iMem, dMem) <-
    withClockResetEnable @System clockGen resetGen enableGen $
      loadProgram @System elfFile

  let cpuOut@(unbundle -> (_circuit, writes, _iBus, _dBus)) =
        withClockResetEnable @System clockGen (resetGenN (SNat @2)) enableGen $
          let (circ, writes1, iBus, dBus) = cpu (Just 7894) iMem dMem
              dBus' = register emptyWishboneS2M dBus
          in bundle (circ, writes1, iBus, dBus')

  case debugConfig of
    RunCharacterDevice ->
      forM_ (sample_lazy @System (bundle (register @System (unpack 0) cpuOut, cpuOut))) $
        \((out, write, dS2M, iS2M), (out1, _write, _dS2M, _iS2M)) -> do
        when (err dS2M) $ do
          let dBusM2S = dBusWbM2S out1
          let dAddr = toInteger (addr dBusM2S) -- `shiftL` 2
          printf "D-bus ERR reply % 8X (% 8X)\n" (toInteger $ dAddr `shiftL` 2) (toInteger dAddr)
          -- exitFailure

        when (err iS2M) $ do
          let iBusM2S = iBusWbM2S out1
          let iAddr = toInteger (addr iBusM2S) -- `shiftL` 2
          printf "I-bus ERR reply % 8X (% 8X)\n" (toInteger $ iAddr `shiftL` 2) (toInteger iAddr)
          printf "%s" (showX iBusM2S)
          -- exitFailure

        case write of
          Just (address, value) | address == 0x0000_1000 -> do
            let (_ :: BitVector 24, b :: BitVector 8) = unpack value
            -- putChar $ chr (fromEnum b)
            -- hFlush stdout
            pure ()
          _ -> pure ()
      -- performPrintsToStdout 0x0000_1000 (sample_lazy $ bitCoerce <$> writes)
    InspectBusses initCycles uninteresting interesting iEnabled dEnabled -> do

      let skipTotal = initCycles + uninteresting

      let sampled = case interesting of
            Nothing -> L.zip [0 ..] $ sample_lazy @System cpuOut
            Just nInteresting ->
              let total = initCycles + uninteresting + nInteresting in L.zip [0 ..] $ L.take total $ sample_lazy @System cpuOut

      forM_ sampled $ \(i, (out, _, iBusS2M, dBusS2M)) -> do
        let doPrint = i >= skipTotal

        -- I-bus interactions

        when (doPrint && iEnabled) $ do
          let iBusM2S = iBusWbM2S out
          let iAddr = toInteger (addr iBusM2S) -- `shiftL` 2

          let cyc = if busCycle iBusM2S then "CYC" else "   "
          let stb = if strobe iBusM2S then "STB" else "   "

          let iResp =
                if
                  | acknowledge iBusS2M -> "ACK  "
                  | err iBusS2M -> "ERR  "
                  | retry iBusS2M -> "RETRY"
                  | otherwise -> "NONE "

          let iRespData =
                if acknowledge iBusS2M
                  then printf "% 8X" (toInteger $ readData iBusS2M)
                  else "<no data>"

          putStr $
            "iM2S:   ("
              <> (cyc <> " " <> stb)
              <> ") "
              <> "("
              <> showX (busSelect iBusM2S)
              <> ") "
              <> printf "% 8X" iAddr
              <> " ("
              <> printf "%X" (iAddr `shiftL` 2)
              <> ")"
          putStrLn $ "            - iS2M: " <> iResp <> " - " <> iRespData

        when (err iBusS2M)
          exitFailure

        -- D-bus interactions

        when (doPrint && dEnabled) $ do
          let dBusM2S = dBusWbM2S out
          let dAddr = toInteger (addr dBusM2S) -- `shiftL` 2
          let dWrite = writeEnable dBusM2S
          let cyc = if busCycle dBusM2S then "CYC" else "   "
          let stb = if strobe dBusM2S then "STB" else "   "
          let dValid = busCycle dBusM2S && strobe dBusM2S
          let dActive = busCycle dBusM2S

          let mode = if dWrite then "W" else "R"

          let dResp =
                if
                    | acknowledge dBusS2M -> "ACK  "
                    | err dBusS2M -> "ERR  "
                    | retry dBusS2M -> "RETRY"
                    | otherwise -> "NONE "

          let dRespData
                | acknowledge dBusS2M && hasUndefined (readData dBusS2M) && not dWrite = printf " - undefined!!"
                | acknowledge dBusS2M && not dWrite = printf " - % 8X" (toInteger $ readData dBusS2M)
                | not dWrite = " - <no data>"
                | otherwise = ""

          let writeDat =
                if dValid && dWrite
                  then printf "% 8X" (toInteger $ writeData dBusM2S)
                  else " no data"

          when (dActive || hasTerminateFlag dBusS2M) $ do
            putStr $
              "dM2S: "
                <> mode
                <> " ("
                <> (cyc <> " " <> stb)
                <> ") "
                <> "("
                <> showX (busSelect dBusM2S)
                <> ") "
                <> printf "% 8X" dAddr
                <> " ("
                <> printf "% 8X" (dAddr `shiftL` 2)
                <> ") "
                <> "<"
                <> writeDat
                <> "> - "
            putStrLn $ "dS2M: " <> dResp <> dRespData

        when (err dBusS2M)
          exitFailure
    InspectWrites ->
      forM_ (catMaybes $ sample_lazy @System writes) $ \(address, value) -> do
        printf "W: % 8X <- % 8X\n" (toInteger address) (toInteger value)
