-- SPDX-FileCopyrightText: 2022-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

import Clash.Prelude
import Control.Monad (forM_, when)
import GHC.Char (chr)
import GHC.IO.Handle (Handle, hFlush, hPutStr)
import Options.Applicative (Parser, auto, execParser, fullDesc, header, help, helper, info, long, option, progDesc, short, showDefault, strOption, switch, value)
import Protocols.Wishbone
import System.Exit (exitFailure)
import System.IO (IOMode (WriteMode), hPutChar, hPutStrLn, openFile, stdout)
import Text.Printf (hPrintf, printf)
import VexRiscv (CpuOut (dBusWbM2S, iBusWbM2S), DumpVcd (NoDumpVcd), JtagIn (JtagIn), JtagOut (JtagOut))
import VexRiscv.JtagTcpBridge (vexrJtagBridge)

import qualified Data.List as L

import Utils.Cpu (cpu)
import Utils.DebugConfig (DebugConfiguration (..))
import Utils.ProgramLoad (loadProgramDmem)

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

data RunOpts = RunOpts
  { a_execPath :: FilePath
  , b_execPath :: FilePath
  , a_logPath :: FilePath
  , b_logPath :: FilePath
  , a_assertResetFor :: Int
  , b_assertResetFor :: Int
  , a_assertResetAfter :: Int
  , b_assertResetAfter :: Int
  , printClockCycles :: Bool
  }

getRunOpts :: Parser RunOpts
getRunOpts =
  RunOpts
    <$> strOption
      ( short 'a'
          <> long "a-exec"
          <> help "Path to the executable for CPU A"
      )
    <*> strOption
      ( short 'b'
          <> long "b-exec"
          <> help "Path to the executable for CPU B"
      )
    <*> strOption
      ( short 'A'
          <> long "a-log"
          <> help "Path to the log file for CPU A"
      )
    <*> strOption
      ( short 'B'
          <> long "b-log"
          <> help "Path to the log file for CPU B"
      )
    <*> option
      auto
      ( long "assert-cpu-a-reset-for"
          <> help "Assert CPU A in reset for N cycles. Must be at least 2 for a proper reset. Note that this reset gets asserted after 'assert-cpu-reset-after' cycles. Simulation will always start with a 2-cycle reset."
          <> value 0
          <> showDefault
      )
    <*> option
      auto
      ( long "assert-cpu-b-reset-for"
          <> help "Assert CPU B in reset for N cycles. Must be at least 2 for a proper reset. Note that this reset gets asserted after 'assert-cpu-reset-after' cycles. Simulation will always start with a 2-cycle reset."
          <> value 0
          <> showDefault
      )
    <*> option
      auto
      ( long "assert-cpu-a-reset-after"
          <> help "Assert CPU A reset after N cycles"
          <> value 0
          <> showDefault
      )
    <*> option
      auto
      ( long "assert-cpu-b-reset-after"
          <> help "Assert CPU B reset after N cycles"
          <> value 0
          <> showDefault
      )
    <*> switch
      ( long "print-clock-cycles"
          <> help "Print number of clock cycles passed after each printed"
      )

jtagDaisyChain :: JtagIn -> JtagOut -> JtagIn
jtagDaisyChain (JtagIn tc ms _) (JtagOut to) = JtagIn tc ms to

type CpuSignals =
  ( CpuOut
  , JtagOut
  , Maybe (BitVector 32, BitVector 32)
  , WishboneS2M (BitVector 32)
  , WishboneS2M (BitVector 32)
  )

toReset :: (KnownDomain dom) => Int -> Int -> Reset dom
toReset assertForN assertAfterN =
  unsafeFromActiveHigh $ fromList (asserted0 <> deasserted <> asserted1 <> L.repeat False)
 where
  asserted0 = L.replicate 2 True
  deasserted = L.replicate assertAfterN False
  asserted1 = L.replicate assertForN True

main :: IO ()
main = do
  RunOpts{..} <- execParser opts

  (iMemA, dMemA) <-
    withClockResetEnable @System clockGen resetGen enableGen
      $ loadProgramDmem @System a_execPath

  (iMemB, dMemB) <-
    withClockResetEnable @System clockGen resetGen enableGen
      $ loadProgramDmem @System b_execPath

  logFileA <- openFile a_logPath WriteMode
  logFileB <- openFile b_logPath WriteMode

  let portNr = 7894
  jtagBridge <- vexrJtagBridge portNr
  putStrLn ("JTAG bridge ready at port " <> show portNr)
  hFlush stdout

  let
    jtagInA = jtagBridge jtagOutB
    resetA = toReset a_assertResetFor a_assertResetAfter
    cpuOutA@(unbundle -> (_circuitA, jtagOutA, _, _iBusA, _dBusA)) =
      withClock @System clockGen
        $ withReset @System resetA
        $ let (circ, jto, writes1, iBus, dBus) = cpu NoDumpVcd (Just jtagInA) iMemA dMemA
           in bundle (circ, jto, writes1, iBus, dBus)

    jtagInB = liftA2 jtagDaisyChain jtagInA jtagOutA
    resetB = toReset b_assertResetFor b_assertResetAfter
    cpuOutB@(unbundle -> (_circuitB, jtagOutB, _, _iBusB, _dBusB)) =
      withClock @System clockGen
        $ withReset @System resetB
        $ let (circ, jto, writes1, iBus, dBus) = cpu NoDumpVcd (Just jtagInB) iMemB dMemB
           in bundle (circ, jto, writes1, iBus, dBus)
    cpuOut = bundle (cpuOutA, cpuOutB)

  runSampling
    printClockCycles
    debugConfig
    (logFileA, logFileB)
    cpuOut
 where
  opts =
    info
      (getRunOpts <**> helper)
      ( fullDesc
          <> progDesc "Run binaries on two Vex RISC-V CPUs linked with JTAG chaining"
          <> header "vex-riscv-chain-simulation - a test for JTAG chaining"
      )

runSampling ::
  -- | Print clock cycles whenever a line is printed
  Bool ->
  DebugConfiguration ->
  (Handle, Handle) ->
  Signal System (CpuSignals, CpuSignals) ->
  IO ()
runSampling printClockCycles dbg (handleA, handleB) cpusOutputs = do
  case dbg of
    RunCharacterDevice ->
      forM_
        (L.zip [(0 :: Int) ..] (sample_lazy @System (bundle (register @System (unpack 0) cpusOutputs, cpusOutputs))))
        $ \(nCycle, ((a1, b1), (a0, b0))) -> do
          runCharacterDevice printClockCycles nCycle handleA a1 a0
          runCharacterDevice printClockCycles nCycle handleB b1 b0
    InspectBusses initCycles uninteresting interesting iEnabled dEnabled -> do
      let
        skipTotal = initCycles + uninteresting
        sampled = case interesting of
          Nothing -> L.zip [0 ..] $ sample_lazy @System cpusOutputs
          Just nInteresting ->
            let total = initCycles + uninteresting + nInteresting
             in L.zip [0 ..] $ L.take total $ sample_lazy @System cpusOutputs
      forM_ sampled $ \(i, (cpuOutA, cpuOutB)) -> do
        runInspectBusses handleA skipTotal iEnabled dEnabled i cpuOutA
        runInspectBusses handleB skipTotal iEnabled dEnabled i cpuOutB
    InspectWrites ->
      forM_
        (sample_lazy @System cpusOutputs)
        $ \((_, _, writesA, _, _), (_, _, writesB, _, _)) -> do
          case (writesA, writesB) of
            (Just (aA, vA), Just (aB, vB)) -> do
              hPrintf handleA "W: % 8X\n" (toInteger aA) (toInteger vA)
              hPrintf handleB "W: % 8X\n" (toInteger aB) (toInteger vB)
            (Just (a, v), Nothing) -> hPrintf handleA "W: % 8X\n" (toInteger a) (toInteger v)
            (Nothing, Just (a, v)) -> hPrintf handleB "W: % 8X\n" (toInteger a) (toInteger v)
            _ -> pure ()

runCharacterDevice ::
  -- | Print clock cycles whenever a line is printed
  Bool ->
  Int ->
  Handle ->
  CpuSignals ->
  CpuSignals ->
  IO ()
runCharacterDevice printClockCycles nCycle logFile (_, _, write, dS2M, iS2M) (out1, _, _, _, _) = do
  when (err dS2M) $ do
    let dBusM2S = dBusWbM2S out1
    let dAddr = toInteger (addr dBusM2S) -- `shiftL` 2
    hPrintf
      logFile
      "D-bus ERR reply % 8X (% 8X)\n"
      (toInteger $ dAddr `shiftL` 2)
      (toInteger dAddr)
    exitFailure

  when (err iS2M) $ do
    let iBusM2S = iBusWbM2S out1
    let iAddr = toInteger (addr iBusM2S) -- `shiftL` 2
    hPrintf
      logFile
      "I-bus ERR reply % 8X (% 8X)\n"
      (toInteger $ iAddr `shiftL` 2)
      (toInteger iAddr)
    hPrintf logFile "%s\n" (show iBusM2S)
    exitFailure

  case write of
    Just (address, val) | address == 0x0000_1000 -> do
      let
        (_ :: BitVector 24, b :: BitVector 8) = unpack val
        char = chr (fromEnum b)
      hPutChar logFile char
      when (char == '\n') $ do
        when printClockCycles $ do
          hPutStrLn logFile ("[CPU] clock cycle: " <> show nCycle)
        hFlush logFile
    _ -> pure ()

runInspectBusses ::
  Handle ->
  Int ->
  Bool ->
  Bool ->
  Int ->
  CpuSignals ->
  IO ()
runInspectBusses
  logFile
  skipTotal
  iEnabled
  dEnabled
  i
  (out, _, _, iBusS2M, dBusS2M) =
    do
      let doPrint = i >= skipTotal

      when (doPrint && iEnabled) $ do
        let
          iBusM2S = iBusWbM2S out
          iAddr = toInteger (addr iBusM2S) -- `shiftL` 2
          cyc = if busCycle iBusM2S then "CYC" else "   "
          stb = if strobe iBusM2S then "STB" else "   "
          iResp =
            if
              | acknowledge iBusS2M -> "ACK  "
              | err iBusS2M -> "ERR  "
              | retry iBusS2M -> "RETRY"
              | otherwise -> "NONE "
          iRespData =
            if acknowledge iBusS2M
              then printf "% 8X" (toInteger $ readData iBusS2M)
              else "<no data>"

        hPutStr logFile
          $ "iM2S: ("
          <> (cyc <> " " <> stb)
          <> ") ("
          <> showX (busSelect iBusM2S)
          <> ") "
          <> printf "% 8X" iAddr
          <> " ("
          <> printf "%X" (iAddr `shiftL` 2)
          <> ")"
        hPutStrLn logFile $ "iS2M: " <> iResp <> " - " <> iRespData

      when
        (err iBusS2M)
        exitFailure

      when (doPrint && dEnabled) $ do
        let
          dBusM2S = dBusWbM2S out
          dAddr = toInteger (addr dBusM2S) -- `shiftL 2`
          dWrite = writeEnable dBusM2S
          cyc = if busCycle dBusM2S then "CYC" else "   "
          stb = if strobe dBusM2S then "STB" else "   "
          dValid = busCycle dBusM2S && strobe dBusM2S
          dActive = busCycle dBusM2S
          mode = if dWrite then "W" else "R"
          dResp =
            if
              | acknowledge dBusS2M -> "ACK  "
              | err dBusS2M -> "ERR  "
              | retry dBusS2M -> "RETRY"
              | otherwise -> "NONE "
          dRespData
            | acknowledge dBusS2M && hasUndefined (readData dBusS2M) && not dWrite = printf " - undefined!!"
            | acknowledge dBusS2M && not dWrite = printf " - % 8X" (toInteger $ readData dBusS2M)
            | not dWrite = " - <no data>"
            | otherwise = ""
          writeDat =
            if dValid && dWrite
              then printf "% 8X" (toInteger $ writeData dBusM2S)
              else " no data"

        when (dActive || hasTerminateFlag dBusS2M) $ do
          hPutStr logFile
            $ "dM2S: "
            <> mode
            <> " ("
            <> (cyc <> " " <> stb)
            <> ") ("
            <> showX (busSelect dBusM2S)
            <> ") "
            <> printf "% 8X" dAddr
            <> " ("
            <> printf "% 8X" (dAddr `shiftL` 2)
            <> ") <"
            <> writeDat
            <> "> - "
          hPutStrLn logFile
            $ "dS2M: "
            <> dResp
            <> dRespData

      when
        (err dBusS2M)
        exitFailure
