-- SPDX-FileCopyrightText: 2022-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

module Tests.JtagChain where

import Control.Monad (when)
import Control.Monad.Extra (unlessM)
import Data.Data (Proxy (Proxy))
import Data.Maybe (fromJust)
import GHC.Stack (HasCallStack)
import System.Directory (doesPathExist)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.IO (IOMode (ReadMode, WriteMode), openFile, withFile)
import System.Process
import Test.Tasty (TestTree, askOption, defaultIngredients, defaultMainWithIngredients, includingOptions, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@=?))
import Test.Tasty.Options (OptionDescription (Option))
import Prelude

import qualified Streaming.Prelude as SP

import Tests.Jtag (JtagDebug (JtagDebug), cabalListBin, getGdb)
import Utils.FilePath (BuildType (Debug), cabalProject, findParentContaining, rustBinsDir)

getSimulateExecPath :: IO FilePath
getSimulateExecPath = cabalListBin "clash-vexriscv-sim:clash-vexriscv-chain-bin"

getProjectRoot :: IO FilePath
getProjectRoot = findParentContaining cabalProject

test ::
  -- | Print debug output of subprocesses
  Bool ->
  Assertion
test debug = do
  simulateExecPath <- getSimulateExecPath
  projectRoot <- getProjectRoot
  let
    rBD = rustBinsDir projectRoot "riscv32imc-unknown-none-elf" Debug
    printAElfPath = rBD </> "print_a"
    logAPath = projectRoot </> "testBoth_cpu_a.log"
    printBElfPath = rBD </> "print_b"
    logBPath = projectRoot </> "testBoth_cpu_b.log"
    simDataDir = projectRoot </> "clash-vexriscv-sim" </> "data"
    openocdCfgPath = simDataDir </> "vexriscv_chain_sim.cfg"
    gdbCmdPathA = simDataDir </> "vexriscv_chain_gdba.cfg"
    gdbCmdPathB = simDataDir </> "vexriscv_chain_gdbb.cfg"
  gdb <- getGdb
  currentEnv <- getEnvironment

  ensureExists logAPath
  ensureExists logBPath

  let
    gdbPort = 3334 :: Integer
    jtagBridgePort = 7895 :: Integer

    vexRiscvProc =
      ( proc
          simulateExecPath
          ["-a", printAElfPath, "-b", printBElfPath, "-A", logAPath, "-B", logBPath]
      )
        { std_out = CreatePipe
        , cwd = Just projectRoot
        , env = Just (("JTAG_BRIDGE_PORT", show jtagBridgePort) : currentEnv)
        }

    openOcdProc =
      ( proc
          "openocd-riscv"
          [ "-c"
          , "gdb port " <> show gdbPort
          , "-c"
          , "set _REMOTE_BITBANG_PORT " <> show jtagBridgePort
          , "-f"
          , openocdCfgPath
          ]
      )
        { std_err = CreatePipe
        , cwd = Just projectRoot
        }

    gdbProcA =
      ( proc
          gdb
          [ "--eval-command"
          , "set $EXTENDED_REMOTE_PORT = " <> show gdbPort
          , "--command"
          , gdbCmdPathA
          ]
      )
        { cwd = Just projectRoot
        , std_out = CreatePipe
        }

    gdbProcB =
      ( proc
          gdb
          [ "--eval-command"
          , "set $EXTENDED_REMOTE_PORT = " <> show (gdbPort + 1)
          , "--command"
          , gdbCmdPathB
          ]
      )
        { cwd = Just projectRoot
        , std_out = CreatePipe
        }

  withCreateProcess vexRiscvProc $ \_ (fromJust -> simStdOut) _ _ -> do
    logAHandle <- openFile logAPath ReadMode
    logBHandle <- openFile logBPath ReadMode
    let
      logA0 = SP.fromHandle logAHandle
      logB0 = SP.fromHandle logBHandle
      simStdOut0 = SP.fromHandle simStdOut

    _ <- waitForLineInStream debug simStdOut0 ("JTAG bridge ready at port " <> show jtagBridgePort)

    logA1 <- expectLineFromStream debug logA0 "[CPU] a"
    logB1 <- expectLineFromStream debug logB0 "[CPU] b"

    withCreateProcess openOcdProc $ \_ _ (fromJust -> openOcdStdErr) _ -> do
      let openOcdStream = SP.fromHandle openOcdStdErr
      _ <- waitForLineInStream debug openOcdStream "Halting processor"

      withCreateProcess gdbProcA $ \_ _ _ gdbProcHandleA -> do
        withCreateProcess gdbProcB $ \_ _ _ gdbProcHandleB -> do
          _ <- expectLineFromStream debug logA1 "[CPU] a"
          _ <- expectLineFromStream debug logB1 "[CPU] b"
          _ <- expectLineFromStream debug logA1 "[CPU] b"
          _ <- expectLineFromStream debug logB1 "[CPU] a"

          gdbAExitCode <- waitForProcess gdbProcHandleA
          gdbBExitCode <- waitForProcess gdbProcHandleB
          ExitSuccess @=? gdbAExitCode
          ExitSuccess @=? gdbBExitCode

<<<<<<< Updated upstream
||||||| Stash base
testInResetA ::
  -- | Print debug output of subprocesses
  Bool ->
  Assertion
testInResetA debug = do
  simulateExecPath <- getSimulateExecPath
  projectRoot <- getProjectRoot
  let
    rBD = rustBinsDir projectRoot "riscv32imc-unknown-none-elf" Debug
    printAElfPath = rBD </> "print_a"
    logAPath = projectRoot </> "cpu_a.log"
    printBElfPath = rBD </> "print_b"
    logBPath = projectRoot </> "cpu_b.log"
    simDataDir = projectRoot </> "clash-vexriscv-sim" </> "data"
    openocdCfgPath = simDataDir </> "vexriscv_chain_sim.cfg"
    gdbCmdPathB = simDataDir </> "vexriscv_chain_gdbb.cfg"
  gdb <- getGdb

  ensureExists logAPath
  ensureExists logBPath

  let
    vexRiscvProc =
      ( proc
          simulateExecPath
          ["-a", printAElfPath, "-b", printBElfPath, "-A", logAPath, "-B", logBPath, "--keep-cpu-a-in-reset"]
      )
        { std_out = CreatePipe
        , cwd = Just projectRoot
        }

    openOcdProc =
      (proc "openocd-riscv" ["-f", openocdCfgPath])
        { std_err = CreatePipe
        , cwd = Just projectRoot
        }

    gdbProcB =
      (proc gdb ["--command", gdbCmdPathB])
        { cwd = Just projectRoot
        , std_out = CreatePipe
        }

  withCreateProcess vexRiscvProc $ \_ (fromJust -> simStdOut) _ _ -> do
    logBHandle <- openFile logBPath ReadMode
    let
      logB0 = SP.fromHandle logBHandle
      simStdOut0 = SP.fromHandle simStdOut

    _ <- waitForLineInStream debug simStdOut0 "JTAG bridge ready at port 7894"

    logB1 <- expectLineFromStream debug logB0 "[CPU] b"

    withCreateProcess openOcdProc $ \_ _ (fromJust -> openOcdStdErr) _ -> do
      let openOcdStream = SP.fromHandle openOcdStdErr
      _ <- waitForLineInStream debug openOcdStream "Error: [riscv.cpu0] Examination failed"
      _ <- waitForLineInStream debug openOcdStream "[riscv.cpu1] Target successfully examined."
      _ <- waitForLineInStream debug openOcdStream "Halting processor"

      withCreateProcess gdbProcB $ \_ _ _ gdbProcHandleB -> do
        _ <- expectLineFromStream debug logB1 "[CPU] b"
        _ <- expectLineFromStream debug logB1 "[CPU] a"

        gdbBExitCode <- waitForProcess gdbProcHandleB
        ExitSuccess @=? gdbBExitCode

=======
testInResetA ::
  -- | Print debug output of subprocesses
  Bool ->
  Assertion
testInResetA debug = do
  simulateExecPath <- getSimulateExecPath
  projectRoot <- getProjectRoot
  let
    rBD = rustBinsDir projectRoot "riscv32imc-unknown-none-elf" Debug
    printAElfPath = rBD </> "print_a"
    logAPath = projectRoot </> "testInResetA_cpu_a.log"
    printBElfPath = rBD </> "print_b"
    logBPath = projectRoot </> "testInResetA_cpu_b.log"
    simDataDir = projectRoot </> "clash-vexriscv-sim" </> "data"
    openocdCfgPath = simDataDir </> "vexriscv_chain_sim.cfg"
    gdbCmdPathB = simDataDir </> "vexriscv_chain_gdbb.cfg"
  gdb <- getGdb
  currentEnv <- getEnvironment

  ensureExists logAPath
  ensureExists logBPath

  let
    gdbPort = 3336 :: Integer
    jtagBridgePort = 7896 :: Integer

    vexRiscvProc =
      ( proc
          simulateExecPath
          [ "-a"
          , printAElfPath
          , "-b"
          , printBElfPath
          , "-A"
          , logAPath
          , "-B"
          , logBPath
          , "--keep-cpu-a-in-reset"
          ]
      )
        { std_out = CreatePipe
        , cwd = Just projectRoot
        , env = Just (("JTAG_BRIDGE_PORT", show jtagBridgePort) : currentEnv)
        }

    openOcdProc =
      ( proc
          "openocd-riscv"
          [ "-c"
          , "gdb port " <> show gdbPort
          , "-c"
          , "set _REMOTE_BITBANG_PORT " <> show jtagBridgePort
          , "-f"
          , openocdCfgPath
          ]
      )
        { std_err = CreatePipe
        , cwd = Just projectRoot
        }

    gdbProcB =
      ( proc
          gdb
          [ "--eval-command"
          , "set $EXTENDED_REMOTE_PORT = " <> show (gdbPort + 1)
          , "--command"
          , gdbCmdPathB
          ]
      )
        { cwd = Just projectRoot
        , std_out = CreatePipe
        }

  withCreateProcess vexRiscvProc $ \_ (fromJust -> simStdOut) _ _ -> do
    logBHandle <- openFile logBPath ReadMode
    let
      logB0 = SP.fromHandle logBHandle
      simStdOut0 = SP.fromHandle simStdOut

    _ <- waitForLineInStream debug simStdOut0 ("JTAG bridge ready at port " <> show jtagBridgePort)

    logB1 <- expectLineFromStream debug logB0 "[CPU] b"

    withCreateProcess openOcdProc $ \_ _ (fromJust -> openOcdStdErr) _ -> do
      let openOcdStream = SP.fromHandle openOcdStdErr
      _ <- waitForLineInStream debug openOcdStream "Error: [riscv.cpu0] Examination failed"
      _ <- waitForLineInStream debug openOcdStream "[riscv.cpu1] Target successfully examined."
      _ <- waitForLineInStream debug openOcdStream "Halting processor"

      withCreateProcess gdbProcB $ \_ _ _ gdbProcHandleB -> do
        _ <- expectLineFromStream debug logB1 "[CPU] b"
        _ <- expectLineFromStream debug logB1 "[CPU] a"

        gdbBExitCode <- waitForProcess gdbProcHandleB
        ExitSuccess @=? gdbBExitCode

>>>>>>> Stashed changes
ensureExists :: (HasCallStack) => FilePath -> IO ()
ensureExists path = unlessM (doesPathExist path) (withFile path WriteMode (\_ -> pure ()))

expectLineFromStream ::
  (HasCallStack) =>
  Bool ->
  SP.Stream (SP.Of String) IO () ->
  String ->
  IO (SP.Stream (SP.Of String) IO ())
expectLineFromStream debug stream lookFor = do
  result <- SP.next stream
  case result of
    Right (out, next) -> do
      when debug $ putStrLn $ "DBG(E): " <> out
      if out == lookFor
        then return next
        else errorHelper lookFor out
    Left _ -> expectLineFromStream debug stream lookFor

waitForLineInStream ::
  (HasCallStack) =>
  Bool ->
  SP.Stream (SP.Of String) IO () ->
  String ->
  IO (SP.Stream (SP.Of String) IO ())
waitForLineInStream debug stream lookFor = do
  result <- SP.next stream
  case result of
    Right (out, next) -> do
      when debug $ putStrLn $ "DBG(W): " <> out
      if out == lookFor
        then return next
        else waitForLineInStream debug next lookFor
    Left _ -> expectLineFromStream debug stream lookFor

errorHelper :: (HasCallStack) => String -> String -> m a
errorHelper expected found = error ("expected `" <> expected <> "`, found `" <> found <> "`")

tests :: TestTree
tests = askOption $ \(JtagDebug debug) ->
  testGroup
    "JTAG chaining"
    [ testCase "Basic GDB commands, breakpoints, and program loading" (test debug)
    ]

main :: IO ()
main =
  defaultMainWithIngredients
    (includingOptions [Option (Proxy :: Proxy JtagDebug)] : defaultIngredients)
    tests
