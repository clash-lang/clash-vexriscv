-- SPDX-FileCopyrightText: 2022-2024 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

module Tests.JtagChain where

import Clash.Prelude (KnownNat, Vec (Nil, (:>)), toList)
import Clash.Sized.Vector (unsafeFromList)
import Clash.Sized.Vector.ToTuple (vecToTuple)

import Control.Monad.Extra (unlessM)
import Data.Data (Proxy (Proxy))
import Data.Maybe (fromJust)
import GHC.Stack (HasCallStack)
import System.Directory (doesPathExist)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.IO (Handle, IOMode (WriteMode), withFile)
import System.Process
import Test.Tasty (TestTree, askOption, defaultIngredients, defaultMainWithIngredients, includingOptions, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@=?))
import Test.Tasty.Options (OptionDescription (Option))
import Prelude

import Tests.Jtag (JtagDebug (JtagDebug), cabalListBin, expectLineOrTimeout, getGdb, waitForLineOrTimeout)
import Utils.FilePath (BuildType (Debug), cabalProject, findParentContaining, rustBinsDir)

getSimulateExecPath :: IO FilePath
getSimulateExecPath = cabalListBin "clash-vexriscv-sim:clash-vexriscv-chain-bin"

getProjectRoot :: IO FilePath
getProjectRoot = findParentContaining cabalProject

data Args = Args
  { vexRiscvProc :: CreateProcess
  , openOcdProc :: CreateProcess
  , gdbProcA :: CreateProcess
  , gdbProcB :: CreateProcess
  , logPathA :: FilePath
  , logPathB :: FilePath
  }

createArgs :: IO Args
createArgs = do
  simulateExecPath <- getSimulateExecPath
  projectRoot <- getProjectRoot
  gdb <- getGdb

  let
    rBD = rustBinsDir projectRoot "riscv32imc-unknown-none-elf" Debug
    printAElfPath = rBD </> "print_a"
    logPathA = projectRoot </> "cpu_a.log"
    printBElfPath = rBD </> "print_b"
    logPathB = projectRoot </> "cpu_b.log"
    simDataDir = projectRoot </> "clash-vexriscv-sim" </> "data"
    openocdCfgPath = simDataDir </> "vexriscv_chain_sim.cfg"
    gdbCmdPathA = simDataDir </> "vexriscv_chain_gdba.cfg"
    gdbCmdPathB = simDataDir </> "vexriscv_chain_gdbb.cfg"

    vexRiscvProc =
      ( proc
          simulateExecPath
          ["-a", printAElfPath, "-b", printBElfPath, "-A", logPathA, "-B", logPathB]
      )
        { std_out = CreatePipe
        , cwd = Just projectRoot
        }

    openOcdProc =
      (proc "openocd-riscv" ["-f", openocdCfgPath])
        { std_err = CreatePipe
        , cwd = Just projectRoot
        }

    gdbProcA =
      (proc gdb ["--command", gdbCmdPathA])
        { cwd = Just projectRoot
        , std_out = CreatePipe
        }

    gdbProcB =
      (proc gdb ["--command", gdbCmdPathB])
        { cwd = Just projectRoot
        , std_out = CreatePipe
        }

  ensureExists logPathA
  ensureExists logPathB

  pure $
    Args
      { vexRiscvProc
      , openOcdProc
      , gdbProcA
      , gdbProcB
      , logPathA
      , logPathB
      }

testBoth ::
  (HasCallStack) =>
  -- | Print debug output of subprocesses
  Bool ->
  Assertion
testBoth debug = do
  let
    -- Timeout after 120 seconds. Warning: removing the type signature breaks
    -- stack traces.
    expectLine :: (HasCallStack) => Bool -> Handle -> String -> Assertion
    expectLine = expectLineOrTimeout 120_000_000

    waitForLine :: (HasCallStack) => Bool -> Handle -> String -> Assertion
    waitForLine = waitForLineOrTimeout 120_000_000

  Args{vexRiscvProc, openOcdProc, gdbProcA, gdbProcB, logPathA, logPathB} <- createArgs

  withStreamingFiles (logPathA :> logPathB :> Nil) $ \(vecToTuple -> (logA, logB)) -> do
    withCreateProcess vexRiscvProc $ \_ (fromJust -> simStdOut) _ _ -> do
      waitForLine debug simStdOut "JTAG bridge ready at port 7894"

      expectLine debug logA "[CPU] a"
      expectLine debug logB "[CPU] b"

      withCreateProcess openOcdProc $ \_ _ (fromJust -> openOcdStdErr) _ -> do
        waitForLine debug openOcdStdErr "[riscv.cpu0] Target successfully examined."
        waitForLine debug openOcdStdErr "[riscv.cpu1] Target successfully examined."
        waitForLine debug openOcdStdErr "Halting processor"

        withCreateProcess gdbProcA $ \_ _ _ gdbProcHandleA -> do
          withCreateProcess gdbProcB $ \_ _ _ gdbProcHandleB -> do
            expectLine debug logA "[CPU] a"
            expectLine debug logB "[CPU] b"
            expectLine debug logA "[CPU] b"
            expectLine debug logB "[CPU] a"

            gdbAExitCode <- waitForProcess gdbProcHandleA
            gdbBExitCode <- waitForProcess gdbProcHandleB
            ExitSuccess @=? gdbAExitCode
            ExitSuccess @=? gdbBExitCode

withStreamingFile :: FilePath -> (Handle -> IO a) -> IO a
withStreamingFile path f =
  let tailProc = (proc "tail" ["-n", "0", "-f", path]){std_out = CreatePipe}
   in withCreateProcess tailProc (\_ (fromJust -> h) _ _ -> f h)

withStreamingFiles :: (KnownNat n) => Vec n FilePath -> (Vec n Handle -> IO a) -> IO a
withStreamingFiles paths f = go (toList paths) []
 where
  go [] hs = f (unsafeFromList (reverse hs))
  go (p : ps) hs = withStreamingFile p (\h -> go ps (h : hs))

addArgs :: CreateProcess -> [String] -> CreateProcess
addArgs cp newArgs = cp{cmdspec = addArgsCmdSpec (cmdspec cp)}
 where
  addArgsCmdSpec (RawCommand cmd args) = RawCommand cmd (args <> newArgs)
  addArgsCmdSpec (ShellCommand cmd) = ShellCommand (cmd <> " " <> unwords newArgs)

testInResetA ::
  (HasCallStack) =>
  -- | Print debug output of subprocesses
  Bool ->
  Assertion
testInResetA debug = do
  Args{vexRiscvProc, openOcdProc, gdbProcB, logPathB} <- createArgs

  let
    -- Timeout after 240 seconds. These tests are extremely slow, because a lot
    -- of bandwidth is reserved to keep examining the CPU in reset (?).
    --
    -- Warning: removing the type signature breaks stack traces.
    expectLine :: (HasCallStack) => Bool -> Handle -> String -> Assertion
    expectLine = expectLineOrTimeout 240_000_000

    waitForLine :: (HasCallStack) => Bool -> Handle -> String -> Assertion
    waitForLine = waitForLineOrTimeout 240_000_000

  let vexRiscvProc1 = addArgs vexRiscvProc ["--keep-cpu-a-in-reset"]

  withStreamingFile logPathB $ \logB -> do
    withCreateProcess vexRiscvProc1 $ \_ (fromJust -> simStdOut) _ _ -> do
      waitForLine debug simStdOut "JTAG bridge ready at port 7894"

      expectLine debug logB "[CPU] b"

      withCreateProcess openOcdProc $ \_ _ (fromJust -> openOcdStdErr) _ -> do
        waitForLine debug openOcdStdErr "Error: [riscv.cpu0] Examination failed"
        waitForLine debug openOcdStdErr "[riscv.cpu1] Target successfully examined."
        waitForLine debug openOcdStdErr "Halting processor"

        withCreateProcess gdbProcB $ \_ _ _ gdbProcHandleB -> do
          expectLine debug logB "[CPU] b"
          expectLine debug logB "[CPU] a"

          gdbBExitCode <- waitForProcess gdbProcHandleB
          ExitSuccess @=? gdbBExitCode

ensureExists :: (HasCallStack) => FilePath -> IO ()
ensureExists path = unlessM (doesPathExist path) (withFile path WriteMode (\_ -> pure ()))

errorHelper :: (HasCallStack) => String -> String -> m a
errorHelper expected found = error ("expected `" <> expected <> "`, found `" <> found <> "`")

tests :: (HasCallStack) => TestTree
tests = askOption $ \(JtagDebug debug) ->
  testGroup
    "JTAG chaining"
    [ testCase "Basic GDB commands, breakpoints, and program loading" (testBoth debug)
    , testCase "Program loading with CPU A held in reset" (testInResetA debug)
    ]

main :: IO ()
main =
  defaultMainWithIngredients
    (includingOptions [Option (Proxy :: Proxy JtagDebug)] : defaultIngredients)
    tests
