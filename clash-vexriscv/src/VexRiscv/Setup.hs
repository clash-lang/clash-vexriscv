-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module VexRiscv.Setup (
  addVexRiscvHooks,
) where

import Prelude

import Control.Concurrent.Async (mapConcurrently_)
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Distribution.PackageDescription (
  BuildInfo (extraLibDirs, extraLibs),
  Library (libBuildInfo),
  PackageDescription (library),
 )
import Distribution.Simple (UserHooks (confHook))
import Distribution.Simple.LocalBuildInfo (
  Component (CLib),
  LocalBuildInfo (localPkgDescr),
  componentBuildDir,
  interpretSymbolicPathLBI,
 )
import Distribution.Simple.Setup (CommonSetupFlags (setupVerbosity), ConfigFlags (configCommonFlags), fromFlag)
import Distribution.Simple.Utils (info, notice)
import Distribution.Types.LocalBuildInfo (allTargetsInBuildOrder')
import Distribution.Types.TargetInfo (
  TargetInfo (targetCLBI, targetComponent),
 )
import Distribution.Utils.Path (makeSymbolicPath)
import Distribution.Verbosity (Verbosity)
import GHC.Stack (HasCallStack)
import System.Directory (
  createDirectoryIfMissing,
  makeAbsolute,
 )
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.Process (
  CreateProcess (cwd),
  proc,
  readCreateProcessWithExitCode,
 )

import qualified VexRiscv.Paths as Paths_clash_vexriscv

{- | Add VexRiscv build hooks to existing UserHooks.

This function modifies the confHook to build VexRiscv libraries and update
the package build configuration.

Example usage:

> main :: IO ()
> main = defaultMainWithHooks (addVexRiscvHooks simpleUserHooks "data" ["MyProject"])
-}
addVexRiscvHooks ::
  UserHooks ->
  -- | Configuration directory containing Scala files
  FilePath ->
  -- | List of CPU names to build
  [FilePath] ->
  UserHooks
addVexRiscvHooks hooks configDir cpuNames =
  hooks
    { confHook = \(genericPkgDesc, hookedBuildInfo) configFlags -> do
        lbi <- confHook hooks (genericPkgDesc, hookedBuildInfo) configFlags
        makeVexRiscvFromLocalBuildInfo lbi configFlags configDir cpuNames
    }

makeVexRiscvFromLocalBuildInfo ::
  (HasCallStack) =>
  LocalBuildInfo ->
  ConfigFlags ->
  FilePath ->
  [FilePath] ->
  IO LocalBuildInfo
makeVexRiscvFromLocalBuildInfo lbi configFlags configDir cpuNames = do
  let
    pkg = localPkgDescr lbi
    verbosity = fromFlag (setupVerbosity (configCommonFlags configFlags))

  -- Find the main library component
  case library pkg of
    Nothing -> error "No main library found in package"
    Just lib -> do
      let
        allTargets = allTargetsInBuildOrder' pkg lbi
        mainLibTarget = case [t | t <- allTargets, CLib lib' <- [targetComponent t], lib == lib'] of
          (t : _) -> t
          [] -> error "Could not find ComponentLocalBuildInfo for main library"
        clbi = targetCLBI mainLibTarget
      buildDir <- makeAbsolute (interpretSymbolicPathLBI lbi (componentBuildDir lbi clbi))
      let
        autogenDir = buildDir </> "autogen"
        libsDir = autogenDir </> "libs"

        -- XXX: Include static libraries only, as Cabal prefers dynamic libraries, but
        --      dynamic libraries break at runtime. REPL only works with dynamic libraries,
        --      but it seems that adding them in the preREPL hook will still make Cabal
        --      pick the static ones. I dunno man..
        staticLibsDirSymbolic = makeSymbolicPath (libsDir </> "static")

      mapConcurrently_ (makeVexRiscv verbosity autogenDir configDir) cpuNames

      return $
        updateLibBuildInfo lbi $
          \buildInfo ->
            buildInfo
              { extraLibDirs = staticLibsDirSymbolic : extraLibDirs buildInfo
              , extraLibs = map (++ "VexRiscvFFI") cpuNames ++ extraLibs buildInfo
              }

makeVexRiscv :: (HasCallStack) => Verbosity -> FilePath -> FilePath -> FilePath -> IO ()
makeVexRiscv verbosity autogenDir configDir cpuName = do
  let libraryDir = autogenDir </> "libs" </> cpuName ++ "VexRiscv"
  createDirectoryIfMissing True libraryDir
  dataDir <- Paths_clash_vexriscv.getDataDir
  let clashVexriscvDataDir = dataDir </> "data" </> "vexriscv"
  configDirAbs <- makeAbsolute configDir

  notice verbosity $ "Building VexRiscv_" ++ cpuName ++ ".."
  runCommand
    verbosity
    libraryDir
    "make"
    [ "-f"
    , clashVexriscvDataDir </> "Makefile"
    , "-C"
    , libraryDir
    , "SCALA_CONFIG_DIR=" ++ configDirAbs
    , "CPU_NAME=" ++ cpuName
    , "-j"
    ]

  verilogSource <- readFile (libraryDir </> cpuName ++ "VexRiscv.v")

  -- -- Generate Haskell module
  let haskellModuleSource = generateHaskellModule cpuName verilogSource
  writeFile (autogenDir </> "VexRiscv_" ++ cpuName ++ ".hs") haskellModuleSource

-- Helper to update library build info in package description
updateLibBuildInfo :: LocalBuildInfo -> (BuildInfo -> BuildInfo) -> LocalBuildInfo
updateLibBuildInfo lbi f =
  lbi
    { localPkgDescr =
        pkg
          { library =
              Just $
                lib
                  { libBuildInfo = f libBuild
                  }
          }
    }
 where
  pkg = localPkgDescr lbi
  lib = fromMaybe (error "updateLibBuildInfo: unexpected empty library in package description") (library pkg)
  libBuild = libBuildInfo lib

runCommand :: Verbosity -> FilePath -> String -> [String] -> IO ()
runCommand verbosity workDir cmd args = do
  info verbosity $ "Running: " ++ cmd ++ " " ++ unwords args
  (exitCode, stdout, stderr) <-
    readCreateProcessWithExitCode (proc cmd args){cwd = Just workDir} ""
  when (exitCode /= ExitSuccess) $ do
    error $
      "Command failed: "
        ++ cmd
        ++ "\nstdout: "
        ++ stdout
        ++ "\nstderr: "
        ++ stderr

generateHaskellModule :: String -> String -> String
generateHaskellModule cpuName verilogSource =
  [i|
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module VexRiscv_#{cpuName} (
  vexRiscv,
) where

import Prelude
import Clash.Prelude (Clock, KnownDomain, Signal, clashSimulation)
import Data.Word (Word64)
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr)
import GHC.Stack (HasCallStack)
import VexRiscv (CpuIn, CpuOut, DumpVcd, JtagIn, JtagOut)
import VexRiscv.Internal (vexRiscvSim, vexRiscvSynth, VexRiscv, VerilatedVcdC)
import VexRiscv.FFI (COMB_INPUT, NON_COMB_INPUT, OUTPUT)
import VexRiscv.Reset (MinCyclesReset)

-- FFI declarations for: V#{cpuName}VexRiscv
foreign import ccall unsafe "V#{cpuName}VexRiscv_init"
  c_init :: IO (Ptr VexRiscv)

foreign import ccall unsafe "V#{cpuName}VexRiscv_init_vcd"
  c_init_vcd :: Ptr VexRiscv -> CString -> IO (Ptr VerilatedVcdC)

-- TODO:
-- foreign import ccall unsafe "V#{cpuName}VexRiscv_shutdown"
--  c_shutdown :: Ptr VexRiscv -> IO ()

foreign import ccall unsafe "V#{cpuName}VexRiscv_init_stage1"
  c_init_stage1 :: Ptr VerilatedVcdC -> Word64 -> Ptr VexRiscv -> Ptr NON_COMB_INPUT -> Ptr OUTPUT -> IO ()

foreign import ccall unsafe "V#{cpuName}VexRiscv_init_stage2"
  c_init_stage2 :: Ptr VexRiscv -> Ptr COMB_INPUT -> IO ()

foreign import ccall unsafe "V#{cpuName}VexRiscv_step_rising_edge"
  c_step_rising :: Ptr VerilatedVcdC -> Ptr VexRiscv -> Word64 -> Ptr NON_COMB_INPUT -> Ptr OUTPUT -> IO ()

foreign import ccall unsafe "V#{cpuName}VexRiscv_step_falling_edge"
  c_step_falling :: Ptr VerilatedVcdC -> Ptr VexRiscv -> Word64 -> Ptr COMB_INPUT -> IO ()

-- | VexRiscv CPU simulation function
vexRiscv ::
  forall dom.
  (HasCallStack, KnownDomain dom) =>
  DumpVcd ->
  Clock dom ->
  MinCyclesReset dom 2 ->
  Signal dom CpuIn ->
  Signal dom JtagIn ->
  (Signal dom CpuOut, Signal dom JtagOut)
vexRiscv dumpVcd clk rst cpuIn jtagIn
  | clashSimulation = vexRiscvSim c_init c_init_vcd c_init_stage1 c_init_stage2 c_step_rising c_step_falling dumpVcd clk rst cpuIn jtagIn
  | otherwise = vexRiscvSynth "#{cpuName}VexRiscv" #{show verilogSource} clk rst cpuIn jtagIn
{-# INLINE vexRiscv #-}
|]
