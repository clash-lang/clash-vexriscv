-- SPDX-FileCopyrightText: 2022 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

import Data.Maybe
import Distribution.PackageDescription hiding (Flag)
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import System.Directory
import System.FilePath.Glob (glob)
import System.FilePath ((</>))

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { preConf = makeExtLib,
        confHook = \a f -> confHook simpleUserHooks a f >>= updateExtraLibDirs,
        postBuild = \a f p l -> postBuild simpleUserHooks a f p l >> patchLibHSfile f l,
        copyHook = copyVexRiscvFfiLib
      }

copyVexRiscvFfiLib :: PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags -> IO ()
copyVexRiscvFfiLib pkgDescr lbi _ flags = do
  let
    libBuildDir0 = buildDir lbi
    libPref = libdir . absoluteInstallDirs pkgDescr lbi
              . fromFlag . copyDest
              $ flags

  libBuildDir <- makeAbsolute libBuildDir0

  libDirContents <- getDirectoryContents libBuildDir

  putStrLn "------------------"
  putStrLn $ "Copy target dir " <> libPref
  putStrLn $ "Prev target dir " <> libBuildDir
  putStrLn $ "  -> " <> show libDirContents
  putStrLn "------------------"

  createDirectoryIfMissing True libPref

  let
    tmpPath file = libBuildDir </> file
    finalPath file = libPref </> file
  
  copyFile (tmpPath "libVexRiscvFFI.so") (finalPath "libVexRiscvFFI.so")
  copyFile (tmpPath "libVexRiscvFFI.a") (finalPath "libVexRiscvFFI.a")
  copyFile (tmpPath "impl.o") (finalPath "impl.o")
  copyFile (tmpPath "verilated.o") (finalPath "verilated.o")
  copyFile (tmpPath "VVexRiscv__ALL.a") (finalPath "VVexRiscv__ALL.a")

makeExtLib :: Args -> ConfigFlags -> IO HookedBuildInfo
makeExtLib _ flags = do
  let verbosity = fromFlag $ configVerbosity flags
  rawSystemExit
    verbosity
    "env"
    ["make"]
  return emptyHookedBuildInfo

-- Patch the location of libVexRiscvFFI.so into the RUNPATH header of
-- libHSclash-vexriscv-[...].so file.
-- So ghc will know where to find it.
-- This is a bit of a hack, but we couldn't find a more direct way of accomplishing this.
patchLibHSfile :: BuildFlags -> LocalBuildInfo -> IO ()
patchLibHSfile flags localBuildInfo = do
  let verbosity = fromFlag $ buildVerbosity flags
  let packageDescription = localPkgDescr localBuildInfo
      lib = fromJust $ library packageDescription
      libBuild = libBuildInfo lib
      libBuildDir = buildDir localBuildInfo
  [soFile] <- glob (libBuildDir </> "libHSclash-vexriscv-*.so")
  rawSystemExit verbosity "patchelf" ["--add-rpath",libBuildDir,soFile]

updateExtraLibDirs :: LocalBuildInfo -> IO LocalBuildInfo
updateExtraLibDirs localBuildInfo = do
  let packageDescription = localPkgDescr localBuildInfo
      lib = fromJust $ library packageDescription
      libBuild = libBuildInfo lib
      libBuildDir = buildDir localBuildInfo
  putStrLn $ "clash-vexriscv build dir: " <> libBuildDir

  dir <- getCurrentDirectory

  let tmpPath file = dir </> "build_out_dir" </> file
      finalPath file = libBuildDir </> file
  
  copyFile (tmpPath "libVexRiscvFFI.so") (finalPath "libVexRiscvFFI.so")
  copyFile (tmpPath "libVexRiscvFFI.a") (finalPath "libVexRiscvFFI.a")
  copyFile (tmpPath "impl.o") (finalPath "impl.o")
  copyFile (tmpPath "verilated.o") (finalPath "verilated.o")
  copyFile (tmpPath "VVexRiscv__ALL.a") (finalPath "VVexRiscv__ALL.a")

  putStrLn $ "LIB DIRS!!!!! " <> show (extraLibDirs libBuild)

  return
    localBuildInfo
      { localPkgDescr =
          packageDescription
            { library =
                Just $
                  lib
                    { libBuildInfo =
                        libBuild
                          { extraLibDirs =
                              libBuildDir :
                              extraLibDirs libBuild
                          , extraLibs =
                              "VexRiscvFFI" : "stdc++" :
                              extraLibs libBuild
                          }
                    }
            }
      }
