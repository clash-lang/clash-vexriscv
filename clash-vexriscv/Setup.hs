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
import System.FilePath ((</>))

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { preConf = makeExtLib
      , confHook = \a f -> confHook simpleUserHooks a f >>= updateExtraLibDirs
      , copyHook = \desc lbi h f -> copyHook simpleUserHooks desc lbi h f >> copyVexRiscvFfiLib desc lbi f
      }

makeExtLib :: Args -> ConfigFlags -> IO HookedBuildInfo
makeExtLib _ flags = do
  let verbosity = fromFlag $ configVerbosity flags
  rawSystemExit
    verbosity
    "env"
    ["make"]
  return emptyHookedBuildInfo

updateExtraLibDirs :: LocalBuildInfo -> IO LocalBuildInfo
updateExtraLibDirs localBuildInfo = do
  let packageDescription = localPkgDescr localBuildInfo
      lib = fromJust $ library packageDescription
      libBuild = libBuildInfo lib
      libBuildDir = buildDir localBuildInfo

  dir <- getCurrentDirectory

  let tmpPath file = dir </> "build_out_dir" </> file
      finalPath file = libBuildDir </> file
  
  -- when using `extra-bundled-libraries`, cabal seems to get confused
  -- and needs multiple versions of the library at different points
  -- during compilation
  copyFile (tmpPath "libVexRiscvFFI.so") (finalPath "libVexRiscvFFI.so")
  copyFile (tmpPath "libVexRiscvFFI.so") (finalPath "libCVexRiscvFFI.so")
  copyFile (tmpPath "libVexRiscvFFI.so") (finalPath "libCVexRiscvFFI-ghc9.0.2.so")
  
  copyFile (tmpPath "libVexRiscvFFI.a") (finalPath "libCVexRiscvFFI.a")

  pure localBuildInfo


copyVexRiscvFfiLib :: PackageDescription -> LocalBuildInfo -> CopyFlags -> IO ()
copyVexRiscvFfiLib pkgDescr lbi flags = do
  let
    libBuildDir0 = buildDir lbi
    libPref = libdir . absoluteInstallDirs pkgDescr lbi
              . fromFlag . copyDest
              $ flags

  libBuildDir <- makeAbsolute libBuildDir0

  createDirectoryIfMissing True libPref

  let
    tmpPath file = libBuildDir </> file
    finalPath file = libPref </> file
  
  -- when using `extra-bundled-libraries`, cabal seems to get confused
  -- and needs multiple versions of the library at different points
  -- during compilation
  copyFile (tmpPath "libVexRiscvFFI.so") (finalPath "libVexRiscvFFI.so")
  copyFile (tmpPath "libCVexRiscvFFI.so") (finalPath "libCVexRiscvFFI.so")
  copyFile (tmpPath "libCVexRiscvFFI-ghc9.0.2.so") (finalPath "libCVexRiscvFFI-ghc9.0.2.so")
  copyFile (tmpPath "libCVexRiscvFFI.a") (finalPath "libCVexRiscvFFI.a")