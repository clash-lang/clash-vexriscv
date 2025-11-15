-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: CC0-1.0

module MyProject where

import Clash.Prelude
import VexRiscv (CpuIn, CpuOut, DumpVcd, JtagIn, JtagOut)
import VexRiscv.Reset (MinCyclesReset)

import qualified VexRiscv_MyProject

myCpu ::
  forall dom.
  (KnownDomain dom) =>
  DumpVcd ->
  Clock dom ->
  MinCyclesReset dom 2 ->
  Signal dom CpuIn ->
  Signal dom JtagIn ->
  ( Signal dom CpuOut
  , Signal dom JtagOut
  )
myCpu = VexRiscv_MyProject.vexRiscv
