-- SPDX-FileCopyrightText: 2023 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

import Clash.Prelude
import Clash.Annotations.TH

import VexRiscv

circuit ::
  "CLK" ::: Clock System ->
  "RST" ::: Reset System ->
  "CPU_COMB_INPUT" ::: Signal System CpuIn ->
  "JTAG_IN_" ::: Signal System JtagIn ->
  "" :::
    ( "CPU_OUTPUT" ::: Signal System CpuOut
    , "JTAG_OUT_" ::: Signal System JtagOut)
circuit clk rst input jtagIn =
  vexRiscv clk rst input jtagIn

makeTopEntity 'circuit

main :: IO ()
main = pure ()
