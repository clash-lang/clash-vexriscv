-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

import Distribution.Simple
import VexRiscv.Setup (addVexRiscvHooks)

main :: IO ()
main = defaultMainWithHooks (addVexRiscvHooks simpleUserHooks "data" ["Example", "Example2"])
