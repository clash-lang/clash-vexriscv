<!--
SPDX-FileCopyrightText: 2022 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# clash-vexriscv
This package provides a VexRiscv CPU core for use in Clash designs.

## Dependencies
For building the CPU, the following software needs to be installed and available
in the `PATH`:

- a recent JDK installation
- SBT, the scala build tool
- a recent C compiler
- `verilator`
- `make` for building the verilated library and FFI code

## Limitations
 * The core will generate as part of your project's setup. This means that configuration cannot be based on parts of your design. E.g., whether or not to activate the floating point extension on the CPU will have to be decided before integration in the project.
 * The package is currently hardcoded to expect a Wishbone and JTAG bus.
 * The setup procedure currently compiles the Verilated Verilog itself, bypassing Cabal. This means Cabal compiler flags have no effect.

## Integration
This package does not give you a VexRiscv core directly, but offers the necessary
tools to generate one as part of a Cabal package. You will have to do two things:

  * Provide a (Scala-based) configuration file for the VexRiscv
  * Integrate `clash-vexriscv` with your Cabal project

### Example project
A complete example project is provided in the [`../clash-vexriscv-example/`](../clash-vexriscv-example/) directory. This example demonstrates:

- A minimal Cabal project setup for using `clash-vexriscv`
- A VexRiscv CPU configuration file (`data/MyProject.scala`) with:
  - Basic RiscV ISA components (IBus, DBus, CSR, decoder, register file, ALU)
  - Simple hazard handling
  - Branch prediction
  - JTAG debug interface
  - Wishbone memory buses for instruction and data access

You can use this as a template for your own integration project.

### CPU configuration
VexRiscv is a very configurable CPU as shown by a great number of [demo configurations](https://github.com/SpinalHDL/VexRiscv/tree/master/src/main/scala/vexriscv/demo). This package is currently hardcoded to need two things:

 * A Wishbone interface
 * A debug (JTAG) interface

It also expects SpinalHDL to generate all Verilog names with a certain prefix. This is all handled by this [example configuration](../clash-vexriscv-example/data/MyProject.scala). Place the configuration in a folder, e.g. `data` and give it a recognizable name. In our example, this is `MyProject`. If you change the filename, don't forget to change the object name in the configuration file too.

### Cabal integration
In the root of your package, add `Setup.hs`:

```haskell
import Distribution.Simple
import VexRiscv.Setup (addVexRiscvHooks)

main :: IO ()
main = defaultMainWithHooks (addVexRiscvHooks simpleUserHooks "data" ["MyProject"])
```

Then, in your Cabal file, set the build type to `Custom`, add your configuration, and add a `custom-setup`:

```cabal
build-type: Custom

extra-source-files:
  data/*.Scala

custom-setup
  setup-depends:
    base,
    Cabal,
    clash-vexriscv
```

In your `library` section, add the following:

```cabal
  other-modules:
    VexRiscv_MyProject
  autogen-modules:
    VexRiscv_MyProject
```

If you picked another name than `MyProject`, replace that part of the module name. The module will export a single function:

```haskell
vexRiscv ::
  forall dom.
  (HasCallStack, KnownDomain dom) =>
  DumpVcd ->
  Clock dom ->
  MinCyclesReset dom 2 ->
  Signal dom CpuIn ->
  Signal dom JtagIn ->
  ( Signal dom CpuOut
  , Signal dom JtagOut
  )
```

Good luck!

## Notes for using the core:

- The contents of memories need to be stored in little endian. This means that
  for example the contents of an ELF file need to be endian-swapped before being
  used as the contents of the instruction storage.
  This applies to all storages.
