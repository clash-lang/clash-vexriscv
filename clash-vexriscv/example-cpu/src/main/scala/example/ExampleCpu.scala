// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

package example

import spinal.core._
import spinal.lib._
import spinal.lib.com.jtag.Jtag
import spinal.lib.bus.wishbone.{Wishbone, WishboneConfig}

import vexriscv.plugin._
import vexriscv.{VexRiscv, VexRiscvConfig, plugin}
import vexriscv.ip.{DataCacheConfig}
import vexriscv.ip.fpu.FpuParameter

object ExampleCpu extends App {
  def cpu() : VexRiscv = {

    // val config = VexRiscvConfig(
    //     plugins = List(
    //         new IBusSimplePlugin(
    //             resetVector             = 0x20000000l,
    //             cmdForkOnSecondStage    = false,
    //             cmdForkPersistence      = false,
    //             prediction              = NONE,
    //             // Trap when an iBus access returns an error.
    //             catchAccessFault        = true,    
    //             compressedGen           = true
    //         ),
    //         new DBusSimplePlugin(
    //             // Trap when a load or store access is misaligned.
    //             catchAddressMisaligned  = true,     
    //             // Trap when a load or store access results in a bus error
    //             catchAccessFault        = true
    //         ),
    //         new CsrPlugin(
    //             // CsrPluginConfig(
    //             //     // Trap when accessing a priviledged CSR from non-priviledged mode. Or when
    //             //     // there are MRET or SRET instructions during non-priviledged mode. And other CSR
    //             //     // related errors.
    //             //     catchIllegalAccess  = true,
    //             //     mvendorid           = null,
    //             //     marchid             = null,
    //             //     mimpid              = null,
    //             //     mhartid             = null,
    //             //     misaExtensionsInit  = 66,
    //             //     misaAccess          = CsrAccess.NONE,
    //             //     mtvecAccess         = CsrAccess.NONE,
    //             //     mtvecInit           = 0x00000020,
    //             //     mepcAccess          = CsrAccess.READ_WRITE,
    //             //     mscratchGen         = false,
    //             //     mcauseAccess        = CsrAccess.READ_ONLY,
    //             //     mbadaddrAccess      = CsrAccess.READ_ONLY,    // == mtvalAccess
    //             //     mcycleAccess        = CsrAccess.NONE,
    //             //     minstretAccess      = CsrAccess.NONE,
    //             //     ecallGen            = false,
    //             //     ebreakGen           = true,
    //             //     wfiGenAsWait        = false,
    //             //     ucycleAccess        = CsrAccess.READ_ONLY,
    //             //     uinstretAccess      = CsrAccess.NONE
    //             // )
    //             CsrPluginConfig.smallest.copy(ebreakGen = true, mtvecAccess = CsrAccess.READ_WRITE)
// 
    //         ),
    //         new DecoderSimplePlugin(
    //             // Trap when we execute an illegal instruction. For example, a MUL instruction when
    //             // the CPU hasn't been configured with HW multiplier support.
    //             catchIllegalInstruction = true
    //         ),
    //         new RegFilePlugin(
    //             regFileReadyKind        = plugin.SYNC,
    //             zeroBoot                = false
    //         ),
    //         new IntAluPlugin,
    //         new SrcPlugin(
    //             separatedAddSub         = false,
    //             executeInsertion        = false
    //         ),
    //         new FullBarrelShifterPlugin,
    //         new HazardSimplePlugin(
    //           bypassExecute           = false,
    //           bypassMemory            = false,
    //           bypassWriteBack         = false,
    //           bypassWriteBackBuffer   = false,
    //           pessimisticUseSrc       = false,
    //           pessimisticWriteRegFile = false,
    //           pessimisticAddressMatch = false
    //         ),
    //         // new HazardSimplePlugin(
    //         //     bypassExecute           = true,
    //         //     bypassMemory            = true,
    //         //     bypassWriteBack         = true,
    //         //     bypassWriteBackBuffer   = true,
    //         //     pessimisticUseSrc       = false,
    //         //     pessimisticWriteRegFile = false,
    //         //     pessimisticAddressMatch = false
    //         // ),
    //         new BranchPlugin(
    //             earlyBranch             = false,
    //             catchAddressMisaligned  = true
    //         ),
    //         new DebugPlugin(ClockDomain.current.clone(reset = Bool().setName("debugReset"))),
    //         new YamlPlugin("VexRiscvWithDebug.yaml")
    //     )
    // )

    val config = VexRiscvConfig(
          plugins = List(
            new IBusSimplePlugin(
                resetVector             = 0x20000000l,
                cmdForkOnSecondStage    = false,
                cmdForkPersistence      = false,
                prediction              = STATIC,
                // Trap when an iBus access returns an error.
                catchAccessFault        = true,    
                compressedGen           = true
            ),
            new DBusSimplePlugin(
                // Trap when a load or store access is misaligned.
                catchAddressMisaligned  = true,     
                // Trap when a load or store access results in a bus error
                catchAccessFault        = true
            ),

            new CsrPlugin(
              // CsrPluginConfig.smallest.copy(ebreakGen = true, mtvecAccess = CsrAccess.READ_WRITE)
              CsrPluginConfig(
                catchIllegalAccess = false,
                mvendorid      = null,
                marchid        = null,
                mimpid         = null,
                mhartid        = null,
                misaExtensionsInit = 66,
                misaAccess     = CsrAccess.NONE,
                mtvecAccess    = CsrAccess.READ_WRITE,
                mtvecInit      = 0x00000020,
                mepcAccess     = CsrAccess.NONE,
                mscratchGen    = false,
                mcauseAccess   = CsrAccess.READ_ONLY,
                mbadaddrAccess = CsrAccess.READ_ONLY,
                mcycleAccess   = CsrAccess.NONE,
                minstretAccess = CsrAccess.NONE,
                ecallGen       = false,
                ebreakGen      = true,
                wfiGenAsWait   = false,
                ucycleAccess   = CsrAccess.READ_ONLY,
                uinstretAccess = CsrAccess.NONE
              )
            ),
            new DecoderSimplePlugin(
              catchIllegalInstruction = true
            ),
            new RegFilePlugin(
              regFileReadyKind = plugin.SYNC,
              zeroBoot = false
            ),
            new IntAluPlugin,
            new SrcPlugin(
              separatedAddSub = false,
              executeInsertion = false
            ),

            // M extension
            new MulPlugin,
            new DivPlugin,

            new LightShifterPlugin,
            new HazardSimplePlugin(
              bypassExecute           = false,
              bypassMemory            = false,
              bypassWriteBack         = false,
              bypassWriteBackBuffer   = false,
              pessimisticUseSrc       = false,
              pessimisticWriteRegFile = false,
              pessimisticAddressMatch = false
            ),
            new BranchPlugin(
              earlyBranch = false,
              catchAddressMisaligned = true
            ),
            new DebugPlugin(ClockDomain.current),
            new YamlPlugin("ExampleCpu.yaml")
        )

    )

    val cpu = new VexRiscv(config)

    cpu.rework {
      for (plugin <- config.plugins) plugin match {
        case plugin: IBusSimplePlugin => {
          plugin.iBus.setAsDirectionLess() //Unset IO properties of iBus
          master(plugin.iBus.toWishbone()).setName("iBusWishbone")
        }
        case plugin: DBusSimplePlugin => {
          plugin.dBus.setAsDirectionLess()
          master(plugin.dBus.toWishbone()).setName("dBusWishbone")
        }

        case plugin: DBusCachedPlugin => {
          plugin.dBus.setAsDirectionLess()
          master(plugin.dBus.toWishbone()).setName("dBusWishbone")
        }
        
        case plugin: DebugPlugin => plugin.debugClockDomain {
          plugin.io.bus.setAsDirectionLess() 
          val jtag = slave(new Jtag()).setName("jtag")
          jtag <> plugin.io.bus.fromJtag()
        }
        case _ =>
      }
    }

    return cpu
  }

  SpinalVerilog(cpu())
}