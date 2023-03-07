// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#![no_std]
#![cfg_attr(not(test), no_main)]

use core::fmt::Write;

#[cfg(not(test))]
use riscv_rt::entry;

use bittide_sys::{elf_loading::validation::ElfConfig, print, println};

static PAYLOAD: &[u8] =
    include_bytes!("../../../target/riscv32imc-unknown-none-elf/release/loaded-program");

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    unsafe {
        bittide_sys::character_device::initialise(0x0000_1000 as *mut u8);
    }

    println!("The payload is {} bytes large.", PAYLOAD.len());

    let config = ElfConfig {
        instruction_memory_address: 0x6000_0000..(0x6000_0000 + (64 * 1024)),
        data_memory_address: 0x8000_0000..(0x8000_0000 + (16 * 1024)),
    };

    print!("Validating ELF...");

    let elf = match bittide_sys::elf_loading::validation::validate_elf_file(PAYLOAD, &config) {
        Ok(elf) => elf,
        Err(err) => {
            panic!("Error when validating ELF file! {err:?}")
        }
    };
    println!("done!");

    print!("Loading ELF into memory...");

    unsafe {
        bittide_sys::elf_loading::load_elf_file(&elf);
    }

    println!("done!");

    println!("Jumping to entry point");
    println!("----------------------");

    let entry_point = elf.entry_point();

    unsafe {
        core::arch::asm! {
            "jr {0}",
            in(reg) entry_point,
        }
    }

    loop {
        continue;
    }
}
