// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#![no_std]
#![cfg_attr(not(test), no_main)]

use core::fmt::Write;

use elf::{abi, endian::AnyEndian, ElfBytes};
#[cfg(not(test))]
use riscv_rt::entry;

use bittide_sys::{print, println};

static PAYLOAD: &[u8] =
    include_bytes!("../../../target/riscv32imc-unknown-none-elf/release/loaded-program");

const PAYLOAD_INSTR_ADDR: usize = 0x6000_0000;
const PAYLOAD_DATA_ADDR: usize = 0x8000_0000;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    unsafe {
        bittide_sys::character_device::initialise(0x0000_1000 as *mut u8);
    }

    println!("The payload is {} bytes large.", PAYLOAD.len());

    println!("Reading ELF");

    let file = ElfBytes::<AnyEndian>::minimal_parse(PAYLOAD).expect("Payload ELF is not valid");

    println!("Done reading ELF");

    let segs = file.segments().expect("ELF has no segments");

    println!("Done reading segs");

    for seg in segs {
        if seg.p_type != abi::PT_LOAD {
            // doesn't need loading, skip.
            continue;
        }

        if seg.p_paddr as usize == PAYLOAD_INSTR_ADDR {
            if (seg.p_flags & abi::PF_X) == 0 {
                panic!(
                    "Segment at instruction memory address is not marked as executable: {:X?}",
                    seg.p_flags
                );
            }
        }

        if seg.p_paddr as usize == PAYLOAD_DATA_ADDR {
            if (seg.p_flags & abi::PF_R) == 0 {
                panic!(
                    "Segment at data memory address is not marked as readable: {:X?}",
                    seg.p_flags
                );
            }
        }

        let seg_data = file
            .segment_data(&seg)
            .expect("Could not read segment data");

        let padding_addr = seg.p_paddr as usize + seg_data.len();
        let padding_len = seg.p_memsz as usize - seg_data.len();

        println!("Segment found!");
        println!("  Load at   {:X?}", seg.p_paddr);
        println!("  File-size {:X?}", seg_data.len());
        println!("  Mem-Size  {:X?}", seg.p_memsz);
        println!("  Padding   {padding_len:X?}");
        println!();

        print!("  Loading data into address...");

        unsafe {
            core::ptr::copy_nonoverlapping(
                seg_data.as_ptr(),
                seg.p_paddr as *mut u8,
                seg_data.len(),
            );
        }

        println!("done!");

        if padding_len > 0 {
            print!("  Writing any needed padding...");
            unsafe {
                core::ptr::write_bytes(padding_addr as *mut u8, 0, padding_len);
            }

            println!("done!");
        }
    }

    println!("Jumping to 0x6000_0000");
    println!("----------------------");

    unsafe {
        core::arch::asm! {
            "li t0, 0x60000000",
            "jr t0"
        }
    }

    loop {
        continue;
    }
}
