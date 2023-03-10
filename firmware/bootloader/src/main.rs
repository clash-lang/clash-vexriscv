// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#![no_std]
#![cfg_attr(not(test), no_main)]

pub mod uart;

use core::fmt::Write;

#[cfg(not(test))]
use riscv_rt::entry;

use bittide_sys::{elf_loading::validation::ElfConfig, print, println};
use uart::Uart;

/*
static STATIC_PAYLOAD: &[u8] =
    include_bytes!("../../../target/riscv32imc-unknown-none-elf/release/payload-program");
*/

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    unsafe {
        bittide_sys::character_device::initialise(0x0000_1000 as *mut u8);
    }

    // static payload
    // let data = &STATIC_PAYLOAD[..];

    // UART payload
    let buffer = {
        static mut UART_PAYLOAD: [u8; 1024 * 16] = [0; 1024 * 16];
        unsafe { &mut UART_PAYLOAD[..] }
    };
    let Some(data) = (unsafe { uart_payload(buffer) }) else {
        panic!("invalid UART load")
    };

    println!("The payload is {} bytes large.", data.len());

    let config = ElfConfig {
        instruction_memory_address: 0x6000_0000..(0x6000_0000 + (64 * 1024)),
        data_memory_address: 0x8000_0000..(0x8000_0000 + (16 * 1024)),
    };

    print!("Validating ELF...");

    let elf = match bittide_sys::elf_loading::validation::validate_elf_file(data, &config) {
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

const UART_ADDR: *mut u8 = 0x0000_2000 as *mut u8;

unsafe fn uart_payload(buffer: &mut [u8]) -> Option<&[u8]> {
    let mut uart = unsafe { Uart::new(UART_ADDR) };

    fn expect_uart_read(uart: &mut Uart, s: &str) -> Option<()> {
        for b in s.bytes() {
            if uart.receive() != b {
                return None;
            }
        }
        Some(())
    }

    expect_uart_read(&mut uart, "size")?;

    let size = u32::from_le_bytes([
        uart.receive(),
        uart.receive(),
        uart.receive(),
        uart.receive(),
    ]);

    if size > buffer.len() as u32 {
        panic!(
            "UART data does not fit into buffer! {size} {}",
            buffer.len()
        );
    }

    expect_uart_read(&mut uart, "payload")?;

    for i in 0..size {
        buffer[i as usize] = uart.receive();
    }

    expect_uart_read(&mut uart, "end")?;

    Some(&buffer[0..size as usize])
}
