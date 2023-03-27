// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#![no_std]
#![cfg_attr(not(test), no_main)]

use core::fmt::Write;

#[cfg(not(test))]
use riscv_rt::entry;

pub mod uart;

#[panic_handler]
fn panic_handler(_: &core::panic::PanicInfo) -> ! {
    loop {}
}

use uart::Uart;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    let mut uart = unsafe { Uart::new(0x0100_0000 as *mut u8) };

    let _ = writeln!(uart, "hello, world from the payload program.");

    loop {
        let _ = writeln!(uart, "Doing some work....");
    }
}
