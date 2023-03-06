// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#![no_std]
#![cfg_attr(not(test), no_main)]

use core::fmt::Write;

#[cfg(not(test))]
use riscv_rt::entry;

use bittide_sys::println;

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    unsafe {
        bittide_sys::character_device::initialise(0x0000_1000 as *mut u8);
    }

    println!("hello, world from the payload program.");

    loop {
        for i in 0..100 {
            println!("Doing some work.... {i}");
        }
    }
}
