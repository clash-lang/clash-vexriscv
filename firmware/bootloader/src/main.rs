// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#![no_std]
#![cfg_attr(not(test), no_main)]

pub mod uart;

use core::fmt::Write;

use bittide_sys::elf_loading::validation::ElfConfig;
use md5::Digest;
#[cfg(not(test))]
use riscv_rt::entry;

use uart::Uart;

const STATIC_PAYLOAD: &[u8] =
    include_bytes!("../../../target/riscv32imc-unknown-none-elf/release/payload-program");

#[cfg_attr(not(test), entry)]
fn main() -> ! {
    unsafe {
        uart::set_panic_handler_uart(Uart::new(0x0100_0000 as *mut u8));
    }

    let mut term_uart = unsafe { Uart::new(0x0100_0000 as *mut u8) };

    let mut boot_uart = unsafe { Uart::new(0x1000_0000 as *mut u8) };

    writeln!(term_uart, "Hello from the bootloader!!!").unwrap();

    // static payload
    // let data = &STATIC_PAYLOAD[..];

    // UART payload
    let buffer = {
        static mut UART_PAYLOAD: [u8; 1024 * 16] = [0; 1024 * 16];
        unsafe { &mut UART_PAYLOAD[..] }
    };
    let data = match uart_payload(&mut term_uart, &mut boot_uart, buffer) {
        Ok(data) => data,
        Err(UartLoadError::ReceiveError(err)) => {
            writeln!(term_uart, "Error receiving payload: {err:?}").unwrap();
            panic!()
        }
        Err(UartLoadError::Md5Mismatch) => {
            writeln!(term_uart, "MD5 mismatch").unwrap();
            panic!()
        }
        Err(UartLoadError::PayloadTooLarge { payload_size }) => {
            writeln!(term_uart, "Payload too large for buffer: {payload_size}").unwrap();
            panic!()
        }
    };

    writeln!(term_uart, "The payload is {} bytes large.", data.len()).unwrap();

    let config = ElfConfig {
        instruction_memory_address: 0x6000_0000..(0x6000_0000 + (64 * 1024)),
        data_memory_address: 0x8000_0000..(0x8000_0000 + (16 * 1024)),
    };

    write!(term_uart, "Validating ELF...").unwrap();

    let elf = match bittide_sys::elf_loading::validation::validate_elf_file(data, &config) {
        Ok(elf) => elf,
        Err(err) => {
            panic!("Error when validating ELF file! {err:?}")
        }
    };
    writeln!(term_uart, "done!").unwrap();

    write!(term_uart, "Loading ELF into memory...").unwrap();

    unsafe {
        bittide_sys::elf_loading::load_elf_file(&elf);
    }

    writeln!(term_uart, "done!").unwrap();

    writeln!(term_uart, "Jumping to entry point").unwrap();
    writeln!(term_uart, "----------------------").unwrap();

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

enum UartLoadError {
    ReceiveError(uart::ReceiveError),
    PayloadTooLarge { payload_size: u32 },
    Md5Mismatch,
}

impl From<uart::ReceiveError> for UartLoadError {
    fn from(v: uart::ReceiveError) -> Self {
        Self::ReceiveError(v)
    }
}

const UART_RECV_TIMEOUT: u32 = 1000;

fn uart_payload<'b>(
    term_uart: &mut Uart,
    uart: &mut Uart,
    buffer: &'b mut [u8],
) -> Result<&'b [u8], UartLoadError> {
    const CHUNK_SIZE: u32 = 128;

    let mut rec = {
        let mut i = 0;
        move || {
            if (i % CHUNK_SIZE) == 0 {
                uart.send(1);
            }
            let b = uart.receive_timeout(UART_RECV_TIMEOUT);

            if b.is_err() {
                panic!("Timeout after transmitting byte number {i} {i:X?}")
            }
            i += 1;
            b
        }
    };

    let size = u32::from_le_bytes([rec()?, rec()?, rec()?, rec()?]);
    writeln!(term_uart, "[UART] size: {size:X?} {size}").unwrap();

    let mut md5_expected = [0u8; 16];
    for b in &mut md5_expected {
        *b = rec()?;
    }

    writeln!(term_uart, "[UART] expected md5 {md5_expected:?}").unwrap();

    if size > buffer.len() as u32 {
        writeln!(
            term_uart,
            "UART data does not fit into buffer! {size} {}",
            buffer.len()
        )
        .unwrap();
        return Err(UartLoadError::PayloadTooLarge { payload_size: size });
    }

    let chunks = size / CHUNK_SIZE;

    writeln!(term_uart, "[UART] receiving data...").unwrap();

    /*
    write!(term_uart, "[UART] (").unwrap();
    for _ in 0..=chunks {
        term_uart.send(b'-');
    }
    writeln!(term_uart, ")").unwrap();
    */

    write!(term_uart, "[UART] (").unwrap();

    for i in 0..size {
        if (i % CHUNK_SIZE) == 0 {
            term_uart.send(b'.');
        }
        let b = rec()?;
        let ref_b = STATIC_PAYLOAD[i as usize];
        if b != ref_b {
            writeln!(
                term_uart,
                "\nmismatch at {i} {i:X?}, got {b:X?} expected {ref_b:X?}."
            );
            let start_idx = i.saturating_sub(1);
            writeln!(
                term_uart,
                "Reference, starting at {i}(-1), {:x?}",
                &STATIC_PAYLOAD[start_idx as usize..(i as usize + 10)]
            );
            panic!()
        }
        buffer[i as usize] = b;
    }
    writeln!(term_uart, ")").unwrap();

    write!(term_uart, "[UART] checking MD5...").unwrap();

    let data = &buffer[0..size as usize];

    let mut hasher = md5::Md5::new();
    hasher.update(data);

    let md5_result_arr = hasher.finalize();
    let md5_result = &md5_result_arr[..];

    if md5_result != md5_expected {
        writeln!(term_uart, " mismatch!").unwrap();
        writeln!(term_uart, "[UART] MD5 expected: {md5_expected:?}").unwrap();
        writeln!(term_uart, "[UART] MD5 found:    {md5_result:?}").unwrap();
        return Err(UartLoadError::Md5Mismatch);
    } else {
        writeln!(term_uart, " matching!").unwrap();
    }

    writeln!(term_uart, "[UART] done").unwrap();
    Ok(&buffer[0..size as usize])
}
