// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

pub mod validation;

use validation::ValidatedElfFile;

/// Load a validated ELF file into memory
///
/// # Safety
///
/// The segments of the ELF file will be written into their specified physical
/// addresses. Calling this function is only safe if those addresses:
///
/// - point to valid memory
/// - point to memory that is not used by the current program
pub unsafe fn load_elf_file(valid_elf: &ValidatedElfFile<'_>) {
    let elf = valid_elf.elf_file();

    for p in elf.segments().unwrap() {
        // skip segments that don't need loading
        if p.p_type != elf::abi::PT_LOAD {
            continue;
        }

        let paddr = usize::try_from(p.p_paddr).unwrap();
        let data = elf
            .segment_data(&p)
            .expect("ELF file contains invalid data indices");

        let addr_ptr = paddr as *mut u8;

        // write the segment to its desired memory location
        core::ptr::copy_nonoverlapping(data.as_ptr(), addr_ptr, data.len());

        // the size in memory can be bigger than the data provided in the ELF file,
        // the remaining bytes should be filled with zeroes.
        let zero_padding = (p.p_memsz as usize).saturating_sub(data.len());
        if zero_padding > 0 {
            // SAFETY: - `addr_ptr.add(data.len())` is valid since it points at
            //           most one byte after a valid allocation
            //         - the call to `write_bytes` is valid as the memory/buffer
            //           size was validated to be big enough to hold the full
            //           data + padding.
            core::ptr::write_bytes(addr_ptr.add(data.len()), 0, zero_padding);
        }
    }
}
