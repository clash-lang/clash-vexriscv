// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

use elf::{endian::LittleEndian, file::Class, ElfBytes};

pub struct ValidatedElfFile<'a>(ElfBytes<'a, LittleEndian>);

impl core::fmt::Debug for ValidatedElfFile<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_tuple("ValidatedElfFile(..)}").finish()
    }
}

impl<'a> ValidatedElfFile<'a> {
    pub(crate) fn elf_file(&self) -> &ElfBytes<'a, LittleEndian> {
        &self.0
    }

    /// Retrieve the entry point of a validated ELF file.
    pub fn entry_point(&self) -> usize {
        usize::try_from(self.0.ehdr.e_entry).unwrap()
    }
}

#[derive(Debug)]
pub struct ElfConfig {
    pub instruction_memory_address: core::ops::Range<usize>,
    pub data_memory_address: core::ops::Range<usize>,
}

#[derive(Debug)]
pub enum ElfValidationError {
    MachineNotRiscv,
    ElfParsingError(elf::ParseError),
    Elf64Found,
    UnexpectedEndianess,
    UnexpectedElfType {
        found: &'static str,
    },
    NoSegmentsFound,
    SegmentOutOfRange {
        expected: core::ops::Range<usize>,
        found: core::ops::Range<usize>,
        segment_type: &'static str,
    },
    SegmentAddrDoesNotFitUsize {
        addr: u64,
    },
    SegmentMemSizeDoesNotFitUsize {
        size: u64,
    },
    EntryPointDoesNotFitUsize {
        entry: u64,
    },
    EntryPointNotLoaded {
        entry_point: usize,
    },
}

pub fn validate_elf_file<'a>(
    elf_bin: &'a [u8],
    config: &ElfConfig,
) -> Result<ValidatedElfFile<'a>, ElfValidationError> {
    let elf = match ElfBytes::minimal_parse(elf_bin) {
        Ok(elf) if elf.ehdr.class == Class::ELF32 => elf,
        Ok(_) => return Err(ElfValidationError::Elf64Found),
        Err(elf::ParseError::UnsupportedElfEndianness(_)) => {
            return Err(ElfValidationError::UnexpectedEndianess)
        }
        Err(err) => return Err(ElfValidationError::ElfParsingError(err)),
    };

    if elf.ehdr.e_machine != elf::abi::EM_RISCV {
        return Err(ElfValidationError::MachineNotRiscv);
    }

    if elf.ehdr.e_type != elf::abi::ET_EXEC {
        let ty = match elf.ehdr.e_type {
            elf::abi::ET_CORE => "Core",
            elf::abi::ET_DYN => "Dynamic",
            elf::abi::ET_HIOS => "OS-specific (HI)",
            elf::abi::ET_HIPROC => "Processor-specific (HI)",
            elf::abi::ET_LOOS => "OS-specific (LO)",
            elf::abi::ET_LOPROC => "Processor-specific (LO)",
            elf::abi::ET_NONE => "None",
            elf::abi::ET_REL => "Relocatable",
            _ => "unsupported ELF type",
        };

        return Err(ElfValidationError::UnexpectedElfType { found: ty });
    }

    let Ok(entry_point) = usize::try_from(elf.ehdr.e_entry) else {
        return Err(ElfValidationError::EntryPointDoesNotFitUsize {
            entry: elf.ehdr.e_entry,
        })
    };
    let mut entry_point_loaded = false;

    let Some(seg_iter) = elf.segments() else {
        return Err(ElfValidationError::NoSegmentsFound)
    };

    for p in seg_iter {
        if p.p_type != elf::abi::PT_LOAD {
            continue;
        }

        let Ok(addr) = usize::try_from(p.p_paddr) else {
            return Err(ElfValidationError::SegmentAddrDoesNotFitUsize { addr: p.p_paddr })
        };
        let Ok(size) = usize::try_from(p.p_memsz) else {
            return Err(ElfValidationError::SegmentMemSizeDoesNotFitUsize { size: p.p_memsz })
        };
        let found_range = addr..(addr + size);

        if p.p_flags == elf::abi::PF_X | elf::abi::PF_R {
            // instruction memory

            if found_range.contains(&entry_point) {
                entry_point_loaded = true;
            }

            if found_range.is_empty()
                || config.instruction_memory_address.is_empty()
                || config.instruction_memory_address.contains(&addr)
            {
                continue;
            } else {
                return Err(ElfValidationError::SegmentOutOfRange {
                    expected: config.instruction_memory_address.clone(),
                    found: found_range,
                    segment_type: "instruction memory",
                });
            }
        } else if p.p_flags == elf::abi::PF_R || p.p_flags == elf::abi::PF_W | elf::abi::PF_R {
            // data memory

            if found_range.is_empty()
                || config.data_memory_address.is_empty()
                || config.data_memory_address.contains(&addr)
            {
                continue;
            } else {
                return Err(ElfValidationError::SegmentOutOfRange {
                    expected: config.data_memory_address.clone(),
                    found: found_range,
                    segment_type: "data memory",
                });
            }
        }
    }

    if !entry_point_loaded {
        return Err(ElfValidationError::EntryPointNotLoaded { entry_point });
    }

    Ok(ValidatedElfFile(elf))
}
