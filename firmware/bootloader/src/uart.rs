pub struct Uart {
    payload_addr: *mut u32,
    flags_addr: *const u32,
}

impl Uart {
    pub const unsafe fn new(base_addr: *mut u8) -> Uart {
        Uart {
            payload_addr: base_addr.cast(),
            flags_addr: base_addr.cast::<u32>().cast_const().add(1),
        }
    }

    pub fn receive(&mut self) -> u8 {
        loop {
            if let Some(val) = self.try_receive() {
                return val;
            }
        }
    }

    pub fn try_receive(&mut self) -> Option<u8> {
        if self.has_rec_data() {
            unsafe {
                let data_dword = self.payload_addr.read_volatile();
                let [_, _, _, data] = data_dword.to_le_bytes();
                Some(data)
            }
        } else {
            None
        }
    }

    pub fn send(&mut self, data: u8) {
        loop {
            if let Ok(()) = self.try_send(data) {
                return;
            }
        }
    }

    pub fn try_send(&mut self, data: u8) -> Result<(), ()> {
        if self.can_send() {
            unsafe {
                self.payload_addr
                    .write_volatile(u32::from_le_bytes([data, data, data, data]));
                Ok(())
            }
        } else {
            Err(())
        }
    }

    pub fn has_rec_data(&mut self) -> bool {
        unsafe {
            let flags = self.flags_addr.read_volatile();
            let [_tx_full, _tx_empty, _rx_full, rx_empty] = flags.to_le_bytes();
            rx_empty != 0
        }
    }

    pub fn can_send(&mut self) -> bool {
        unsafe {
            let flags = self.flags_addr.read_volatile();
            let [tx_full, _tx_empty, _rx_full, _rx_empty] = flags.to_le_bytes();
            tx_full != 0
        }
    }
}
