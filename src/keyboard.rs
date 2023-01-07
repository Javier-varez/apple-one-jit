use std::mem::MaybeUninit;

use libc::termios;
use libc::STDIN_FILENO;
use libc::{tcgetattr, tcsetattr};

use std::io::stdin;
use std::io::Read;

pub struct Keyboard {
    recv: std::sync::mpsc::Receiver<u8>,
}

impl Keyboard {
    pub fn new() -> Self {
        let mut termios: MaybeUninit<termios> = MaybeUninit::uninit();
        unsafe {
            tcgetattr(STDIN_FILENO, termios.as_mut_ptr());
        }

        let mut termios = unsafe { termios.assume_init() };
        termios.c_iflag |= libc::ICRNL;
        termios.c_lflag &= !(libc::ECHO | libc::ICANON);

        unsafe {
            tcsetattr(STDIN_FILENO, libc::TCSAFLUSH, &termios);
        }

        let (sender, receiver) = std::sync::mpsc::channel();
        std::thread::spawn(move || loop {
            const BACKSPACE_INPUT_CODE: u8 = 127;
            const BACKSPACE_ASCII: u8 = 0x08;
            let mut buf = [0];
            stdin().read_exact(&mut buf).unwrap();
            let value = match buf[0] {
                BACKSPACE_INPUT_CODE => BACKSPACE_ASCII,
                value => value,
            };
            sender.send(value).unwrap();
        });

        Self { recv: receiver }
    }

    pub fn get_character(&mut self) -> Option<char> {
        let byte = self.recv.recv_timeout(std::time::Duration::from_nanos(0));
        byte.ok()
            .map(|c| if c == '\n' as u8 { '\r' } else { c as char })
    }
}
