use nix::libc::STDIN_FILENO;
use nix::sys::termios::InputFlags;
use nix::sys::termios::LocalFlags;
use nix::sys::termios::SetArg;

use std::io::stdin;
use std::io::Read;

pub struct Keyboard {
    recv: std::sync::mpsc::Receiver<u8>,
}

impl Default for Keyboard {
    fn default() -> Self {
        let mut termios = nix::sys::termios::tcgetattr(STDIN_FILENO).unwrap();
        termios.input_flags.set(InputFlags::ICRNL, true);
        termios.local_flags.set(LocalFlags::ECHO, false);
        termios.local_flags.set(LocalFlags::ICANON, false);

        nix::sys::termios::tcsetattr(STDIN_FILENO, SetArg::TCSAFLUSH, &termios).unwrap();

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
}

impl Keyboard {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_character(&mut self) -> Option<char> {
        let byte = self.recv.recv_timeout(std::time::Duration::from_nanos(0));
        byte.ok().map(|c| if c == b'\n' { '\r' } else { c as char })
    }
}
