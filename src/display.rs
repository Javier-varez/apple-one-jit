use std::io::Write;

const COLUMNS: usize = 80;
const LINES: usize = 25;

#[derive(Default)]
struct Cursor {
    line: usize,
    column: usize,
}

pub struct Display {
    chars: [[char; COLUMNS]; LINES],
    cursor: Cursor,
}

impl Default for Display {
    fn default() -> Self {
        print!("\x1b[2J");
        print!("\x1b[=3h");
        Self {
            chars: [[' '; COLUMNS]; LINES],
            cursor: Cursor::default(),
        }
    }
}

impl Display {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn shift_line(&mut self) {
        for i in 1..LINES {
            let (first, rest) = self.chars.split_at_mut(i);
            *first.last_mut().unwrap() = rest[0];
        }
        self.chars
            .last_mut()
            .unwrap()
            .iter_mut()
            .for_each(|c| *c = ' ');
    }

    pub fn move_cursor(&mut self) {
        self.cursor.column += 1;
        if self.cursor.column >= COLUMNS {
            self.cursor.line += 1;
            self.cursor.column = 0;
        }

        if self.cursor.line >= LINES {
            self.cursor.line = LINES - 1;
            // shift display up
            self.shift_line();
        }
    }

    pub fn push_char(&mut self, c: char) {
        const NON_PRINTABLE_BOUNDARY_LOW: char = 0x20 as char;
        const NON_PRINTABLE_BOUNDARY_HIGH: char = 0x80 as char;
        const BACKSPACE: char = 0x08 as char;
        if c == '\r' {
            self.cursor.line += 1;
            self.cursor.column = 0;
            if self.cursor.line >= LINES {
                self.cursor.line = LINES - 1;
                self.shift_line();
            }
        } else if c == BACKSPACE {
            if self.cursor.column > 0 {
                self.cursor.column -= 1;
                self.chars[self.cursor.line][self.cursor.column] = ' ';
            }
        } else if c >= NON_PRINTABLE_BOUNDARY_LOW && c < NON_PRINTABLE_BOUNDARY_HIGH {
            self.chars[self.cursor.line][self.cursor.column] = c;
            self.move_cursor();
        } else {
            // Non printable, just print a space
            self.chars[self.cursor.line][self.cursor.column] = ' ';
            self.move_cursor();
        }
        self.update();
    }

    pub fn update(&mut self) {
        print!("\x1b[2J"); // Clear screen
        print!("\x1b[1;1H"); // Move cursor to beggining
        for line in self.chars {
            for val in line {
                print!("{}", val);
            }
            println!();
        }
        print!("\x1b[{};{}H", self.cursor.line + 1, self.cursor.column + 1); // move cursor
        std::io::stdout().flush().unwrap();
    }
}
