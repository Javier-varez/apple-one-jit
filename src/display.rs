use std::io::Write;

const COLUMNS: usize = 80;
const LINES: usize = 25;

struct Cursor {
    line: usize,
    column: usize,
}

pub struct Display {
    chars: [[char; COLUMNS]; LINES],
    cursor: Cursor,
}

impl Display {
    pub fn new() -> Self {
        print!("\x1b[2J");
        print!("\x1b[=3h");
        Self {
            chars: [[' '; COLUMNS]; LINES],
            cursor: Cursor { line: 0, column: 0 },
        }
    }

    pub fn shift_line(&mut self) {
        for i in 1..LINES {
            let (first, rest) = self.chars.split_at_mut(i);
            *first.last_mut().unwrap() = rest[0].clone();
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
        if c == '\r' {
            self.cursor.line += 1;
            self.cursor.column = 0;
            if self.cursor.line >= LINES {
                self.cursor.line = LINES - 1;
                self.shift_line();
            }
        } else {
            self.chars[self.cursor.line][self.cursor.column] = c;
            self.move_cursor();
        }
        self.update();
    }

    pub fn update(&mut self) {
        print!("\x1b[2J"); // Clear screen
        print!("\x1b[1;1H"); // Move cursor to beggining
        for (idx, line) in self.chars.iter().enumerate() {
            for val in line {
                print!("{}", val);
            }
            print!("\n");
            // print!("\x1b[{};1H", idx + 1);
        }
        print!("\x1b[{};{}H", self.cursor.line + 1, self.cursor.column + 1); // move cursor
        std::io::stdout().flush().unwrap();
    }
}
