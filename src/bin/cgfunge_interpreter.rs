// https://www.codingame.com/ide/puzzle/cgfunge-interpreter

use itertools::Itertools;
use std::fmt;
use std::io;
use std::ops::{Add, AddAssign};

type Value = u8;
type Coord = isize;

#[derive(Default, Copy, Clone)]
struct Position {
    x: Coord,
    y: Coord,
}

impl Position {
    fn new(x: Coord, y: Coord) -> Position {
        Position { x, y }
    }
}

impl Add<Position> for Position {
    type Output = Position;

    fn add(self, rhs: Position) -> Self::Output {
        Self::new(self.x + rhs.x, self.y + rhs.y)
    }
}

impl AddAssign for Position {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

#[derive(Default, Copy, Clone)]
enum Direction {
    Up,
    Down,
    Left,
    #[default]
    Right,
}

impl Direction {
    fn delta(self) -> Position {
        match self {
            Direction::Up => Position { x: 0, y: -1 },
            Direction::Down => Position { x: 0, y: 1 },
            Direction::Left => Position { x: -1, y: 0 },
            Direction::Right => Position { x: 1, y: 0 },
        }
    }
}

#[derive(Copy, Clone)]
enum Axis {
    Horizontal,
    Vertical,
}

impl Axis {
    fn to_conditional_direction(self, condition: bool) -> Direction {
        match self {
            Axis::Horizontal => {
                if condition {
                    Direction::Right
                } else {
                    Direction::Left
                }
            }
            Axis::Vertical => {
                if condition {
                    Direction::Down
                } else {
                    Direction::Up
                }
            }
        }
    }
}

#[derive(Copy, Clone)]
enum Instruction {
    Move(Direction),
    Digit(Value),
    ArithmeticOperation(ArithmeticOperation),
    ToggleAscii,
    Pop,
    Switch,
    Duplicate,
    Conditional(Axis),
    Print { use_ascii: bool },
    Skip,
    NoOp,
    End,
}

impl Instruction {
    fn execute(self, state: &mut State) -> Result<(), ExecutionError> {
        match self {
            Self::Move(direction) => {
                state.set_direction(direction);
            }
            Self::Digit(value) => {
                state.push_stack(value);
            }
            Self::ArithmeticOperation(operation) => {
                let left = state.pop_stack()?;
                let right = state.pop_stack()?;
                let result = operation.calculate(left, right);
                state.push_stack(result);
            }
            Self::ToggleAscii => {
                state.set_ascii_mode(true);
            }
            Self::Pop => {
                let _ = state.pop_stack()?;
            }
            Self::Switch => {
                let left = state.pop_stack()?;
                let right = state.pop_stack()?;
                state.push_stack(left);
                state.push_stack(right);
            }
            Self::Duplicate => {
                let value = state.pop_stack()?;
                state.push_stack(value);
                state.push_stack(value);
            }
            Self::Conditional(axis) => {
                let condition = state.pop_stack()? == 0;
                let direction = axis.to_conditional_direction(condition);
                state.set_direction(direction);
            }
            Self::Print { use_ascii } => {
                let value = state.pop_stack()?;
                if use_ascii {
                    let char = value as char;
                    if !char.is_ascii() {
                        return Err(ExecutionError::InvalidAsciiCharacter(char));
                    }
                    print!("{}", char);
                } else {
                    print!("{}", value);
                }
            }
            Self::Skip => {
                state.set_skipping(true);
            }
            Self::NoOp => {}
            Self::End => {
                state.finish();
            }
        }

        Ok(())
    }
}

impl TryFrom<char> for Instruction {
    type Error = char;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            '>' => Ok(Instruction::Move(Direction::Right)),
            '<' => Ok(Instruction::Move(Direction::Left)),
            '^' => Ok(Instruction::Move(Direction::Up)),
            'v' => Ok(Instruction::Move(Direction::Down)),
            digit if digit.is_ascii_digit() => Ok(Instruction::Digit(digit.to_digit(10).unwrap() as Value)),
            '+' => Ok(Instruction::ArithmeticOperation(ArithmeticOperation::Add)),
            '-' => Ok(Instruction::ArithmeticOperation(ArithmeticOperation::Subtract)),
            '*' => Ok(Instruction::ArithmeticOperation(ArithmeticOperation::Multiply)),
            '"' => Ok(Instruction::ToggleAscii),
            'P' => Ok(Instruction::Pop),
            'X' => Ok(Instruction::Switch),
            'D' => Ok(Instruction::Duplicate),
            '_' => Ok(Instruction::Conditional(Axis::Horizontal)),
            '|' => Ok(Instruction::Conditional(Axis::Vertical)),
            'I' => Ok(Instruction::Print { use_ascii: false }),
            'C' => Ok(Instruction::Print { use_ascii: true }),
            'S' => Ok(Instruction::Skip),
            ' ' => Ok(Instruction::NoOp),
            'E' => Ok(Instruction::End),
            _ => Err(value),
        }
    }
}

#[derive(Copy, Clone)]
enum ArithmeticOperation {
    Add,
    Subtract,
    Multiply,
}

impl ArithmeticOperation {
    fn calculate(self, left: Value, right: Value) -> Value {
        match self {
            Self::Add => left + right,
            Self::Subtract => left - right,
            Self::Multiply => left * right,
        }
    }
}

#[derive(Default)]
struct State {
    stack: Vec<Value>,
    direction: Direction,
    position: Position,
    ascii_mode: bool,
    finished: bool,
    skipping: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExecutionError {
    FinishedState,
    InvalidInstruction(char),
    InvalidAsciiCharacter(char),
    EmptyStack,
}

impl fmt::Display for ExecutionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FinishedState => write!(f, "interpreter finished"),
            Self::InvalidInstruction(c) => write!(f, "invalid instruction: '{c}'"),
            Self::InvalidAsciiCharacter(c) => write!(f, "invalid ascii character: '{c}'"),
            Self::EmptyStack => write!(f, "stack is empty"),
        }
    }
}

impl std::error::Error for ExecutionError {}

impl State {
    fn is_finished(&self) -> bool {
        self.finished
    }

    fn is_skipping(&self) -> bool {
        self.skipping
    }

    fn finish(&mut self) {
        self.finished = true;
    }

    fn set_skipping(&mut self, skipping: bool) {
        self.skipping = skipping;
    }

    fn push_stack(&mut self, value: Value) {
        self.stack.push(value);
    }

    fn pop_stack(&mut self) -> Result<Value, ExecutionError> {
        self.stack.pop().ok_or(ExecutionError::EmptyStack)
    }

    fn set_direction(&mut self, direction: Direction) {
        self.direction = direction;
    }

    fn set_ascii_mode(&mut self, ascii_mode: bool) {
        self.ascii_mode = ascii_mode;
    }

    fn step(&mut self, board: &Board) -> Result<(), ExecutionError> {
        if self.is_finished() {
            return Err(ExecutionError::FinishedState);
        }

        if !self.is_skipping() {
            let char = board.read(self.position).expect("Failed to read character from board");

            if self.ascii_mode {
                if char == '"' {
                    self.set_ascii_mode(false);
                } else {
                    let ascii_char: Value = char
                        .try_into()
                        .map_err(|_| ExecutionError::InvalidAsciiCharacter(char))?;
                    self.push_stack(ascii_char);
                }
            } else {
                let instruction = Instruction::try_from(char).map_err(|_| ExecutionError::InvalidInstruction(char))?;
                instruction.execute(self)?;
            }
        } else {
            self.set_skipping(false);
        }

        self.position += self.direction.delta();

        Ok(())
    }

    fn run(&mut self, board: &Board) -> Result<(), ExecutionError> {
        while !self.is_finished() {
            self.step(board)?;
        }
        Ok(())
    }
}

struct Board {
    board: Vec<Vec<char>>,
}

impl Board {
    fn new(board: Vec<Vec<char>>) -> Board {
        Board { board }
    }

    fn read(&self, position: Position) -> Option<char> {
        let y: usize = position.y.try_into().ok()?;
        let x: usize = position.x.try_into().ok()?;
        self.board.get(y)?.get(x).copied()
    }
}

fn read_line() -> String {
    let mut buffer = String::new();
    io::stdin()
        .read_line(&mut buffer)
        .expect("Failed to read line from stdin");
    buffer.trim_end_matches(['\r', '\n']).to_string()
}

fn main() {
    let mut state = State::default();

    let n_lines = read_line().parse::<usize>().expect("Failed to parse number of lines");

    let board = (0..n_lines)
        .into_iter()
        .map(|_| read_line().chars().collect_vec())
        .collect_vec();

    let board = Board::new(board);

    state.run(&board).expect("Failed to run state");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_text() {
        let board = vec![r#""txet elpmiS"CCCCCCCCCCCE"#]
            .into_iter()
            .map(|s| s.chars().collect_vec());

        let board = Board::new(board.collect());

        let mut state = State::default();

        state.run(&board).expect("Failed to run state");
    }

    #[test]
    fn go_all_ways() {
        let raw_board = r#"
v > v
  C "
 vC GS<
> "GC"^
  C "
  G C
 C" C
 >^ E
"#
        .strip_prefix('\n')
        .unwrap()
        .strip_suffix('\n')
        .unwrap();

        let board_rows: Vec<Vec<char>> = raw_board.lines().map(|line| line.chars().collect()).collect();
        let board = Board::new(board_rows);

        let mut state: State = State::default();
        state.run(&board).expect("Failed to run state");
    }
}
