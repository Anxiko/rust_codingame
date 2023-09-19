use std::io;
use std::str::FromStr;
use itertools::Itertools;

type Word = i32;

#[derive(Copy, Clone)]
enum Register {
    A,
    B,
    C,
    D,
}

impl FromStr for Register {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "a" => Ok(Register::A),
            "b" => Ok(Register::B),
            "c" => Ok(Register::C),
            "d" => Ok(Register::D),
            _ => Err(())
        }
    }
}

#[derive(Default)]
struct RegisterBank {
    a: Word,
    b: Word,
    c: Word,
    d: Word,
}

impl RegisterBank {
    fn read(&self, r: Register) -> Word {
        match r {
            Register::A => self.a,
            Register::B => self.b,
            Register::C => self.c,
            Register::D => self.d
        }
    }

    fn write(&mut self, r: Register, v: Word) {
        let r_ref: &mut Word = match r {
            Register::A => &mut self.a,
            Register::B => &mut self.b,
            Register::C => &mut self.c,
            Register::D => &mut self.d
        };

        *r_ref = v;
    }
}

#[derive(Copy, Clone)]
enum RegOrImm {
    Reg(Register),
    Imm(Word),
}

impl FromStr for RegOrImm {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Register::from_str(s)
            .map(|reg| Self::Reg(reg))
            .or_else(|()|
                Word::from_str(s)
                    .map(|word| Self::Imm(word))
                    .map_err(|_| ())
            )
    }
}

#[derive(Copy, Clone)]
enum Instruction {
    Mov { dst: Register, src: RegOrImm },
    Add { dst: Register, left: RegOrImm, right: RegOrImm },
    Sub { dst: Register, left: RegOrImm, right: RegOrImm },
    Jne { addr: Word, reg: Register, value: RegOrImm },
}

impl FromStr for Instruction {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.split_whitespace().collect::<Vec<_>>()[..] {
            ["MOV", dst, reg_or_imm] => {
                let dst = Register::from_str(dst)?;
                let reg_or_imm = RegOrImm::from_str(reg_or_imm)?;

                Ok(Self::Mov { dst, src: reg_or_imm })
            }
            ["ADD", dst, left, right] => {
                let dst = Register::from_str(dst)?;
                let left = RegOrImm::from_str(left)?;
                let right = RegOrImm::from_str(right)?;

                Ok(Self::Add { dst, left, right })
            }
            ["SUB", dst, left, right] => {
                let dst = Register::from_str(dst)?;
                let left = RegOrImm::from_str(left)?;
                let right = RegOrImm::from_str(right)?;

                Ok(Self::Sub { dst, left, right })
            }
            ["JNE", addr, reg, value] => {
                let addr = Word::from_str(addr)
                    .map_err(|_| ())?;
                let reg = Register::from_str(reg)?;
                let value = RegOrImm::from_str(value)?;

                Ok(Self::Jne { addr, reg, value })
            }
            _ => Err(())
        }
    }
}

struct Interpreter {
    bank: RegisterBank,
    pc: Word,
    program: Vec<Instruction>,
}

impl Interpreter {
    fn new(program: Vec<Instruction>) -> Self {
        Self {
            bank: RegisterBank::default(),
            pc: Word::default(),
            program,
        }
    }

    fn read(&self, reg_or_imm: RegOrImm) -> Word {
        match reg_or_imm {
            RegOrImm::Reg(reg) => self.bank.read(reg),
            RegOrImm::Imm(word) => word
        }
    }

    fn write(&mut self, r: Register, v: Word) {
        self.bank.write(r, v);
    }

    fn go_next_pc(&mut self) {
        self.pc += 1;
    }

    fn go_addr(&mut self, addr: Word) {
        self.pc = addr;
    }

    fn execute(&mut self, inst: Instruction) {
        let next_pc: Option<Word> = match inst {
            Instruction::Mov { dst, src } => {
                self.write(dst, self.read(src));
                None
            }
            Instruction::Add { dst, left, right } => {
                let result = self.read(left) + self.read(right);
                self.write(dst, result);
                None
            }
            Instruction::Sub { dst, left, right } => {
                let result = self.read(left) - self.read(right);
                self.write(dst, result);
                None
            }
            Instruction::Jne { addr, reg, value } => {
                if self.bank.read(reg) != self.read(value) {
                    Some(addr)
                } else {
                    None
                }
            }
        };

        if let Some(next_pc) = next_pc {
            self.go_addr(next_pc);
        } else {
            self.go_next_pc();
        }
    }

    fn run(&mut self) -> bool {
        let pc: usize = self.pc.try_into().unwrap();
        if let Some(instruction) = self.program.get(pc).copied() {
            self.execute(instruction);
            false
        } else {
            true
        }
    }

    fn values(&self) -> [Word; 4] {
        [Register::A, Register::B, Register::C, Register::D]
            .map(|reg| self.bank.read(reg))
    }
}

fn parse_program() -> Vec<Instruction> {
    let mut n_instructions = String::new();
    io::stdin().read_line(&mut n_instructions).unwrap();
    let n_instructions = usize::from_str(&n_instructions).unwrap();

    (0..n_instructions)
        .map(|_| {
            let mut instruction = String::new();
            io::stdin().read_line(&mut instruction).unwrap();

            Instruction::from_str(&instruction).unwrap()
        })
        .collect()
}

fn main() {
    let program = parse_program();
    let mut interpreter = Interpreter::new(program);

    while !interpreter.run() {}

    let values = interpreter.values();
    let values = values
        .into_iter()
        .map(|reg| reg.to_string())
        .join(" ");

    println!("{values}");
}