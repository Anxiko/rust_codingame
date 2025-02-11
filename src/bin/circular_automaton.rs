// https://www.codingame.com/training/medium/circular-automation-the-period-of-chaos

use itertools::Itertools;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::iter::successors;
use std::str::FromStr;
use std::convert::TryInto;

#[derive(Hash, PartialEq, Eq)]
struct Memory {
    bits: Vec<bool>,
}

type Reading = (bool, bool, bool);

impl Memory {
    fn new(bits: Vec<bool>) -> Self {
        Self { bits }
    }

    fn from_size(size: usize) -> Self {
        let mut bits = vec![false; size];
        bits[size / 2] = true;
        Self::new(bits)
    }

    fn reading(&self, at: usize) -> Reading {
        let middle = *self.bits.get(at).unwrap();

        let left_idx = at.checked_sub(1).unwrap_or(self.bits.len() - 1);
        let left = *self.bits.get(left_idx).unwrap();

        let right_idx = (at + 1) % self.bits.len();
        let right = *self.bits.get(right_idx).unwrap();

        (left, middle, right)
    }

    fn iterate(&self, ruleset: &Ruleset) -> Self {
        let bits = self
            .bits
            .iter()
            .copied()
            .enumerate()
            .map(|(idx, _val)| {
                let reading = self.reading(idx);
                ruleset.next_state(reading)
            })
            .collect_vec();
        Self::new(bits)
    }
}

impl Display for Memory {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.bits.iter().try_for_each(|bit| {
            let bit = if *bit { '1' } else { '.' };
            write!(f, "{} ", bit)
        })
    }
}

fn byte_to_bits(byte: u8) -> [bool; 8] {
    (0..8)
        .map(|i| byte & (1 << i) != 0)
        .collect_vec()
        .try_into()
        .unwrap()
}

fn bits_to_byte(bits: &[bool]) -> u8 {
    bits.iter()
        .copied()
        .enumerate()
        .map(|(idx, bit)| (bit as u8) << idx)
        .sum()
}

struct Ruleset {
    next_states: [bool; 8],
}

impl Ruleset {
    fn new(next_states: [bool; 8]) -> Ruleset {
        Ruleset { next_states }
    }

    fn from_rule(rule: u8) -> Ruleset {
        Self::new(byte_to_bits(rule))
    }

    fn next_state(&self, (left, middle, right): Reading) -> bool {
        let byte = bits_to_byte(&[left, middle, right]);
        self.next_states[byte as usize]
    }
}

fn read_input<T: FromStr>() -> T {
    let mut buf = String::new();
    std::io::stdin().read_line(&mut buf).unwrap();
    buf.trim().parse().ok().expect("Error parsing input")
}

type RepeatResult = Result<HashMap<Memory, u32>, u32>;

fn main() {
    let length: usize = read_input();
    let max_iter: usize = read_input();
    let rule_number: u8 = read_input();

    let memory = Memory::from_size(length);
    let ruleset = Ruleset::from_rule(rule_number);

    let result: RepeatResult = successors(Some(memory), |memory| Some(memory.iterate(&ruleset)))
        .inspect(|memory| eprintln!("{memory}"))
        .enumerate()
        .take(max_iter)
        .try_fold(
            HashMap::<Memory, u32>::new(),
            |mut acc, (idx, memory)| match acc.entry(memory) {
                Entry::Occupied(previous) => Err(idx as u32 - *previous.get()),
                Entry::Vacant(vacant) => {
                    vacant.insert(idx as u32);
                    Ok(acc)
                }
            },
        );

    match result {
        Ok(_) => println!("BIG"),
        Err(cycle) => println!("{cycle}"),
    }
}
