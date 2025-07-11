// https://www.codingame.com/training/hard/kgood

use itertools::Itertools;
use std::cmp::Ordering;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::io;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
struct CharFreq {
    char: char,
    freq: u32,
}

impl CharFreq {
    fn new(char: char, freq: u32) -> Self {
        Self { char, freq }
    }
}

impl Display for CharFreq {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.char.to_string().repeat(self.freq as usize))
    }
}

fn reduce_string(s: &str) -> Vec<CharFreq> {
    s.chars()
        .group_by(|&c| c)
        .into_iter()
        .map(|(c, chars)| CharFreq::new(c, chars.count() as u32))
        .collect()
}

#[derive(Debug)]
enum SlidingWindowDropEndError {
    EmptyWindow,
    NotFound(CharFreq),
    ContainsLessThanDropped { dropped: CharFreq, contained: u32 },
}

impl Display for SlidingWindowDropEndError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EmptyWindow => write!(f, "can't drop chars from an empty window"),
            Self::NotFound(char) => {
                write!(f, "Char removed {} is not part of the window", char.char)
            }
            Self::ContainsLessThanDropped { dropped, contained } => write!(
                f,
                "Can't remove {} occurrences of {} because only {} are present in the window",
                dropped.freq, dropped.char, contained
            ),
        }
    }
}

impl std::error::Error for SlidingWindowDropEndError {}

#[derive(Debug, Clone)]
struct SlidingWindow<'a> {
    counts: HashMap<char, u32>,
    start: std::slice::Iter<'a, CharFreq>,
    end: std::iter::Peekable<std::slice::Iter<'a, CharFreq>>,
}

impl<'a> SlidingWindow<'a> {
    fn new(items: &'a [CharFreq]) -> Self {
        Self {
            counts: HashMap::new(),
            start: items.iter(),
            end: items.iter().peekable(),
        }
    }

    fn k_value(&self) -> u32 {
        self.counts.len() as u32
    }

    fn total_chars(&self) -> u32 {
        self.counts.values().sum()
    }

    fn drop_start(&mut self) -> Result<CharFreq, SlidingWindowDropEndError> {
        let start = self
            .start
            .next()
            .ok_or(SlidingWindowDropEndError::EmptyWindow)?;

        match self.counts.entry(start.char) {
            Entry::Vacant(_) => Err(SlidingWindowDropEndError::NotFound(*start)),
            Entry::Occupied(mut entry) => match entry.get().cmp(&start.freq) {
                Ordering::Less => Err(SlidingWindowDropEndError::ContainsLessThanDropped {
                    dropped: *start,
                    contained: *entry.get(),
                }),
                Ordering::Equal => {
                    entry.remove();
                    Ok(*start)
                }
                Ordering::Greater => {
                    *entry.get_mut() -= start.freq;
                    Ok(*start)
                }
            },
        }
    }

    fn take_end(&mut self) -> Result<CharFreq, ()> {
        let next = self.end.next().copied().ok_or(())?;

        let char_count = self.counts.entry(next.char).or_default();
        *char_count += next.freq;
        Ok(next)
    }

    fn next_is_contained(&mut self) -> bool {
        let maybe_next_char = self.end.peek().copied().copied();

        maybe_next_char
            .is_some_and(|next_char| self.counts.contains_key(&next_char.char))
    }
}

fn iterate(sliding_window: &mut SlidingWindow, k_limit: u32) -> bool {
    let current_size = sliding_window.k_value();

    // If you can add while staying below the limit, add
    if (current_size < k_limit || sliding_window.next_is_contained()) && sliding_window.take_end().is_ok() {
        return true;
    }

    // Shrink otherwise
    match sliding_window.drop_start() {
        Ok(_) => true,
        Err(SlidingWindowDropEndError::EmptyWindow) => false, // If you can't do either, window remains unchanged
        Err(unrecoverable) => panic!("Unrecoverable error: {unrecoverable}"),
    }
}

fn biggest_k(s: &str, k_limit: u32) -> u32 {
    let reduced_string = reduce_string(s);
    let mut sliding_window = SlidingWindow::new(&reduced_string);
    std::iter::from_fn(move || {
        if iterate(&mut sliding_window, k_limit) {
            Some(sliding_window.clone())
        } else {
            None
        }
    })
    .map(|sliding_window| sliding_window.total_chars())
    .max()
    .unwrap_or_default()
}

fn expand_string(reduced_string: &[CharFreq]) -> String {
    reduced_string
        .iter()
        .map(|char_freq| char_freq.to_string())
        .join("")
}

macro_rules! parse_input {
    ($x:expr, $t:ident) => {
        $x.trim().parse::<$t>().unwrap()
    };
}

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
fn main() {
    let mut input_line = String::new();
    io::stdin().read_line(&mut input_line).unwrap();
    let s = input_line.trim_matches('\n').to_string();
    let mut input_line = String::new();
    io::stdin().read_line(&mut input_line).unwrap();
    let k = parse_input!(input_line, u32);

    // Write an answer using println!("message...");
    // To debug: eprintln!("Debug message...");

    println!("{}", biggest_k(s.as_str(), k));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_3() {
        let string = "phcaiodkhycinmbakdsjfplekgwaxkgqbhbhmvtlvwrmrvkqxeepjhvfdmjjewhthnhilkrxanxrelqajavubjfiaxebsjmjgsbfmxganqffrmlcydcsvxpnawskrhmutgpnkpwopudobptikjrfogdeairwyglcrqepnybcpinrybrjudcjalxumwedvngnkvoportnqeshafaodwxwjxvonaumvdtmtkotppuisubxhwcmunbdxcatppdswnsmjnygnrikvienvpysfuxrlikbkndeyxxj";

        assert_eq!(string, expand_string(&reduce_string(string)));

        let target_k = 23;

        let expected = 102;

        assert_eq!(biggest_k(string, target_k), expected);
    }
}
