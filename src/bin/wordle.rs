// https://www.codingame.com/multiplayer/optimization/wordle

use std::{io, thread};
use std::cmp::Ordering;
use std::collections::{HashSet, VecDeque};
use std::convert::TryInto;
use std::fmt::{Debug, Display, Formatter};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::ops::{Add, AddAssign};
use std::str::FromStr;
use std::sync::{Arc, Mutex};

use itertools::Itertools;

const PARTITION_MAX_WORDS: Option<usize> = Some(200);

#[derive(Copy, Clone)]
struct NotNaN {
    value: f32,
}

impl NotNaN {
    fn new(value: f32) -> Result<Self, ()> {
        if value.is_nan() {
            Err(())
        } else {
            Ok(Self { value })
        }
    }
}

impl PartialEq for NotNaN {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl Eq for NotNaN {}

impl PartialOrd for NotNaN {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.value == other.value {
            Some(Ordering::Equal)
        } else if self.value > other.value {
            Some(Ordering::Greater)
        } else {
            Some(Ordering::Less)
        }
    }
}

impl Ord for NotNaN {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

const CHARACTERS_PER_WORD: usize = 6;

fn read_input<T: FromStr>() -> T {
    let mut s = String::new();
    io::stdin().read_line(&mut s).unwrap();
    s.trim().parse().ok().unwrap()
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum CharacterFeedback {
    Missing,
    Misplaced,
    Correct,
    Unknown,
}

impl Display for CharacterFeedback {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let chr = match self {
            Self::Missing => '-',
            Self::Misplaced => '+',
            Self::Correct => '=',
            Self::Unknown => '?'
        };
        write!(f, "{chr}")
    }
}

impl FromStr for CharacterFeedback {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "0" => Ok(Self::Unknown),
            "1" => Ok(Self::Missing),
            "2" => Ok(Self::Misplaced),
            "3" => Ok(Self::Correct),
            _ => Err(())
        }
    }
}

impl CharacterFeedback {
    fn read_feedback() -> [Self; CHARACTERS_PER_WORD] {
        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer).unwrap();

        buffer
            .split_whitespace()
            .map(|s| s.parse().unwrap())
            .collect::<Vec<Self>>()
            .try_into()
            .unwrap()
    }
}

#[derive(Debug)]
enum Feedback {
    Unknown,
    PerCharacter([CharacterFeedback; CHARACTERS_PER_WORD]),
    Correct,
}

impl Display for Feedback {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let chr_feedbacks: [CharacterFeedback; CHARACTERS_PER_WORD] = match self {
            Self::PerCharacter(feedbacks) => *feedbacks,
            Self::Correct => [CharacterFeedback::Correct; CHARACTERS_PER_WORD],
            Self::Unknown => [CharacterFeedback::Unknown; CHARACTERS_PER_WORD]
        };
        for feedback in chr_feedbacks {
            write!(f, "{feedback}")?;
        }
        Ok(())
    }
}

impl Feedback {
    fn read() -> Self {
        let feedbacks = CharacterFeedback::read_feedback();

        let unknowns = feedbacks
            .iter()
            .filter(|char_feedback|
                matches!(char_feedback, CharacterFeedback::Unknown)
            )
            .count();

        match unknowns {
            0 => {
                let corrects = feedbacks
                    .iter()
                    .filter(|char_feedback|
                        matches!(char_feedback, CharacterFeedback::Correct)
                    )
                    .count();
                if corrects == CHARACTERS_PER_WORD {
                    Self::Correct
                } else {
                    Self::PerCharacter(feedbacks)
                }
            }
            CHARACTERS_PER_WORD => {
                Self::Unknown
            }
            _ => panic!("Unexpected number of unknown feedback: {unknowns}")
        }
    }

    fn from_guess(guess: &Word, chosen: &Word) -> Self {
        let char_feedback: [CharacterFeedback; CHARACTERS_PER_WORD] = guess
            .iter()
            .enumerate()
            .map(|(idx, c)| {
                if chosen.has_char_at(c, idx) {
                    CharacterFeedback::Correct
                } else if chosen.has_char(c) {
                    CharacterFeedback::Misplaced
                } else {
                    CharacterFeedback::Missing
                }
            })
            .collect_vec()
            .try_into()
            .unwrap();

        let unknowns = char_feedback
            .iter()
            .copied()
            .filter(|&cf| cf == CharacterFeedback::Unknown)
            .count();

        if unknowns > 0 {
            if unknowns == CHARACTERS_PER_WORD {
                return Self::Unknown;
            }
            panic!("Invalid count of unknowns: {unknowns}")
        }

        if char_feedback.iter().all(|&feedback| feedback == CharacterFeedback::Correct) {
            return Self::Correct;
        }

        Self::PerCharacter(char_feedback)
    }

    fn get_per_character(&self) -> [CharacterFeedback; CHARACTERS_PER_WORD] {
        match &self {
            Self::Correct => [CharacterFeedback::Correct; CHARACTERS_PER_WORD],
            Self::Unknown => [CharacterFeedback::Unknown; CHARACTERS_PER_WORD],
            Self::PerCharacter(per_character) => *per_character
        }
    }
}

#[derive(Debug, Copy, Clone)]
enum CharacterRule {
    ContainsCharacterHere { idx: usize, chr: char },
    ContainsCharacterElsewhere { idx: usize, chr: char },
    NotContainsCharacter { chr: char },
}


impl CharacterRule {
    fn new(chr: char, idx: usize, state: CharacterFeedback) -> Self {
        match state {
            CharacterFeedback::Unknown => panic!("Can't build rule from unknown state"),
            CharacterFeedback::Correct => Self::ContainsCharacterHere { idx, chr },
            CharacterFeedback::Misplaced => Self::ContainsCharacterElsewhere { idx, chr },
            CharacterFeedback::Missing => Self::NotContainsCharacter { chr }
        }
    }

    fn satisfies(&self, word: &Word) -> bool {
        match *self {
            Self::ContainsCharacterHere { chr, idx } => {
                word.has_char_at(&chr, idx)
            }
            Self::ContainsCharacterElsewhere { chr, idx } => {
                !word.has_char_at(&chr, idx) && word.has_char(&chr)
            }
            Self::NotContainsCharacter { chr } => {
                !word.has_char(&chr)
            }
        }
    }
}

struct RuleSet {
    rules: [CharacterRule; CHARACTERS_PER_WORD],
}

impl RuleSet {
    fn from_guess_feedback(guess: &Word, feedback: &[CharacterFeedback; CHARACTERS_PER_WORD]) -> Self {
        let rules: Vec<_> =
            guess
                .iter()
                .copied()
                .enumerate()
                .zip(feedback.iter())
                .map(|((idx, chr), &state)|
                    CharacterRule::new(chr, idx, state))
                .collect();

        Self { rules: rules.try_into().unwrap() }
    }

    fn satisfies(&self, word: &Word) -> bool {
        self
            .rules
            .iter()
            .all(|rule| rule.satisfies(word))
    }
}

struct WordSolver {
    dictionary: Vec<Word>,
}

impl WordSolver {
    fn new(dictionary: Vec<String>) -> Self {
        let dictionary = dictionary
            .into_iter()
            .map(|word| Word::from_str(&word))
            .collect_vec();

        Self { dictionary }
    }

    fn read() -> Self {
        let n_words: usize = read_input();
        let dictionary: Vec<_> =
            read_input::<String>()
                .split_whitespace()
                .map(String::from)
                .collect();

        if dictionary.len() != n_words {
            panic!("Unexpected number of words in dictionary");
        }

        Self::new(dictionary)
    }

    fn generate_guess_by_bruteforce(&self) -> Word {
        if self.dictionary.len() <= 2 {
            let word = self.dictionary.first().expect("Not empty dictionary");
            return word.clone();
        }

        DictionaryTreeNode::from_words(self.dictionary.iter().collect_vec())
            .get_guess()
    }

    fn generate_guess_by_words(&self) -> Word {
        let dictionary: Box<dyn Iterator<Item=&Word>> =
            if let Some(max_words) = PARTITION_MAX_WORDS {
                Box::new(self.dictionary.iter().take(max_words))
            } else {
                Box::new(self.dictionary.iter())
            };
        dictionary
            .enumerate()
            .map(|(_idx, word)| {
                DictionaryTreeNode::by_word(self.dictionary.iter().collect_vec(), word)
            })
            .max_by_key(|node| NotNaN::new(node.entropy()).unwrap())
            .unwrap()
            .get_guess()
    }

    fn update(&mut self, guess: &Word, feedback: &[CharacterFeedback; CHARACTERS_PER_WORD]) {
        let ruleset = RuleSet::from_guess_feedback(guess, feedback);
        self.dictionary.retain(|word| ruleset.satisfies(word))
    }
}

#[derive(Copy, Clone)]
enum PartitionResult {
    Exact,
    Misplaced,
    Missing,
}

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
struct Word {
    characters: [char; CHARACTERS_PER_WORD],
}

impl Word {
    fn new(characters: [char; CHARACTERS_PER_WORD]) -> Self {
        Self { characters }
    }

    fn from_str(s: &str) -> Self {
        let characters: [char; CHARACTERS_PER_WORD] =
            s
                .chars()
                .collect_vec()
                .try_into()
                .unwrap();
        Self::new(characters)
    }

    fn partition(&self, chr: char, idx: usize) -> PartitionResult {
        if self.characters[idx] == chr {
            PartitionResult::Exact
        } else if self.characters.contains(&chr) {
            PartitionResult::Misplaced
        } else {
            PartitionResult::Missing
        }
    }

    fn iter(&self) -> impl Iterator<Item=&char> {
        self.characters.iter()
    }

    fn has_char_at(&self, chr: &char, idx: usize) -> bool {
        self.characters[idx] == *chr
    }

    fn has_char(&self, chr: &char) -> bool {
        self.characters.contains(chr)
    }

    fn permutations(&self) -> impl Iterator<Item=Word> {
        self.characters
            .into_iter()
            .permutations(CHARACTERS_PER_WORD)
            .map(|characters| {
                let characters: [char; CHARACTERS_PER_WORD] = characters.try_into().unwrap();
                Word::new(characters)
            })
            .unique()
    }
}

impl From<&Word> for String {
    fn from(value: &Word) -> Self {
        value.iter().collect()
    }
}

impl Display for Word {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let word = String::from(self);
        std::fmt::Display::fmt(&word, f)
    }
}


struct PartitionBy<'a> {
    exact: Vec<&'a Word>,
    misplaced: Vec<&'a Word>,
    missing: Vec<&'a Word>,
}

impl<'a> PartitionBy<'a> {
    fn partition(words: &[&'a Word], chr: char, idx: usize) -> PartitionBy<'a> {
        let mut exact: Vec<&'a Word> = Vec::new();
        let mut misplaced: Vec<&'a Word> = Vec::new();
        let mut missing: Vec<&'a Word> = Vec::new();

        for &word in words {
            match word.partition(chr, idx) {
                PartitionResult::Exact => exact.push(word),
                PartitionResult::Misplaced => misplaced.push(word),
                PartitionResult::Missing => missing.push(word)
            }
        }

        Self { exact, misplaced, missing }
    }

    fn split(self) -> (Vec<&'a Word>, Vec<&'a Word>, Vec<&'a Word>) {
        (self.exact, self.misplaced, self.missing)
    }
}

fn entropy(probabilities: &[f32]) -> f32 {
    let s: f32 = probabilities
        .iter()
        .copied()
        .map(|p|
            if p == 0f32 { p } else { p * p.log2() }
        )
        .sum();
    -s
}

enum DictionaryTreeNode<'a> {
    Leaf(Vec<&'a Word>),
    PartitionedBy {
        chr: char,
        idx: usize,
        exact: Box<DictionaryTreeNode<'a>>,
        misplaced: Box<DictionaryTreeNode<'a>>,
        missing: Box<DictionaryTreeNode<'a>>,
    },
}

impl<'a> DictionaryTreeNode<'a> {
    fn leaf_sizes(&self) -> Vec<usize> {
        match self {
            Self::Leaf(words) => vec![words.len()],
            Self::PartitionedBy { exact, misplaced, missing, .. } => {
                [exact, misplaced, missing]
                    .into_iter()
                    .flat_map(|tree| tree.leaf_sizes())
                    .collect_vec()
            }
        }
    }

    fn entropy(&self) -> f32 {
        let sizes = self.leaf_sizes();
        let total_count: usize = sizes.iter().sum();
        if total_count == 0 {
            return 0.0;
        }

        let probabilities = sizes
            .into_iter()
            .map(|size| (size as f32) / (total_count as f32))
            .collect_vec();

        entropy(&probabilities)
    }

    fn sortable_entropy(&self) -> NotNaN {
        NotNaN::new(self.entropy()).unwrap()
    }

    fn partition(words: Vec<&'a Word>, idx: usize, open_characters: &HashSet<char>) -> DictionaryTreeNode<'a> {
        if idx == CHARACTERS_PER_WORD {
            DictionaryTreeNode::Leaf(words)
        } else {
            let open_chars_count = open_characters.len();

            open_characters
                .iter()
                .copied()
                .enumerate()
                .map(|(chr_idx, chr)| {
                    if idx < 3 {
                        eprintln!(
                            "Partition[{:02}/{open_chars_count:02}]: {}{}",
                            chr_idx + 1, String::from("=").repeat(idx), String::from(chr)
                        );
                    }

                    let (exact, misplaced, missing) = PartitionBy::partition(&words, chr, idx).split();
                    let exact = Self::partition(exact, idx + 1, open_characters);
                    let misplaced = Self::partition(misplaced, idx + 1, open_characters);
                    let missing = Self::partition(missing, idx + 1, open_characters);

                    DictionaryTreeNode::PartitionedBy {
                        idx,
                        chr,
                        exact: Box::new(exact),
                        misplaced: Box::new(misplaced),
                        missing: Box::new(missing),
                    }
                })
                .max_by_key(|node| NotNaN::new(node.entropy()).unwrap())
                .unwrap()
        }
    }

    fn from_words(words: Vec<&'a Word>) -> Self {
        let open_characters = words
            .iter()
            .fold(HashSet::new(), |mut acc, word| {
                for &chr in word.iter() {
                    acc.insert(chr);
                }
                acc
            });

        Self::partition(words, 0, &open_characters)
    }

    fn by_word(words: Vec<&'a Word>, word: &Word) -> Self {
        Self::by_chars(words, word.iter().copied().enumerate().collect())
    }

    fn by_chars(words: Vec<&'a Word>, mut chars: VecDeque<(usize, char)>) -> Self {
        if let Some((idx, chr)) = chars.pop_front() {
            let partition = PartitionBy::partition(&words, chr, idx);

            let exact = Self::by_chars(partition.exact, chars.clone());
            let misplaced = Self::by_chars(partition.misplaced, chars.clone());
            let missing = Self::by_chars(partition.missing, chars);

            Self::PartitionedBy {
                chr,
                idx,
                exact: Box::new(exact),
                misplaced: Box::new(misplaced),
                missing: Box::new(missing),
            }
        } else {
            Self::Leaf(words)
        }
    }

    fn get_guess(&self) -> Word {
        let mut result: Vec<char> = Vec::new();

        let mut node = self;
        while let Self::PartitionedBy { chr, exact, .. } = node {
            result.push(*chr);
            node = exact;
        }

        let characters: [char; CHARACTERS_PER_WORD] = result.try_into().unwrap();
        Word::new(characters)
    }
}

fn main() {
    let mut word_solver = WordSolver::read();
    let initial_feedback = Feedback::read();
    if !matches!(initial_feedback, Feedback::Unknown) {
        println!("Expected to receive initial feedback as unknown")
    }
    let mut precaculated_guesses: VecDeque<_> = vec!["BONIER"]
        .into_iter()
        .map(Word::from_str)
        .collect();

    loop {
        let guess = precaculated_guesses
            .pop_front()
            .unwrap_or_else(|| word_solver.generate_guess_by_words());

        println!("{guess}");

        match Feedback::read() {
            Feedback::Correct => {
                break;
            }
            Feedback::PerCharacter(per_character) => {
                word_solver.update(&guess, &per_character);
            }
            feedback => panic!("Unexpected feedback {:?}", feedback)
        }
    }
}

const FILE_PATH: &str = "wordle_words.txt";
const N_THREADS: usize = 32;
const INITIAL_GUESSES: &[&str] = &["BONIER"];

fn read_words(file: &str) -> Vec<String> {
    let file = File::open(file).unwrap();
    let reader = BufReader::new(file);

    reader
        .lines()
        .map(|word| word.unwrap())
        .filter(|word| word.len() == 6)
        .filter(|word| word.is_ascii())
        .map(|word| word.to_ascii_uppercase())
        .collect_vec()
}


#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::convert::identity;

    use rand::{seq::IteratorRandom, thread_rng};

    use crate::*;

    #[test]
    fn test_from_file() {
        let mut rng = thread_rng();

        let words = read_words(FILE_PATH);

        let chosen: Word = Word::from_str(&words.iter().choose(&mut rng).unwrap().to_owned());
        eprintln!("Chosen word: {chosen}");

        let mut word_solver = WordSolver::new(words);
        let mut n_tries = 0u32;

        loop {
            let guess = word_solver.generate_guess_by_words();
            let feedback = Feedback::from_guess(&guess, &chosen);
            n_tries += 1;

            eprintln!("Guess {n_tries}, {} word(s)", word_solver.dictionary.len());
            eprintln!("{chosen}");
            eprintln!("{guess}");
            eprintln!("{feedback}");

            match feedback {
                Feedback::Correct => break,
                Feedback::PerCharacter(feedback) => {
                    word_solver.update(&guess, &feedback);
                }
                Feedback::Unknown => unreachable!()
            }
        }

        eprintln!("{chosen} guessed in {n_tries} tries")
    }

    #[test]
    fn extract_initial_word() {
        let words = read_words(FILE_PATH)
            .into_iter()
            .map(|word| Word::from_str(&word))
            .collect_vec();

        let permutations = words
            .iter()
            .flat_map(Word::permutations)
            .collect_vec();

        let chunk_size = (permutations.len() / N_THREADS) + 1;


        let handles =
            permutations
                .into_iter()
                .chunks(chunk_size)
                .into_iter()
                .map(|words_chunk| {
                    let words_chunk = words_chunk.collect_vec();
                    let words = words.clone();
                    thread::spawn(move || {
                        let root =
                            words_chunk
                                .into_iter()
                                .map(|word|
                                    DictionaryTreeNode::by_word(words.iter().collect_vec(), &word)
                                )
                                .max_by_key(DictionaryTreeNode::sortable_entropy)
                                .unwrap();
                        (root.get_guess(), root.sortable_entropy())
                    })
                })
                .collect_vec();

        let (guess, _entropy) =
            handles
                .into_iter()
                .map(|handle| handle.join().unwrap())
                .max_by_key(|(_guess, entropy)| *entropy)
                .unwrap();


        eprintln!("Initial guess: {guess}")
    }

    #[test]
    fn extract_second_word() {
        let words = read_words(FILE_PATH);
        let chunk_size = (words.len() / N_THREADS) + 1;

        let handles =
            words
                .iter()
                .map(|word| Word::from_str(word))
                .chunks(chunk_size)
                .into_iter()
                .enumerate()
                .map(|(idx, words_chunk)| {
                    let thread_id = idx + 1;
                    let words_chunk = words_chunk.collect_vec();
                    let words = words.clone();
                    thread::spawn(move || {
                        let mut results: HashMap<Word, f32> = HashMap::new();
                        for chosen in words_chunk.iter() {
                            let mut word_solver = WordSolver::new(words.clone());
                            for initial_guess in INITIAL_GUESSES.iter().copied().map(Word::from_str) {
                                let feedback = Feedback::from_guess(&initial_guess, &chosen);
                                word_solver.update(&initial_guess, &feedback.get_per_character());
                            }

                            let next_guess = word_solver.generate_guess_by_words();
                            let root = DictionaryTreeNode::by_word(
                                word_solver.dictionary.iter().collect_vec(),
                                &next_guess,
                            );

                            let entropy = root.entropy();
                            eprintln!("[{thread_id}] => Entropy for chosen {chosen}, last guess {next_guess} = {entropy}");
                            results.insert(next_guess, entropy);
                        }
                        results
                    })
                })
                .collect_vec();

        let (best_word, entropy) = handles
            .into_iter()
            .map(|h| h.join().unwrap())
            .reduce(|mut acc, e| {
                e.into_iter().for_each(|(k, v)| {
                    acc.insert(k, v);
                });
                acc
            })
            .unwrap()
            .into_iter()
            .max_by_key(|(_word, entropy)| NotNaN::new(*entropy))
            .unwrap();

        eprintln!("Best next word is {best_word} with entropy of {entropy}");
    }

    #[test]
    fn test_all_words() {
        let words = read_words(FILE_PATH);
        let mut total_guesses = 0usize;
        let initial_words: VecDeque<_> = INITIAL_GUESSES
            .iter()
            .copied()
            .map(Word::from_str)
            .collect();

        for (idx, chosen) in words.iter().enumerate() {
            let chosen = Word::from_str(chosen);

            let mut n_tries = 0usize;
            let mut word_solver = WordSolver::new(words.clone());

            loop {
                let guess = word_solver.generate_guess_by_words();
                let feedback = Feedback::from_guess(&guess, &chosen);
                n_tries += 1;

                match feedback {
                    Feedback::Correct => break,
                    Feedback::PerCharacter(feedback) => {
                        word_solver.update(&guess, &feedback);
                    }
                    Feedback::Unknown => unreachable!()
                }
            }
            eprintln!("{chosen} guessed in {n_tries} tries");

            total_guesses += n_tries;
            eprintln!(
                "[{}/{}], avg: {}",
                idx + 1, words.len(),
                (total_guesses as f32) / ((idx + 1) as f32)
            )
        }
    }

    #[test]
    fn test_parallel() {
        let words = read_words(FILE_PATH);
        let chunk_size: usize = ((words.len() as f32) / (N_THREADS as f32)).ceil() as usize;
        let words_chunked = words
            .clone()
            .into_iter()
            .chunks(chunk_size)
            .into_iter()
            .map(|chunk| chunk.collect_vec())
            .collect_vec();

        let mut n_tries = Arc::new(Mutex::new(0usize));

        let handles =
            (1..=N_THREADS)
                .zip(words_chunked)
                .map(|(thread, words_chunk)| thread::spawn({
                    let words = words.clone();
                    let mut n_tries = Arc::clone(&n_tries);
                    move || {
                        let mut total_guesses = 0usize;

                        for (idx, chosen) in words_chunk.iter().enumerate() {
                            let chosen = Word::from_str(chosen);

                            let mut word_solver = WordSolver::new(words.clone());
                            let mut guesses_for_word = 0usize;

                            loop {
                                let guess = word_solver.generate_guess_by_words();
                                let feedback = Feedback::from_guess(&guess, &chosen);
                                guesses_for_word += 1;

                                match feedback {
                                    Feedback::Correct => break,
                                    Feedback::PerCharacter(feedback) => {
                                        word_solver.update(&guess, &feedback);
                                    }
                                    Feedback::Unknown => unreachable!()
                                }
                            }
                            eprintln!("[{thread}] => {chosen} guessed in {guesses_for_word} tries");

                            total_guesses += guesses_for_word;
                            eprintln!(
                                "[{thread}] => [{}/{}], avg: {}",
                                idx + 1, words_chunk.len(),
                                (total_guesses as f32) / ((idx + 1) as f32)
                            );
                        }
                        {
                            let mut mutex_guard = n_tries.lock().unwrap();
                            mutex_guard.add_assign(total_guesses);
                        }
                    }
                }))
                .collect_vec();


        for handle in handles {
            handle.join().unwrap();
        }
        let n_tries = n_tries.lock().unwrap();

        eprintln!("Total guesses: {:?}, average {}", n_tries, (*n_tries as f32) / (words.len() as f32))
    }

    #[test]
    fn test_best() {
        let words = read_words(FILE_PATH)
            .into_iter()
            .map(|word| Word::from_str(&word))
            .collect_vec();
        ["CARIES", "BONIER"].into_iter().map(Word::from_str).for_each(|best_word| {
            let root = DictionaryTreeNode::by_word(words.iter().collect_vec(), &best_word);
            let entropy = root.entropy();
            eprintln!("{best_word} => {entropy}");
        })
    }
}