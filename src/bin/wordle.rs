// https://www.codingame.com/multiplayer/optimization/wordle

use std::cmp::Ordering;
use std::collections::HashSet;
use std::convert::TryInto;
use std::io;
use std::str::FromStr;

use itertools::Itertools;


struct NotNaN {
    value: f32,
}

impl NotNaN {
    fn new(value: f32) -> Result<Self, ()> {
        if value.is_nan() {
            Err(())
        } else {
            Ok(Self{value})
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

#[derive(Debug, Copy, Clone)]
enum CharacterFeedback {
    NotPresent,
    PresentMisplaced,
    Correct,
    Unknown,
}

impl FromStr for CharacterFeedback {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "0" => Ok(Self::Unknown),
            "1" => Ok(Self::NotPresent),
            "2" => Ok(Self::PresentMisplaced),
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
            CharacterFeedback::PresentMisplaced => Self::ContainsCharacterElsewhere { idx, chr },
            CharacterFeedback::NotPresent => Self::NotContainsCharacter { chr }
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
    fn from_guess_feedback(guess: &str, feedback: &[CharacterFeedback; CHARACTERS_PER_WORD]) -> Self {
        let rules: Vec<_> =
            guess
                .chars()
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

    fn generate_guess(&self) -> String {
        if self.dictionary.len() <= 2 {
            let word = self.dictionary.first().expect("Not empty dictionary");
            return word.into();
        }

        let word = DictionaryTreeNode::from_words(
            self.dictionary.iter().collect_vec()
        ).get_guess();

        String::from(&word)
    }

    fn update(&mut self, guess: &str, feedback: &[CharacterFeedback; CHARACTERS_PER_WORD]) {
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
}

impl From<&Word> for String {
    fn from(value: &Word) -> Self {
        value.iter().collect()
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
        .map(|p| p * p.log2())
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

    fn partition(words: Vec<&'a Word>, idx: usize, open_characters: &HashSet<char>) -> DictionaryTreeNode<'a> {
        if idx == CHARACTERS_PER_WORD {
            DictionaryTreeNode::Leaf(words)
        } else {
            open_characters
                .iter()
                .copied()
                .map(|chr| {
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

    loop {
        let guess = word_solver.generate_guess();
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
