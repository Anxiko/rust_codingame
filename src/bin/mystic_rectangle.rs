// https://www.codingame.com/ide/puzzle/the-mystic-rectangle

use itertools::Itertools;
use std::cmp::{max, min};
use std::fmt::{Display, Formatter};
use std::ops::{Add, Mul, Sub};
use std::{io, iter};

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
struct Vct<T>
where
	T: Clone + Copy,
{
	x: T,
	y: T,
}

impl<T> Vct<T>
where
	T: Copy + Clone,
{
	fn new(x: T, y: T) -> Self {
		Self { x, y }
	}
}

impl<T> Add for Vct<T>
where
	T: Copy + Clone + Add<Output=T>,
{
	type Output = Vct<T>;

	fn add(self, rhs: Self) -> Self::Output {
		Vct::new(self.x + rhs.x, self.y + rhs.y)
	}
}

impl<T> Sub for Vct<T>
where
	T: Copy + Clone + Sub<Output=T>,
{
	type Output = Vct<T>;

	fn sub(self, rhs: Self) -> Self::Output {
		Vct::new(self.x - rhs.x, self.y - rhs.y)
	}
}

impl<T> Mul for Vct<T>
where
	T: Copy + Clone + Mul<Output=T>,
{
	type Output = Vct<T>;

	fn mul(self, rhs: Self) -> Self::Output {
		Vct::new(self.x * rhs.x, self.y * rhs.y)
	}
}

impl<T> Default for Vct<T>
where
	T: Copy + Clone + Default,
{
	fn default() -> Self {
		Vct::new(T::default(), T::default())
	}
}

impl<T> From<(T, T)> for Vct<T>
where
	T: Copy + Clone,
{
	fn from((x, y): (T, T)) -> Self {
		Vct::new(x, y)
	}
}

impl<T> Display for Vct<T>
where
	T: Copy + Clone + Display,
{
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		write!(f, "({}, {})", self.x, self.y)
	}
}

type IntVct = Vct<i32>;

fn possible_targets(target: IntVct, limits: IntVct) -> impl Iterator<Item=IntVct> {
	(-1..=1)
		.cartesian_product(-1..=1)
		.map(IntVct::from)
		.chain(iter::once(Vct::default()))
		.map(move |offset| offset * limits)
		.map(move |offset| offset + target)
}

const HORIZONTAL_COST: f32 = 0.3;
const VERTICAL_COST: f32 = 0.4;
const DIAGONAL_COST: f32 = 0.5;
const WIDTH: u32 = 200;
const HEIGHT: u32 = 150;
const LIMITS: IntVct = IntVct { x: WIDTH as i32, y: HEIGHT as i32 };

fn clamp_magnitude(value: i32, magnitude: u32) -> i32 {
	let magnitude: i32 = magnitude as i32;
	if value.is_negative() {
		max(value, -magnitude)
	} else {
		min(value, magnitude)
	}
}

fn clamp_vector(vct: IntVct, magnitude: u32) -> IntVct {
	IntVct::new(clamp_magnitude(vct.x, magnitude), clamp_magnitude(vct.y, magnitude))
}

fn move_diagonal(pos: IntVct, target: IntVct) -> (IntVct, u32) {
	let delta = target - pos;
	let max_diagonal = min(delta.x.unsigned_abs(), delta.y.unsigned_abs());
	let clamped_delta = clamp_vector(delta, max_diagonal);

	(pos + clamped_delta, max_diagonal)
}

fn move_cross(pos: IntVct, target: IntVct) -> (u32, u32) {
	let delta = target - pos;
	(delta.x.unsigned_abs(), delta.y.unsigned_abs())
}

fn distance_to_target(pos: IntVct, target: IntVct) -> f32 {
	let (pos, diagonal_movement) = move_diagonal(pos, target);
	eprintln!("New pos: {pos}");
	let (horizontal_movement, vertical_movement) = move_cross(pos, target);

	eprintln!("{pos} => {target}: (hor: {horizontal_movement}, ver: {vertical_movement}, diag: {diagonal_movement})");

	DIAGONAL_COST * (diagonal_movement as f32)
		+ HORIZONTAL_COST * (horizontal_movement as f32)
		+ VERTICAL_COST * (vertical_movement as f32)
}

macro_rules! parse_input {
    ($x:expr, $t:ident) => ($x.trim().parse::<$t>().unwrap())
}

fn solve(pos: IntVct, target: IntVct) -> f32 {
	possible_targets(target, LIMITS)
		.map(|t| distance_to_target(pos, t))
		.min_by(|a, b| a.partial_cmp(b).unwrap())
		.unwrap()
}

fn format_cost(cost: f32) -> String {
	format!("{cost:?}")
}

fn main() {
	let mut input_line = String::new();
	io::stdin().read_line(&mut input_line).unwrap();
	let inputs = input_line.split(" ").collect::<Vec<_>>();
	let x = parse_input!(inputs[0], i32);
	let y = parse_input!(inputs[1], i32);
	let mut input_line = String::new();
	io::stdin().read_line(&mut input_line).unwrap();
	let inputs = input_line.split(" ").collect::<Vec<_>>();
	let u = parse_input!(inputs[0], i32);
	let v = parse_input!(inputs[1], i32);

	let pos = IntVct::new(x, y);
	let target = IntVct::new(u, v);

	let cost = solve(pos, target);

	println!("{}", format_cost(cost));
}

#[cfg(test)]
mod tests {
	use crate::{distance_to_target, format_cost, possible_targets, solve, IntVct, LIMITS};
	use itertools::Itertools;

	#[test]
	fn all_targets() {
		let targets = possible_targets(IntVct::new(65, 145), LIMITS).collect_vec();
		eprintln!("{targets:?}");
	}

	#[test]
	fn distance() {
		let pos = IntVct::new(50, 15);
		let target = IntVct::new(65, -5);

		let actual = distance_to_target(pos, target);

		assert_eq!(actual, 9.5f32);
	}

	#[test]
	fn formatted_cost() {
		assert_eq!(format_cost(5.0f32), "5.0".to_owned());
	}

	#[test]
	fn example() {
		let pos = IntVct::new(50, 15);
		let target = IntVct::new(65, -5);

		let expected = 9.5f32;
		let actual = solve(pos, target);

		assert_eq!(actual, expected);
	}
}