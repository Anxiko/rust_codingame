use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::io;
use std::str::FromStr;

use itertools::Itertools;

type Team = char;
type Coord = (usize, usize);
type Visitors = HashSet<Expedition>;

fn visitors_to_char(visitors: &Visitors) -> char {
	if visitors.len() > 1 {
		return '+';
	}

	if let Some(single_expedition) = visitors.iter().next() {
		return single_expedition.team;
	}

	panic!("Can't convert empty visitors to tile")
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
struct Expedition {
	team: Team,
	base: Coord,
}

#[derive(Debug, Clone)]
enum Tile {
	Unreachable,
	Reachable,
	Tower(Team),
	VisitedBy(Visitors),
}

impl Tile {
	fn free_to_visit(&self) -> bool {
		matches!(self, Self::Reachable)
	}
}

impl From<char> for Tile {
	fn from(value: char) -> Self {
		match value {
			'.' => Self::Reachable,
			'#' => Self::Unreachable,
			team => Self::Tower(team),
		}
	}
}

impl From<&Tile> for char {
	fn from(value: &Tile) -> Self {
		match value {
			Tile::Tower(team) => *team,
			Tile::VisitedBy(visitors) => visitors_to_char(&visitors),
			Tile::Unreachable => '#',
			Tile::Reachable => '.',
		}
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct ActiveVisitors {
	visitors: Visitors,
	coord: Coord,
}

impl ActiveVisitors {
	fn new(visitors: Visitors, coord: Coord) -> Self {
		Self { visitors, coord }
	}

	fn from_base(starting_expedition: Expedition) -> Self {
		Self::new(
			Visitors::from([starting_expedition]),
			starting_expedition.base,
		)
	}

	fn empty(coord: Coord) -> Self {
		Self::new(HashSet::new(), coord)
	}

	fn combine_with(&mut self, other_visitors: &Visitors) {
		self.visitors.extend(other_visitors)
	}

	fn to_tile(&self) -> Tile {
		Tile::VisitedBy(self.visitors.clone())
	}
}

fn parse_line(raw_line: &str) -> Vec<Tile> {
	raw_line.chars().map(|c| c.into()).collect()
}

fn get_line() -> String {
	let mut buffer = String::new();
	io::stdin().read_line(&mut buffer).expect("Read a line");
	buffer.trim_end().to_owned()
}

fn read<T: FromStr>() -> T {
	get_line().parse().ok().expect("Read and parse from stdin")
}

fn add_delta_to_coord((x, y): (usize, usize), (dx, dy): (isize, isize)) -> Option<(usize, usize)> {
	let new_x = x.checked_add_signed(dx)?;
	let new_y = y.checked_add_signed(dy)?;

	Some((new_x, new_y))
}

impl Expedition {
	fn new(team: char, base: Coord) -> Self {
		Self { team, base }
	}
}

struct TileMap {
	grid: Vec<Vec<Tile>>,
}

impl TileMap {
	fn new(grid: Vec<Vec<Tile>>) -> Self {
		Self { grid }
	}

	fn from_lines(width: usize, height: usize, lines: &Vec<&str>) -> Self {
		if lines.len() != height {
			panic!("Invalid amount of lines given, not matching height")
		}

		let grid = lines
			.into_iter()
			.map(|raw_row| {
				let row = parse_line(&raw_row);
				if row.len() != width {
					panic!("Read row with unexpected width");
				}
				row
			})
			.collect_vec();

		Self::new(grid)
	}

	fn from_stdin(width: usize, height: usize) -> Self {
		let grid = (0..height)
			.map(|_| {
				let raw_row = get_line();
				let row = parse_line(&raw_row);
				if row.len() != width {
					panic!("Read row with unexpected width");
				}
				row
			})
			.collect_vec();

		Self::new(grid)
	}

	fn read(&self, (x, y): (usize, usize)) -> Option<Tile> {
		self.grid.get(y).and_then(|row| row.get(x)).cloned()
	}

	fn write(&mut self, (x, y): (usize, usize), t: Tile) {
		let tile = self
			.grid
			.get_mut(y)
			.expect("Valid row for writing")
			.get_mut(x)
			.expect("valid column writing");
		*tile = t;
	}

	fn neighbour_coords(&self, coord: (usize, usize)) -> impl Iterator<Item = (usize, usize)> {
		(-1isize..=1)
			.into_iter()
			.cartesian_product((-1isize..=1).into_iter())
			.filter(|&(dx, dy)| dx == 0 || dy == 0)
			.flat_map(move |delta| add_delta_to_coord(coord, delta))
	}

	fn free_neighbours(&self, coord: (usize, usize)) -> impl Iterator<Item = Coord> + '_ {
		self.neighbour_coords(coord).filter(move |&neighbour| {
			self.read(neighbour)
				.is_some_and(|tile| tile.free_to_visit())
		})
	}

	fn towers(&self) -> impl Iterator<Item = Expedition> + '_ {
		self.grid
			.iter()
			.enumerate()
			.flat_map(|(y, row)| row.iter().enumerate().map(move |(x, tile)| ((x, y), tile)))
			.filter_map(|(coord, tile)| match tile {
				Tile::Tower(team) => Some(Expedition::new(*team, coord)),
				_ => None,
			})
	}

	fn output_lines(&self) -> impl Iterator<Item = String> + '_ {
		self.grid
			.iter()
			.map(|row| row.iter().map(|tile| char::from(tile)).collect())
	}
}

impl Display for TileMap {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
		for line in self.output_lines() {
			write!(f, "{line}\n")?;
		}
		Ok(())
	}
}

fn solve(tile_map: &mut TileMap) {
	let mut active_visitors = tile_map
		.towers()
		.map(|expedition| ActiveVisitors::from_base(expedition))
		.collect_vec();

	while !active_visitors.is_empty() {
		let mut new_active_visitors: HashMap<Coord, ActiveVisitors> = HashMap::new();
		for active_visitor in active_visitors {
			for free_neighbour in tile_map.free_neighbours(active_visitor.coord) {
				new_active_visitors
					.entry(free_neighbour)
					.or_insert_with(|| ActiveVisitors::empty(free_neighbour))
					.combine_with(&active_visitor.visitors);
			}
		}

		new_active_visitors
			.iter()
			.for_each(|(&coord, active_visitors)| {
				tile_map.write(coord, active_visitors.to_tile());
			});

		active_visitors = new_active_visitors.values().cloned().collect_vec();
	}
}

#[cfg(test)]
mod test {
	use crate::{solve, TileMap};

	#[test]
	fn case1() {
		let width = 10;
		let height = 10;
		let lines = vec![
			"..#.#...##",
			"#..#.#....",
			".........#",
			"..#..#..#.",
			".......#..",
			"..#.JEDI.#",
			"..#.....#.",
			".....#..#.",
			"..........",
			"..........",
		];

		let mut tile_map = TileMap::from_lines(width, height, &lines);
		solve(&mut tile_map);

		let expected = "\
JJ#.#DDD##
#JJ#J#DDDD
JJJJJ+DDD#
JJ#JJ#DD#I
JJJJJED#II
JJ#JJEDII#
JJ#JJEDI#I
JJJJJ#DI#I
JJJJJ+DIII
JJJJJ+DIII
";
		assert_eq!(format!("{tile_map}"), expected);
	}
}

fn main() {
	let width: usize = read();
	let height: usize = read();

	let mut tile_map = TileMap::from_stdin(width, height);

	solve(&mut tile_map);
	print!("{tile_map}")
}
