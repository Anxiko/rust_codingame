use itertools::Itertools;
use std::collections::HashMap;
use std::fmt::Debug;
use std::io;
use std::str::FromStr;

const BASE_DAMAGE: u32 = 10;
const MAX_HEALTH: u32 = 5_000;

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
enum ShipType {
	Fighter,
	Cruiser,
}

impl ShipType {
	fn multiplier(&self) -> u32 {
		match self {
			ShipType::Cruiser => 1,
			ShipType::Fighter => 2,
		}
	}
}

impl FromStr for ShipType {
	type Err = ();

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		match s {
			"FIGHTER" => Ok(Self::Fighter),
			"CRUISER" => Ok(Self::Cruiser),
			_ => Err(()),
		}
	}
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
struct Ship {
	ship_type: ShipType,
	hp: u32,
	armor: u32,
	damage: u32,
	active: bool,
}

impl Ship {
	fn new(ship_type: ShipType, hp: u32, armor: u32, damage: u32, active: bool) -> Self {
		Self {
			ship_type,
			hp,
			armor,
			damage,
			active,
		}
	}

	fn damage_dealt(&self) -> u32 {
		if self.active {
			self.damage
		} else {
			0
		}
	}

	fn damage_taken(&self, damage: u32) -> u32 {
		let actual_damage = damage * self.ship_type.multiplier();
		match actual_damage.checked_sub(self.armor) {
			Some(0) | None => 1,
			Some(damage) => damage,
		}
	}

	fn turns_to_die(&self, damage: u32) -> u32 {
		let actual_damage = self.damage_taken(damage);
		self.hp / actual_damage + {
			if self.hp % actual_damage > 0 {
				1
			} else {
				0
			}
		}
	}
}

impl FromStr for Ship {
	type Err = ();

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		match s.split_ascii_whitespace().collect_vec().as_slice() {
			[ship_type, hp, armor, damage] => {
				let ship_type = ShipType::from_str(ship_type)?;
				let hp = u32::from_str(hp).map_err(|_err| ())?;
				let armor = u32::from_str(armor).map_err(|_err| ())?;
				let damage = u32::from_str(damage).map_err(|_err| ())?;
				Ok(Self::new(ship_type, hp, armor, damage, true))
			}
			_ => Err(()),
		}
	}
}

fn read_line() -> String {
	let mut buffer = String::new();
	io::stdin().read_line(&mut buffer).expect("Read a line");
	buffer
}

fn parse_input<T: FromStr>() -> T
where
	<T as FromStr>::Err: Debug,
{
	let line = read_line();
	line.trim().parse::<T>().expect("Parse input")
}

fn min_damage_tanked(
	damage_taken: u32,
	ships: &Vec<Ship>,
	memo: &mut HashMap<Vec<Ship>, Result<u32, ()>>,
) -> Result<u32, ()> {
	if damage_taken >= MAX_HEALTH {
		return Err(());
	}

	if ships.iter().all(|ship| !ship.active) {
		return Ok(damage_taken);
	}

	if let Some(result) = memo.get(ships) {
		return *result;
	}

	let damage_per_turn = ships.iter().map(|ship| ship.damage_dealt()).sum::<u32>();

	let result = ships
		.iter()
		.enumerate()
		.filter(|(_idx, ship)| ship.active)
		.flat_map(|(idx, ship)| {
			let turns = ship.turns_to_die(BASE_DAMAGE);
			let damage_taken_before_kill = turns * damage_per_turn;
			let mut ships_copy = ships.clone();
			ships_copy[idx].active = false;

			min_damage_tanked(damage_taken + damage_taken_before_kill, &ships_copy, memo)
		})
		.min()
		.ok_or(());

	memo.insert(ships.clone(), result);

	result
}

fn main() {
	let n: i32 = parse_input();

	let ships: Vec<_> = (0..n)
		.map(|_idx| {
			let buffer = read_line();
			Ship::from_str(&buffer)
		})
		.try_collect()
		.expect("Parse ships");

	let min_damage = min_damage_tanked(0, &ships, &mut HashMap::new());
	if let Some(remaining) = min_damage
		.ok()
		.and_then(|min_damage| MAX_HEALTH.checked_sub(min_damage))
	{
		println!("{remaining}");
	} else {
		println!("FLEE");
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn strongest_first() {
		let ships = "\
FIGHTER 10 0 500
FIGHTER 10 0 800
"
		.lines()
		.map(|line| Ship::from_str(line).unwrap())
		.collect_vec();

		assert_eq!(
			MAX_HEALTH - min_damage_tanked(0, &ships, &mut HashMap::new()).unwrap(),
			3200
		);
	}

	#[test]
	fn the_swarm() {
		let ships = "\
FIGHTER 26 6 1
CRUISER 14 3 3
CRUISER 25 6 4
CRUISER 13 5 4
CRUISER 19 7 3
FIGHTER 12 7 2
FIGHTER 21 0 2
FIGHTER 15 3 3
FIGHTER 26 1 1
CRUISER 21 4 3
FIGHTER 18 5 3
FIGHTER 30 0 2
CRUISER 20 8 3
FIGHTER 22 3 4
FIGHTER 6 6 3
CRUISER 13 5 2
CRUISER 11 2 3
CRUISER 14 1 4
FIGHTER 9 6 3
CRUISER 14 2 4
CRUISER 5 2 1
FIGHTER 9 3 2
FIGHTER 6 0 3
FIGHTER 13 0 1
CRUISER 19 1 1
FIGHTER 15 3 4
FIGHTER 8 1 4
CRUISER 9 7 1
CRUISER 14 5 2
FIGHTER 19 4 3
FIGHTER 15 0 1
FIGHTER 8 7 3
FIGHTER 26 0 4
FIGHTER 21 3 1
CRUISER 10 2 3
FIGHTER 20 6 2
FIGHTER 28 1 1
CRUISER 19 8 4
FIGHTER 14 8 3
CRUISER 24 1 4
CRUISER 17 4 2
FIGHTER 20 1 1
FIGHTER 23 4 3
FIGHTER 18 3 2
FIGHTER 17 3 4
CRUISER 5 3 4
CRUISER 18 2 2
FIGHTER 11 0 1
CRUISER 27 1 2
FIGHTER 19 0 3
"
		.lines()
		.map(|line| Ship::from_str(line).unwrap())
		.collect_vec();

		assert_eq!(
			MAX_HEALTH - min_damage_tanked(0, &ships, &mut HashMap::new()).unwrap(),
			212
		);
	}
}
