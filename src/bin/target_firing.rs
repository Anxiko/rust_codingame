use itertools::Itertools;
use std::collections::HashMap;
use std::fmt::Debug;
use std::io;
use std::str::FromStr;

const BASE_DAMAGE: u32 = 10;
const MAX_HEALTH: u32 = 5_000;

#[derive(Eq, PartialEq, Hash, Clone)]
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

#[derive(Eq, PartialEq, Hash, Clone)]
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

fn min_damage_tanked(ships: Vec<Ship>, memo: &mut HashMap<Vec<Ship>, u32>) -> u32 {
    if let Some(result) = memo.get(&ships) {
        return *result;
    }

    let damage_per_turn = ships.iter().map(|ship| ship.damage_dealt()).sum::<u32>();

    let result = ships
        .iter()
        .enumerate()
        .filter(|(_idx, ship)| ship.active)
        .map(|(idx, ship)| {
            let turns = ship.turns_to_die(BASE_DAMAGE);
            let damage_taken_before_kill = turns * damage_per_turn;
            let mut ships_copy = ships.clone();
            ships_copy[idx].active = false;
            damage_taken_before_kill + min_damage_tanked(ships_copy, memo)
        })
        .min()
        .unwrap_or(0);

    memo.insert(ships, result);

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

    let min_damage = min_damage_tanked(ships, &mut HashMap::new());
    if min_damage >= MAX_HEALTH {
        println!("FLEE")
    } else {
        println!("{min_damage}")
    }
}
