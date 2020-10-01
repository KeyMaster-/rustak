use crate::{
  Location,
  StoneKind,
  Direction,
  Move,
  FILE_CHARS,
  RANK_CHARS,
  utils::nom::{
    Input as NomInput, 
    Result as NomResult, 
    digit_nonzero_u8,
    finalise}
};
use nom::{
  character::complete::{one_of},
  combinator::{map, map_opt, opt},
  sequence::{pair, tuple},
  branch::alt,
  multi::many1
};
use std::str::FromStr;
use std::result::Result as StdResult;

pub type Input<'a> = NomInput<'a>;
pub type Result<'a, T> = NomResult<'a, T, ()>;

impl Location {
  fn parse(i: Input) -> Result<Location> {
    map(pair(one_of(FILE_CHARS), one_of(RANK_CHARS)), 
      |(file, rank)| Location { x: FILE_CHARS.find(file).unwrap() as usize, y: RANK_CHARS.find(rank).unwrap() as usize }) (i)
  }
}

impl FromStr for Location {
  type Err = nom::Err<()>;

  fn from_str(s: &str) -> StdResult<Self, Self::Err> {
    finalise(Location::parse, s)
  }
}


impl StoneKind {
  fn parse(i: Input) -> Result<StoneKind> {
    map(one_of("FSC"), |c| match c {
      'F' => StoneKind::FlatStone,
      'S' => StoneKind::StandingStone,
      'C' => StoneKind::Capstone,
      _ => unreachable!()
    })(i)
  }
}

impl FromStr for StoneKind {
  type Err = nom::Err<()>;

  fn from_str(s: &str) -> StdResult<Self, Self::Err> {
    finalise(StoneKind::parse, s)
  }
}

impl Direction {
  fn parse(i: Input) -> Result<Direction> {
    map(one_of("+-<>"), |c| match c {
      '+' => Direction::Up,
      '-' => Direction::Down,
      '<' => Direction::Left,
      '>' => Direction::Right,
      _ => unreachable!()
    })(i)
  }
}

impl FromStr for Direction {
  type Err = nom::Err<()>;

  fn from_str(s: &str) -> StdResult<Self, Self::Err> {
    finalise(Direction::parse, s)
  }
}

impl Move {
  fn parse(i: Input) -> Result<Move> {
    let parse_placement = map(
      pair(opt(StoneKind::parse), Location::parse),
      |(kind, location)| Move::Placement { kind: kind.unwrap_or(StoneKind::FlatStone), location: location,  });

      // apply defaults for omitted values, then verify that pickup and sum of drops match
    let normalize_piece_amounts = |(pickup, location, direction, drops): (Option<u8>, _, _, Option<Vec<u8>>)| {
      let pickup = pickup.unwrap_or(1);
      let drops = drops.unwrap_or(vec![pickup]);
      
      if pickup == drops.iter().sum() {
        Some((location, direction, drops.iter().map(|&d| d as usize).collect()))
      } else {
        None
      }
    };

    let parse_movement = map(
      map_opt(tuple((opt(digit_nonzero_u8), Location::parse, Direction::parse, opt(many1(digit_nonzero_u8)))), normalize_piece_amounts),
      |(location, direction, drops)| Move::Movement { start: location, direction, drops }
    );

      // The order of these is important. 
      // A movement with no leading number starts with a location, which can be parsed as a complete placement.
      // So we need to first try to greedily parse the potentially longer movement, and then fall back to placement if that fails.
    alt((parse_movement, parse_placement))(i)
  }
}

impl FromStr for Move {
  type Err = nom::Err<()>;

  fn from_str(s: &str) -> StdResult<Self, Self::Err> {
    finalise(Move::parse, s)
  }
}