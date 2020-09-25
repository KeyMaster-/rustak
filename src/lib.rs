use std::fmt;
use std::str::FromStr;
use nom::{
  IResult,
  InputLength,
  error::ParseError,
  character::complete::one_of,
  character::complete::anychar,
  combinator::map,
  combinator::map_opt,
  combinator::opt,
  combinator::all_consuming,
  combinator::verify,
  sequence::pair,
  sequence::tuple,
  branch::alt,
  multi::many1
};

fn finalise<I, O, EI: ParseError<I>, E, F>(f: F) -> impl Fn(I) -> Result<O, E>
where 
  I: InputLength,
  E: std::convert::From<nom::Err<EI>>,
  F: Fn(I) -> IResult<I, O, EI> + Copy // Not sure about the copy here, but it's required to move f into all_consuming
{
  move |input: I| {
    let (_, res) = all_consuming(f)(input)?;
    Ok(res)
  }
}

fn finalise_ignorant<I, O, EI: ParseError<I>, F>(f: F) -> impl Fn(I) -> Result<O, ()>
where
  I: InputLength,
  F: Fn(I) -> IResult<I, O, EI> + Copy
{
  move |input: I| {
    all_consuming(f)(input).map_or(Err(()), |(_, res)| Ok(res))
  }
}


const MAX_BOARD_SIZE: u8 = 8;
const FILE_CHARS: &str = "abcdefgh";
const RANK_CHARS: &str = "12345678";

// Location on the board
// x is the file, i.e. the lettered direction
// y is the rank, i.e. the numbered direction
// note that x and y are 0-indexed, while ranks are 1-indexed
#[derive(Debug)]
pub struct Location {
  pub x: u8, 
  pub y: u8
}

impl fmt::Display for Location {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.x >= MAX_BOARD_SIZE || self.y >= MAX_BOARD_SIZE {
      return Err(fmt::Error);
    }
    let file = FILE_CHARS.chars().nth(self.x as usize).unwrap();
    let rank = self.y + 1;
    write!(f, "{}{}", file, rank)
  }
}

impl Location {
  pub fn from_coords(x: u8, y: u8) -> Option<Self> {
    if x < MAX_BOARD_SIZE && y < MAX_BOARD_SIZE {
      Some(Self { x, y })
    } else {
      None
    }
  }
}

impl Location {
  fn parse(i: &str) -> IResult<&str, Location> {
    map(pair(one_of(FILE_CHARS), one_of(RANK_CHARS)), 
      |(file, rank)| Location { x: FILE_CHARS.find(file).unwrap() as u8, y: RANK_CHARS.find(rank).unwrap() as u8 }) (i)
  }
}

impl FromStr for Location {
  type Err = ();

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    finalise_ignorant(Location::parse)(s)
  }
}

#[derive(Debug)]
pub enum StoneKind {
  FlatStone,
  StandingStone,
  Capstone
}

impl fmt::Display for StoneKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", match self {
      StoneKind::FlatStone => 'F',
      StoneKind::StandingStone => 'S',
      StoneKind::Capstone => 'C'
    })
  }
}

impl StoneKind {
  fn parse(i: &str) -> IResult<&str, StoneKind> {
    map(one_of("FSC"), |c| match c {
      'F' => StoneKind::FlatStone,
      'S' => StoneKind::StandingStone,
      'C' => StoneKind::Capstone,
      _ => unreachable!()
    })(i)
  }
}

impl FromStr for StoneKind {
  type Err = ();

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    finalise_ignorant(StoneKind::parse)(s)
  }
}

#[derive(Debug)]
pub enum Direction {
  Up,
  Down,
  Left,
  Right
}

impl fmt::Display for Direction {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", match self {
      Direction::Up => '+',
      Direction::Down => '-',
      Direction::Left => '<',
      Direction::Right => '>'
    })
  }
}

impl Direction {
  fn parse(i: &str) -> IResult<&str, Direction> {
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
  type Err = ();

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    finalise_ignorant(Direction::parse)(s)
  }
}

#[derive(Debug)]
pub enum Move {
  Placement { kind: StoneKind, location: Location,  },
  Movement { start: Location, direction: Direction, drops: Vec<u8>}
}

impl fmt::Display for Move {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Move::Placement { location, kind } => {
        let stone_ident = match kind {
          StoneKind::FlatStone => "".to_string(),
          StoneKind::StandingStone | StoneKind::Capstone => kind.to_string()
        };
        write!(f, "{}{}", stone_ident, location)
      },
      Move::Movement { start, direction, drops } => {
        let pickup: u8 = drops.iter().sum();

        let omit_pickup = pickup == 1;
        let pickup_text = 
          if omit_pickup {
            String::from("")
          } else {
            format!("{}", pickup)
          };

        let omit_drops = drops.len() == 1;
        let drops_sequence = 
          if omit_drops {
            String::from("")
          } else {
            drops.iter().map(|x| x.to_string()).collect::<Vec<String>>().join("")
          };

        write!(f, "{}{}{}{}", pickup_text, start, direction, drops_sequence)
      }
    }
  }
}

fn take_digit(i: &str) -> IResult<&str, u8> {
  map(verify(anychar, char::is_ascii_digit), |c| c.to_digit(10).unwrap() as u8) (i)
}

impl Move {
  fn parse(i: &str) -> IResult<&str, Move> {
    let parse_placement = map(
      pair(opt(StoneKind::parse), Location::parse),
      |(kind, location)| Move::Placement { kind: kind.unwrap_or(StoneKind::FlatStone), location: location,  });

      // apply defaults for omitted values, then verify that pickup and sum of drops match
    let normalize_piece_amounts = |(pickup, location, direction, drops): (Option<u8>, _, _, Option<Vec<u8>>)| {
      let pickup = pickup.unwrap_or(1);
      let drops = drops.unwrap_or(vec![pickup]);
      
      if pickup == drops.iter().sum() {
        Some((location, direction, drops))
      } else {
        None
      }
    };

    let parse_movement = map(
      map_opt(tuple((opt(take_digit), Location::parse, Direction::parse, opt(many1(take_digit)))), normalize_piece_amounts),
      |(location, direction, drops)| Move::Movement { start: location, direction, drops }
    );

      // The order of these is important. 
      // A movement with no leading number starts with a location, which can be parsed as a complete placement.
      // So we need to first try to greedily parse the potentially longer movement, and then fall back to placement if that fails.
    alt((parse_movement, parse_placement))(i)
  }
}

impl FromStr for Move {
  type Err = ();

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    finalise_ignorant(Move::parse)(s)
  }
}