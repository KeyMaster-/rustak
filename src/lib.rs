use std::fmt;
use std::str::FromStr;
use nom::{
  IResult,
  InputLength,
  error::ParseError,
  character::complete::one_of,
  combinator::map_opt,
  combinator::all_consuming,
  sequence::pair
};

fn finalise<I, O, EI: ParseError<I>, E, F>(f: F) -> impl Fn(I) -> Result<O, E>
where 
  I: InputLength,
  E: std::convert::From<nom::Err<EI>>,
  F: Fn(I) -> IResult<I, O, EI> + Copy
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
const FILE_CHARS: [char; MAX_BOARD_SIZE as usize] = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'];
const RANK_CHARS: [char; MAX_BOARD_SIZE as usize] = ['1', '2', '3', '4', '5', '6', '7', '8'];
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
    let file = FILE_CHARS[self.x as usize];
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

  fn parse(i: &str) -> IResult<&str, Location> {
    map_opt(pair(one_of("abcdefgh"), one_of("12345678")), 
      |(file, rank)| Location::from_coords("abcdefgh".find(file).unwrap() as u8, "12345678".find(rank).unwrap() as u8)) (i)
  }
}

#[derive(Debug)]
pub enum ParseLocationError {
  BadFormat,
}

impl FromStr for Location {
  type Err = ();

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    // let chars: Vec<char> = s.chars().collect();

    // if chars.len() != 2 {
    //   return Err(())
    // }

    // let x = FILE_CHARS.iter().position(|&x| x == chars[0]).ok_or(())?;
    // let y = RANK_CHARS.iter().position(|&x| x == chars[1]).ok_or(())?;

    // Ok(Location {
    //   x: x as u8,
    //   y: y as u8
    // })

    // need to map Result<(remaining, output), Err> to Result<output, err>, also make sure that nom incomplete becomes an error (has the `complete` combinator)
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

impl FromStr for StoneKind {
  type Err = ();

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "F" => Ok(StoneKind::FlatStone),
      "S" => Ok(StoneKind::StandingStone),
      "C" => Ok(StoneKind::StandingStone),
      _ => Err(())
    }
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

impl FromStr for Direction {
  type Err = ();

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "+" => Ok(Direction::Up),
      "-" => Ok(Direction::Down),
      "<" => Ok(Direction::Left),
      ">" => Ok(Direction::Right),
      _ => Err(())
    }
  }
}

#[derive(Debug)]
pub enum Move {
  Placement { location: Location, kind: StoneKind },
  Movement { start: Location, direction: Direction, carry: u8, drop: Vec<u8>}
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
      Move::Movement { start, direction, carry, drop } => {
        let omit_carry = *carry == 1;
        let carry_text = 
          if omit_carry {
            String::from("")
          } else {
            format!("{}", carry)
          };

        let omit_drop = drop.len() == 1;
        let drop_sequence = 
          if omit_drop {
            String::from("")
          } else {
            drop.iter().map(|x| x.to_string()).collect::<Vec<String>>().join("")
          };

        write!(f, "{}{}{}{}", carry_text, start, direction, drop_sequence)
      }
    }
  }
}

impl FromStr for Move {
  type Err = ();

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    if !s.is_ascii() {
      return Err(())
    }

    let carry = s[0..1].parse::<u8>();
    let offset = if carry.is_ok() { 1 } else { 0 };

    let start = Location::from_str(&s[offset..offset+2])?;
    let direction = Direction::from_str(&s[offset+2..offset+3])?;
    let drop_sequence = &s[offset+3..];
    if drop_sequence.chars().any(|c| !c.is_digit(10)) {
      return Err(());
    }
    let drop: Vec<u8> = s[offset+3..].chars().map(|c| c.to_digit(10).unwrap() as u8).collect();

    // Ok(Move {

    // })

    Err(())

  }
}