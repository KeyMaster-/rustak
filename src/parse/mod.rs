use crate::{
  Location, FILE_CHARS, RANK_CHARS,
  Direction, Move,
  Color, StoneKind, Stone, StoneStack, Game
};

use utils::{ digit_nonzero_u8, u32, finalise };

use nom::{
  IResult,
  error::{ErrorKind as NomErrorKind, ParseError, FromExternalError, ContextError},
  combinator::{map, map_res, map_opt, opt},
  sequence::{pair, tuple},
  branch::alt,
  multi::{many0, many1},
  character::complete::{one_of, space0, multispace0, char},
};
use std::str::FromStr;
use std::result;


pub type Input<'a> = &'a str;
pub type Error<'a> = VerboseError<Input<'a>>;
pub type Result<'a, T> = IResult<Input<'a>, T, VerboseError<Input<'a>>>;

impl Location {
  fn parse(i: Input) -> Result<Location> {
    map(pair(one_of(FILE_CHARS), one_of(RANK_CHARS)), 
      |(file, rank)| Location { x: FILE_CHARS.find(file).unwrap() as usize, y: RANK_CHARS.find(rank).unwrap() as usize }) (i)
  }
}

impl FromStr for Location {
  type Err = ();

  fn from_str(s: &str) -> result::Result<Self, Self::Err> {
      //TODO maybe convert the nom::Err<Error> here into an error with no lifetimes, so it can be propagated out of this function
      // similarly for all other fromstr implementations here
    finalise(Location::parse, s).map_err(|_: nom::Err<_>| ()) 
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

impl<'a> FromStr for StoneKind {
  type Err = ();

  fn from_str(s: &str) -> result::Result<Self, Self::Err> {
    finalise(StoneKind::parse, s).map_err(|_: nom::Err<_>| ())
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

impl<'a> FromStr for Direction {
  type Err = ();

  fn from_str(s: &str) -> result::Result<Self, Self::Err> {
    finalise(Direction::parse, s).map_err(|_: nom::Err<_>| ())
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
  type Err = ();

  fn from_str(s: &str) -> result::Result<Self, Self::Err> {
    finalise(Move::parse, s).map_err(|_: nom::Err<_>| ())
  }
}

fn parse_color(i : Input) -> Result<Color> {
  map(one_of("wWbB"), |c| match c {
    'w'|'W' => Color::White,
    'b'|'B' => Color::Black,
     _  => unreachable!()
  })(i)
}

fn parse_stone_kind(i: Input) -> Result<StoneKind> {
  map(one_of("fFsScC"), |c| match c {
    'f'|'F' => StoneKind::FlatStone,
    's'|'S' => StoneKind::StandingStone,
    'c'|'C' => StoneKind::Capstone,
     _  => unreachable!()
  })(i)
}

fn parse_stone(i: Input) -> Result<Stone> {
  map(pair(parse_color, parse_stone_kind), |(color, kind)| Stone::new(kind, color))(i)
}

fn parse_stack(i: Input) -> Result<StoneStack> { 
  map_res(
    many0(parse_stone), |stones| StoneStack::multiple(stones)
  )(i)
}

fn parse_row(i: Input) -> Result<Vec<StoneStack>> {
  map(
    tuple((
      multispace0, 
      char('|'), 
      many1(map(tuple((space0, parse_stack, space0, char('|'))), |(_, stack, _, _)| stack)),
      multispace0)), 
    |(_, _, stacks, _)| stacks)(i)
}

impl Game {
  pub fn parse_state(i: Input) -> Result<Game> {
    map_res(
      tuple((
        many1(parse_row),
        u32
      )),
    |(rows, size)| {
      let stacks = rows.into_iter().rev().flatten().collect();
      Game::from_data(stacks, size)
    })(i)
  }
}


#[derive(Debug)]
enum ErrorKind {
  Nom(NomErrorKind),
  Context(&'static str),
  External(Box<dyn std::error::Error>, NomErrorKind) // the external error, and the wrapping parser's type
}

pub struct VerboseError<I> {
  errors: Vec<(I, ErrorKind)>
}

impl<I> ParseError<I> for VerboseError<I> {
  fn from_error_kind(input: I, kind: NomErrorKind) -> Self {
     let errors = vec![(input, ErrorKind::Nom(kind))];
     Self { errors }
   }

   fn append(input: I, kind: NomErrorKind, mut other: Self) -> Self {
     // was (input, kind)
     other.errors.push((input, ErrorKind::Nom(kind)));
     other
   }
}

impl<I> ContextError<I> for VerboseError<I> {
  fn add_context(input: I, ctx: &'static str, mut other: Self) -> Self {
    other.errors.push((input, ErrorKind::Context(ctx)));
    other
  }
}

impl<I, E: 'static + std::error::Error> FromExternalError<I, E> for VerboseError<I> {
  fn from_external_error(input: I, kind: NomErrorKind, e: E) -> Self {
    let errors = vec![(input, ErrorKind::External(Box::new(e), kind))];
    Self { errors }
  }
}

impl<I: std::fmt::Display> std::fmt::Debug for VerboseError<I> {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    writeln!(f, "Parsing failed:")?;
    for (input, kind) in self.errors.iter().rev() {
      match kind {
        ErrorKind::Nom(nomkind) => writeln!(f, "Nom parser {:?}", nomkind)?,
        ErrorKind::Context(msg) => writeln!(f, "Context: {}", msg)?,
        ErrorKind::External(err, nomkind) => writeln!(f, "External: {} (in parser {:?})", err, nomkind)?
      };
      writeln!(f, "{}", input)?;
    }
    Ok(())
  }
}

pub mod utils {
  use super::Input;

  use ::nom::{
    IResult, InputLength,
    error::{ParseError, FromExternalError},
    combinator::{map, map_res, all_consuming, verify},
    character::complete::{one_of, digit1}
  };

  pub fn finalise<I, O, EI: ParseError<I>, E, F>(f: F, input: I) -> std::result::Result<O, E>
  where 
    I: InputLength,
    E: std::convert::From<nom::Err<EI>>,
    F: FnMut(I) -> IResult<I, O, EI>
  {
    let (_, res) = all_consuming(f)(input)?;
    Ok(res)
  }

  pub fn digit<'a, E>(i: Input<'a>) -> IResult<Input<'a>, char, E> 
    where E: ParseError<Input<'a>>
  {
    one_of("0123456789")(i)
  }

  pub fn digit_nonzero<'a, E>(i: Input<'a>) -> IResult<Input<'a>, char, E>
    where E: ParseError<Input<'a>>
  {
    verify(digit, |&d| d != '0')(i)
  }

  pub fn digit_u8<'a, E>(i: Input<'a>) -> IResult<Input<'a>, u8, E>
    where E: ParseError<Input<'a>>
  {
    map(digit, |c| c.to_digit(10).unwrap() as u8)(i)
  }

  pub fn digit_nonzero_u8<'a, E>(i: Input<'a>) -> IResult<Input<'a>, u8, E>
    where E: ParseError<Input<'a>>
  {
    map(digit_nonzero, |c| c.to_digit(10).unwrap() as u8)(i)
  }
  
  pub fn u32<'a, E>(i: Input<'a>) -> IResult<Input<'a>, u32, E> 
    where E: ParseError<Input<'a>> + FromExternalError<Input<'a>, std::num::ParseIntError>
  {
    map_res(digit1, |sub: Input| sub.parse::<u32>())(i)
  }
}