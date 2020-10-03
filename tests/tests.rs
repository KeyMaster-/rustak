use tak::*;
use nom::{
  error::ErrorKind as NomErrorKind,
  error::{ParseError, FromExternalError},
  character::complete::{one_of, space0, multispace0, char},
  combinator::{map, map_res},
  sequence::{pair, tuple},
  multi::{many0, many1}
};

use tak::utils::nom::{
  Input,
  Result as NomResult,
  Input as NomInput,
  u32,
  finalise
};
use std::boxed::Box;

use thiserror::Error;

#[derive(Debug)]
enum ErrorKind {
  Nom(NomErrorKind),
  External(Box<dyn std::error::Error>, NomErrorKind) // the external error, and the wrapping parser's type
}

// type ParseError<'a> = nom::error::VerboseError<NomInput<'a>>;
struct GameParseError<I> {
  errors: Vec<(I, ErrorKind)>
}

impl<I> ParseError<I> for GameParseError<I> {
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

impl<I: Copy, E: 'static + std::error::Error> FromExternalError<I, E> for GameParseError<I> {
  fn from_external_error(input: I, kind: NomErrorKind, e: E) -> Self {
    let errors = vec![(input, ErrorKind::External(Box::new(e), kind))];
    Self { errors }
  }
}

impl<I: std::fmt::Display> std::fmt::Debug for GameParseError<I> {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    writeln!(f, "Failed to parse game state")?;
    for (input, kind) in self.errors.iter().rev() {
      match kind {
        ErrorKind::Nom(nomkind) => writeln!(f, "Nom parser {:?}", nomkind)?,
        ErrorKind::External(err, nomkind) => writeln!(f, "External: {:?} (in parser {:?})", err, nomkind)?
      };
      writeln!(f, "{}", input)?;
    }
    Ok(())
  }
}

type ParseResult<'a, T> = NomResult<'a, T, GameParseError<NomInput<'a>>>;

fn parse_color(i: Input) -> ParseResult<Color> {
  map(one_of("wWbB"), |c| match c {
    'w'|'W' => Color::White,
    'b'|'B' => Color::Black,
     _  => unreachable!()
  })(i)
}

fn parse_stone_kind(i: Input) -> ParseResult<StoneKind> {
  map(one_of("fFsScC"), |c| match c {
    'f'|'F' => StoneKind::FlatStone,
    's'|'S' => StoneKind::StandingStone,
    'c'|'C' => StoneKind::Capstone,
     _  => unreachable!()
  })(i)
}

fn parse_stone(i: Input) -> ParseResult<Stone> {
  map(pair(parse_color, parse_stone_kind), |(color, kind)| Stone::new(kind, color))(i)
}

fn parse_stack(i: Input) -> ParseResult<StoneStack> { 
  map_res(
    many0(parse_stone), |stones| StoneStack::multiple(stones)
  )(i)
}

fn parse_row(i: Input) -> ParseResult<Vec<StoneStack>> {
  map(
    tuple((
      multispace0, 
      char('|'), 
      many1(map(tuple((space0, parse_stack, space0, char('|'))), |(_, stack, _, _)| stack)),
      multispace0)), 
    |(_, _, stacks, _)| stacks)(i)
}

fn parse_game_state(i: Input) -> Result<Game, nom::Err<GameParseError<NomInput>>> {
  finalise(
    map_res(
      tuple((
        many1(parse_row),
        u32
      )),
    |(rows, size)| {
      let stacks = rows.into_iter().rev().flatten().collect();
      Game::from_data(stacks, size)
    }), i)
}

#[derive(Error)]
enum TestError {
  #[error("Failed to parse game state")]
  GameParseError(#[from] nom::Err<GameParseError<NomInput<'static>>>),
  #[error("Attempted an invalid move: {0}")]
  MoveInvalid(#[from] MoveInvalidReason)
}

impl std::fmt::Debug for TestError {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(f, "{}", self)
  }
}

#[test]
fn capstone_flattens_standing_stone() -> Result<(), TestError> {
  let mut game = parse_game_state("
    |  |||||
    |  |||||
    |  |||||
    |wc|||||
    |bs||||| 2")?;

  game.make_move("a2-".parse().unwrap())?;

  assert_eq!(game, parse_game_state("
    |    |||||
    |    |||||
    |    |||||
    |    |||||
    |bfwc||||| 3")?);

  Ok(())
}

#[test]
fn carry_limit() -> Result<(), TestError> {
  let mut game = parse_game_state("
    |wfwfwfwf|||
    |        |||
    |        ||| 2")?;

  let res = game.make_move("4a3-".parse().unwrap());
  assert_eq!(res.unwrap_err(), MoveInvalidReason::Movement(MovementInvalidReason::PickupTooLarge));

  Ok(())
}