use tak::*;
use nom::{
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

type ParseError<'a> = nom::error::VerboseError<NomInput<'a>>;
type ParseResult<'a, T> = NomResult<'a, T, ParseError<'a>>;

// "
// ;;;wfbcbs;;
// ;;;;;
// ;;;;;
// ;;;;;
// "

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

fn parse_stack(i: Input) -> ParseResult<Option<StoneStack>> {
  map_res(many0(parse_stone), |stones| {
    match StoneStack::multiple(stones) {
      Ok(stack) => Ok(Some(stack)),
      Err(error) => match error {
        StoneStackConstructionError::NoStones => Ok(None),
        StoneStackConstructionError::IllegalSequence => Err(()) // Better error type, propagate
      }
    }
  })(i)
}

fn parse_row(i: Input) -> ParseResult<Vec<Option<StoneStack>>> {
  map(
    tuple((
      multispace0, 
      char('|'), 
      many1(map(tuple((space0, parse_stack, space0, char('|'))), |(_, stack, _, _)| stack)),
      multispace0)), 
    |(_, _, stacks, _)| stacks)(i)
}

// todo custom error type, pass errors from the game creation out to the parse result
fn parse_board(i: Input) -> Result<Game, nom::Err<ParseError>> {
  finalise(
    map_res(
      tuple((
        many0(parse_row),
        u32
      )),
    |(rows, size)| {
      let stacks = rows.into_iter().rev().flatten().collect();
      Game::from_data(stacks, size)
    }), i)
}

#[test]
fn check_parsing() -> Result<(), nom::Err<ParseError<'static>>> {
  let game = parse_board("
    |    |||  ||
    |wfbc|||  ||
    |    |||  ||
    |    |||bs||
    |    |||  || 2")?;

  println!("{}", game);

  Ok(())
}