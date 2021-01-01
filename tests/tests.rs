use rustak::*;
use rustak::parse::{Input, Error, utils::finalise };

use thiserror::Error;

fn game_from(i: Input) -> Result<Game, nom::Err<Error>> {
  finalise(Game::parse_state, i)
}

  // TODO clean this up to be an assert_eq-style macro that does nice printing of differences we find
  // Maybe also just make it a single assert_game_eq macro that takes a bool for "ignore history"
fn eq_no_history(a: &Game, b: &Game) -> bool {
  a.board() == b.board() &&
  a.held_stones() == b.held_stones() &&
  a.state() == b.state() &&
  a.moves() == b.moves() &&
  a.move_state() == b.move_state()
}

#[derive(Error)]
enum TestError {
  #[error("Failed to parse game state: {0}")]
  GameParseError(#[from] nom::Err<Error<'static>>),
  #[error("Attempted an invalid move: {0}")]
  MoveInvalid(#[from] MoveInvalidReason)
}

impl std::fmt::Debug for TestError {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(f, "{}", self)
  }
}

// first round tests

#[test]
fn first_round_requires_flat() -> Result<(), TestError> {
  let mut game = Game::new(BoardSize::new(5).unwrap());

  let res = game.make_move("Sa1".parse().unwrap());
  assert_eq!(res, Err(MoveInvalidReason::Placement(PlacementInvalidReason::StoneKindNotValid)));

  let res = game.make_move("Ca1".parse().unwrap());
  assert_eq!(res, Err(MoveInvalidReason::Placement(PlacementInvalidReason::StoneKindNotValid)));

  Ok(())
}

#[test]
fn first_round_swaps_colours() -> Result<(), TestError> {
  let mut game = Game::new(BoardSize::new(3).unwrap());
  
  game.make_move("a1".parse().unwrap())?;
  assert!(eq_no_history(&game, &game_from("
    |  |||
    |  |||
    |bf||| 1")?));

  game.make_move("a2".parse().unwrap())?;
  assert!(eq_no_history(&game, &game_from("
    |  |||
    |wf|||
    |bf||| 2")?));

  Ok(())
}

// movement tests

#[test]
fn capstone_flattens_standing_stone() -> Result<(), TestError> {
  // This needs to be a 5x5 since smaller board sizes don't have capstones available
  let mut game = game_from("
    |  |||||
    |  |||||
    |  |||||
    |wc|||||
    |bs||||| 2")?;

  game.make_move("a2-".parse().unwrap())?;

  assert!(eq_no_history(&game, &game_from("
    |    |||||
    |    |||||
    |    |||||
    |    |||||
    |bfwc||||| 3")?));

  Ok(())
}

#[test]
fn carry_limit() -> Result<(), TestError> {
  let mut game = game_from("
    |wfwfwfwf|||
    |        |||
    |        ||| 2")?;

  let res = game.make_move("4a3-".parse().unwrap());
  assert_eq!(res, Err(MoveInvalidReason::Movement(MovementInvalidReason::PickupTooLarge)));

  Ok(())
}

// win tests

#[test]
fn white_road_win() -> Result<(), TestError> {
  let mut game = game_from("
    |wf|wfwf||
    ||||
    |||| 2")?;

  let res = game.make_move("b3>".parse().unwrap());

  assert_eq!(res, Ok(GameState::Win(Color::White, WinKind::Road)));

  Ok(())
}