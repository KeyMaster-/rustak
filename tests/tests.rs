use tak::*;
use tak::parse::{Input, Error, utils::finalise };

use thiserror::Error;

fn game_from(i: Input) -> Result<Game, nom::Err<Error>> {
  finalise(Game::parse_state, i)
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

#[test]
fn capstone_flattens_standing_stone() -> Result<(), TestError> {
  let mut game = game_from("
    |  |||||
    |  |||||
    |  |||||
    |wc|||||
    |bs||||| 2")?;

  game.make_move("a2-".parse().unwrap())?;

  assert_eq!(game, game_from("
    |    |||||
    |    |||||
    |    |||||
    |    |||||
    |bfwc||||| 3")?);

  Ok(())
}

#[test]
fn carry_limit() -> Result<(), TestError> {
  let mut game = game_from("
    |wfwfwfwf|||
    |        |||
    |        ||| 2")?;

  let res = game.make_move("4a3-".parse().unwrap());
  assert_eq!(res.unwrap_err(), MoveInvalidReason::Movement(MovementInvalidReason::PickupTooLarge));

  Ok(())
}