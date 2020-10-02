use read_input::prelude::*;
use tak::*;


fn main() {
  let mut game = Game::new();

  loop {
    println!("{}", game);
    let next_move: Move = input().msg("Input next move: ").get();
    println!("{:?}", next_move);

    let game_before_move = game.clone();
    let move_res = game.make_move(next_move);
    if let Err(error) = move_res {
      println!("{:?}", error);

      if game_before_move != game {
        println!("Move was invalid, but the game state has changed!");
      }
    }
  }
}
