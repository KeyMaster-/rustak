use read_input::prelude::*;
use rustak::*;


fn main() {
  let mut game = Game::new(BoardSize::new(5).unwrap());

  loop {
    println!("{}", game);
    let next_move: Move = input().msg("Input next move: ").get();

    let move_res = game.make_move(next_move);
    if let Err(error) = move_res {
      println!("{}", error);
    } else {
      let game_state = move_res.unwrap();
      println!("{:?}", game_state);
    }
  }
}
