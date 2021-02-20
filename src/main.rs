use read_input::prelude::*;
use rustak::*;

#[cfg(feature = "measure_game_size")]
use deepsize::DeepSizeOf;


fn main() {
  let mut game = Game::new(BoardSize::new(5).unwrap());

  loop {
    println!("{}", game);
    
    #[cfg(feature = "measure_game_size")]
    {
      println!("Size of game: {}", game.deep_size_of());
      println!("Size of game state: {}", game.board().deep_size_of() + game.held_stones().deep_size_of() + game.state().deep_size_of() + game.moves().deep_size_of() + game.move_state().deep_size_of());
    }

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
