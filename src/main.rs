use read_input::prelude::*;
use tak::*;


fn main() {
  // use colored::*;
  // let pieces = vec!["n", "=", "o"];
  // let white_pieces = pieces.iter().map(|piece| piece.white().on_black()).map(|s| s.to_string()).collect::<Vec<String>>().join("");
  // let black_pieces = pieces.iter().map(|piece| piece.black().on_white()).map(|s| s.to_string()).collect::<Vec<String>>().join("");

  // println!("{}", white_pieces);
  // println!("{}", black_pieces);

  // let stack = StoneStack::multiple(vec![Stone::new(StoneKind::FlatStone, Color::White), Stone::new(StoneKind::FlatStone, Color::White), Stone::new(StoneKind::FlatStone, Color::Black), Stone::new(StoneKind::Capstone, Color::White)]).unwrap();
  // let stack_string = stack.to_string();
  // println!("{}", stack_string);
  // println!("{:?}", stack_string.chars().collect::<Vec<_>>());

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
