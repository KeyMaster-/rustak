use std::str::FromStr;
use read_input::prelude::*;
use tak::*;

fn main() {
  let placement: Move = Move::Placement {
    location: Location::from_str("a7").expect("Invalid position string"),
    kind: StoneKind::StandingStone
  };
  println!("{} {:?}", placement, placement);

  let movement: Move = Move::Movement {
    start: Location::from_str("a7").expect("Invalid position string"),
    direction: Direction::Right,
    drops: vec![1,3,2]
  };
  println!("{} {:?}", movement, movement);

  loop {
    let next_move: Move = input().msg("Input next move: ").get();
    if let Move::Placement {kind: _, location} = &next_move {
      if location.x == 0 && location.y == 0 {
        break;
      }
    }

    println!("{}, {:?}", next_move, next_move);
  }
}
