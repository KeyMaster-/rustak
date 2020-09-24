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
    carry: 4,
    drop: vec![4]
  };
  println!("{} {:?}", movement, movement);

  let l = Location::from_coords(2, 6).unwrap();
  println!("{:?}", l.x);
  
  // loop {
  //   let loc: Location = input().msg("Input next move: ").get();
  //   if loc.x == 0 && loc.y == 0 {
  //     break;
  //   }

  //   println!("{}, {:?}", loc, loc);
  // }
}
