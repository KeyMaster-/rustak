use std::fmt;
use grid::Grid;
use thiserror::Error;
use bounded_integer::bounded_integer;

pub mod parse;

bounded_integer! {
  #[repr(usize)]
  pub struct BoardSize { 3..=8 }
}

const FILE_CHARS: &str = "abcdefgh";
const RANK_CHARS: &str = "12345678";

// Location on the board
// x is the file, i.e. the lettered direction
// y is the rank, i.e. the numbered direction
// note that x and y are 0-indexed, while ranks are 1-indexed
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Location {
  x: usize,
  y: usize
}

impl Location {
  pub fn from_coords(x: usize, y: usize) -> Option<Self> {
    if x < BoardSize::MAX_VALUE && y < BoardSize::MAX_VALUE {
      Some(Self { x, y })
    } else {
      None
    }
  }

  pub fn move_along(&self, dir: Direction, board_size: BoardSize) -> Option<Location> {
    use Direction::*;
    match dir {
      Left  => if self.x == 0                    { None } else { Some(Location { x: self.x - 1, y: self.y }) },
      Right => if self.x == board_size.get() - 1 { None } else { Some(Location { x: self.x + 1, y: self.y }) },
      Up    => if self.y == board_size.get() - 1 { None } else { Some(Location { x: self.x, y: self.y + 1 }) },
      Down  => if self.y == 0                    { None } else { Some(Location { x: self.x, y: self.y - 1 }) }
    }
  }

  fn touching_sides(&self, board_size: BoardSize) -> Sides {
    Sides::new(self.x == 0, self.y == board_size.get() - 1, self.x == board_size.get() - 1, self.y == 0)
  }

  fn neighbours(&self, board_size: BoardSize) -> Vec<Self> {
    use Direction::*;
    [self.move_along(Left, board_size), self.move_along(Up, board_size), self.move_along(Right, board_size), self.move_along(Down, board_size)].iter().filter_map(|&loc| loc).collect()
  }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum StoneKind {
  FlatStone,
  StandingStone,
  Capstone
}

impl StoneKind {
  fn check_move_onto(&self, top: &StoneKind) -> Result<bool, ()> {
    match top {
      StoneKind::FlatStone => Ok(false),
      StoneKind::StandingStone => {
        if *self == StoneKind::Capstone {
          Ok(true)
        } else {
          Err(())
        }
      },
      StoneKind::Capstone => Err(())
    }
  }

  fn counts_for_road(&self) -> bool {
    match self {
      StoneKind::FlatStone => true,
      StoneKind::StandingStone => false,
      StoneKind::Capstone => true
    }
  }
}

#[derive(Debug, Copy, Clone)]
pub enum Direction {
  Up,
  Down,
  Left,
  Right
}

#[derive(Debug)]
pub enum Move {
  Placement { kind: StoneKind, location: Location },
  Movement { start: Location, direction: Direction, drops: Vec<usize> }
}

pub struct ColorMove {
  color: Color,
  r#move: Move
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Color {
  White,
  Black
}

impl Color {
  fn opposite(&self) -> Color {
    match self {
      Color::White => Color::Black,
      Color::Black => Color::White
    }
  }

  fn swap(&mut self) {
    *self = self.opposite();
  }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Stone {
  kind: StoneKind,
  color: Color
}

impl Stone {
  pub fn new(kind: StoneKind, color: Color) -> Self {
    Self { kind, color }
  }

  fn place_onto(self, stack: &mut StoneStack) -> Result<(), ()> {
    if stack.count() != 0 {
      Err(())
    } else {
      stack.0.push(self);
      Ok(())
    }
  }
}

#[derive(Debug, Clone, Default, Eq, PartialEq)]
pub struct StoneStack(Vec<Stone>);

#[derive(Error, Debug)]
pub enum StoneStackConstructionError {
  #[error("Illegal sequence of stone kinds")]
  IllegalSequence
}

impl StoneStack {

  pub fn single(stone: Stone) -> Self {
    Self(vec![stone])
  }

  pub fn multiple(stones: Vec<Stone>) -> Result<Self, StoneStackConstructionError> {
    use StoneStackConstructionError::*;

    for (i, stone) in stones.iter().enumerate() {
      if i != stones.len() - 1 {
        if stone.kind != StoneKind::FlatStone {
          return Err(IllegalSequence);
        }
      }
    }
    Ok(Self(stones))
  }

  fn count(&self) -> usize {
    self.0.len()
  }

  fn take(&mut self, count: usize) -> Result<StoneStack, ()> {
    if count > self.count() { return Err(()) }
    Ok(StoneStack(self.0.split_off(self.count() - count)))
  }

  fn drop_split(mut self, count: usize) -> Result<(StoneStack, StoneStack), ()> {
    if count > self.count() { return Err(()) } 
    let top_stack = Self(self.0.split_off(count));
    Ok((self, top_stack))
  }

  fn top_stone(&self) -> Option<Stone> {
    self.0.last().copied()
  }

  fn bot_stone(&self) -> Option<Stone> {
    self.0.first().copied()
  }

  fn controlling_color(&self) -> Option<Color> {
    self.top_stone().map(|s| s.color)
  }

  fn move_onto(mut self, other: &mut StoneStack) -> Result<(), ()> {
    if self.count() == 0 {
      Ok(())
    } else if other.count() == 0 {
      *other = self;
      Ok(())
    } else {
      let self_bot = self.bot_stone().unwrap();
      let other_top = other.top_stone().unwrap();

      let flatten = self_bot.kind.check_move_onto(&other_top.kind)?;
      if flatten {
        other.0.last_mut().unwrap().kind = StoneKind::FlatStone;
      }
      other.0.append(&mut self.0);
      Ok(())
    }
  }

  fn iter(&self) -> std::slice::Iter<Stone> {
    self.0.iter()
  }
}

// The actual state on the board
#[derive(Debug, Eq, PartialEq, Clone)]
struct Board {
  stacks: Grid<StoneStack>,
  size: BoardSize
}

impl Board {
  fn new(size: BoardSize) -> Self {
    let size_prim = size.get();
    Self {
      stacks: Grid::new(size_prim, size_prim),
      size
    }
  }

  fn from_data(stacks: Vec<StoneStack>) -> Result<Self, ()> {
    let size = (stacks.len() as f64).sqrt();
    if size.fract() != 0.0 {
      return Err(());
    }

    let size = BoardSize::new(size as usize).ok_or(())?;

    Ok(Self {
      stacks: Grid::from_vec(stacks, size.get()),
      size
    })
  }
}

impl<T: Clone> core::ops::Index<Location> for Grid<T> {
  type Output = T;
  fn index(&self, loc: Location) -> &Self::Output {
    &self[loc.y][loc.x]
  }
}

impl<T: Clone> core::ops::IndexMut<Location> for Grid<T> {
  fn index_mut(&mut self, loc: Location) -> &mut Self::Output {
    &mut self[loc.y][loc.x]
  }
}

#[derive(Error, Debug, Eq, PartialEq)]
pub enum MoveInvalidReason {
  #[error("Invalid placement: {0}")]
  Placement(#[from] PlacementInvalidReason),

  #[error("Invalid movement: {0}")]
  Movement(#[from] MovementInvalidReason),

  #[error("Game has ended")]
  GameHasEnded,
}


#[derive(Error, Debug, Eq, PartialEq)]
pub enum PlacementInvalidReason {
  #[error("No stone of the requested kind was available")]
  NoStoneAvailable,
  #[error("The requested stone kind is not valid")]
  StoneKindNotValid,
  #[error("The placement location is outside the board")]
  LocationOutsideBoard,
  #[error("The target space is not empty")]
  SpaceOccupied
}

#[derive(Error, Debug, Eq, PartialEq)]
pub enum MovementInvalidReason {
  #[error("The starting location is outside the board")]
  StartOutsideBoard,
  #[error("The target space contains no stones to move")]
  SpaceEmpty,
  #[error("The active player does not control the target stack")]
  StartNotControlled,
  #[error("The target stack does not contain enough stones")]
  PickupTooLarge,
  #[error("The move steps outside the board")]
  MoveOutsideBoard,
  #[error("One of the drops creates an invalid stack")]
  DropNotAllowed, //TODO add location info about where the drop is invalid, maybe the index of the drop (i.e. first, second etc)
}

impl Board {
  fn loc_inside(&self, loc: &Location)->bool {
    loc.x < self.size.get() && loc.y < self.size.get()
  }

  fn make_move(&mut self, m: &ColorMove)->Result<(), MoveInvalidReason> {
    let ColorMove { color, r#move: m } = m;

    match m {
      Move::Placement { kind, location } => {
        use PlacementInvalidReason::*;

        if !self.loc_inside(location) { return Err(LocationOutsideBoard.into()) }
        let stack = &mut self.stacks[*location];
        Stone { kind: *kind, color: *color }.place_onto(stack).map_err(|_| SpaceOccupied.into())
      },
      Move::Movement { start, direction, drops } => {
        use MovementInvalidReason::*;

        if !self.loc_inside(start) { return Err(StartOutsideBoard.into()) }

        let stack = &mut self.stacks[*start];

        match stack.controlling_color() {
          None => return Err(SpaceEmpty.into()),
          Some(controlling_color) => if *color != controlling_color { return Err(StartNotControlled.into()) }
        }

        let pickup_count = drops.iter().sum();
        if pickup_count > self.size.get() {
          return Err(PickupTooLarge.into());
        }

        let mut carry_stack = stack.take(pickup_count).map_err(|_| MoveInvalidReason::from(PickupTooLarge))?;
        let mut drop_loc = *start;

        for &drop_count in drops {
          drop_loc = drop_loc.move_along(*direction, self.size).ok_or(MoveOutsideBoard)?;
          let target_stack = &mut self.stacks[drop_loc];

          let (drop_stack, new_carry_stack) = carry_stack.drop_split(drop_count).unwrap();
          carry_stack = new_carry_stack;
          drop_stack.move_onto(target_stack).map_err(|_| MoveInvalidReason::from(DropNotAllowed))?;
        }

        Ok(())
      }
    }
  }


  fn check_road_win(&self, last_move_color: Color) -> Option<Color> {
    let size_prim = self.size.get();
    let mut board_locs_to_process = Vec::with_capacity(size_prim * size_prim);
    for x in 0..size_prim {
      for y in 0..size_prim {
        board_locs_to_process.push(Location::from_coords(x, y).unwrap());
      }
    }

    let mut components: Vec<(Color, Sides)> = Vec::new();

    while let Some(loc) = board_locs_to_process.pop() {
      let stack = &self.stacks[loc];
      let top_stone = stack.top_stone();
      if top_stone.is_none() { continue; }
      let top_stone = top_stone.unwrap();
      if !top_stone.kind.counts_for_road() { continue; }

      let mut component_touched_sides = Sides::none();
      let mut component_locs_to_process = vec![loc];

      while let Some(loc) = component_locs_to_process.pop() {
        let touching = loc.touching_sides(self.size);
        component_touched_sides.or(&touching);

        for neighbour in loc.neighbours(self.size) {
            // TODO this is a copy of the main stack check above, probably worth factoring out
          let neighbour_stack = &self.stacks[neighbour];
          let neighbour_stone = neighbour_stack.top_stone();
          if neighbour_stone.is_none() { continue; }
          let neighbour_stone = neighbour_stone.unwrap();
          if !neighbour_stone.kind.counts_for_road() { continue; }
          if neighbour_stone.color != top_stone.color { continue; }

          // the neighbour counts for walls, and is of the same color, so it belongs to this component.
          // if this position is still in the global consider list, remove it from there and add it to the component's consider list.
          // This also avoids adding duplicate positions to the component's consider list.
          if let Some(idx) = board_locs_to_process.iter().position(|&other| other == neighbour) {
            board_locs_to_process.swap_remove(idx);
            component_locs_to_process.push(neighbour);
          }
        }
      }

      components.push((top_stone.color, component_touched_sides));
    }

    let mut white_win = false;
    let mut black_win = false;
    for (color, sides) in components {
      if sides.has_opposing() {
        match color {
          Color::White => white_win = true,
          Color::Black => black_win = true
        }
      }
    }

    if white_win && black_win {
      Some(last_move_color)
    } else if white_win {
      Some(Color::White)
    } else if black_win {
      Some(Color::Black)
    } else {
      None
    }

    // Grab the first one.
    //   If a road-participating stone, make new component, tag with stone's colour. Add stone to "to process" stack of that component.
    //   Grab next stone in component's to process stack. find all neighbours, add any that are connected to this component and not already on processing stack. remove them from the global to-process set.
    //   For each that's added, check what board edges it neighbours if any. Set a corresponding flag on the component.
    // When the component's stack is done, that's one component fully explored. Grab new space from global "to process" set and make a new component, until the set is empty.
    // Now, go through all components found. for each check if they have both top+bottom or left+right set. If either, that's a winning road. Set a win flag for that colour.
    // After looking at all components, check winner flags. 
    //  If none are set, no win
    //  If one is set, that colour winds
    //  If both are set, the last move created roads for both players. last_move_color wins.
  }

  fn all_spaces_occupied(&self) -> bool {
    self.stacks.iter().all(|stack| stack.count() != 0)
  }

  fn count_flats_control(&self) -> (u32, u32) {
    self.stacks.iter()
      .filter_map(|stack| stack.top_stone())
      .filter(|stone| stone.kind == StoneKind::FlatStone)
      .fold((0, 0), |(white_count, black_count), stone| match stone.color {
        Color::White => (white_count + 1, black_count),
        Color::Black => (white_count, black_count + 1)
      })
  }
}

#[derive(Debug, Copy, Clone)]
struct Sides {
  left: bool,
  top: bool,
  right: bool,
  bot: bool
}

impl Sides {
  fn new(left: bool, top: bool, right: bool, bot: bool) -> Self {
    Self { left, top, right, bot }
  }

  fn none() -> Self {
    Self::new(false, false, false, false)
  }

  fn has_opposing(&self) -> bool {
    self.left && self.right || self.top && self.bot
  }

  fn or(&mut self, other: &Sides) {
    self.left  |= other.left;
    self.top   |= other.top;
    self.right |= other.right;
    self.bot   |= other.bot;
  }
}


#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct HeldStones {
  flat: u8,
  capstone: u8
}

impl HeldStones {
  fn for_size(size: BoardSize) -> Self {
    match size.get() {
      3 => Self { flat: 10, capstone: 0},
      4 => Self { flat: 15, capstone: 0},
      5 => Self { flat: 21, capstone: 1},
      6 => Self { flat: 30, capstone: 1},
      7 => Self { flat: 40, capstone: 1},
      8 => Self { flat: 50, capstone: 2},
      _ => unreachable!()
    }
  }

  fn take_stone(&mut self, kind: StoneKind) -> bool {
    match kind {
      StoneKind::FlatStone | StoneKind::StandingStone => {
        if self.flat > 0 {
          self.flat -= 1;
          return true;
        } else {
          return false;
        }
      },
      StoneKind::Capstone => {
        if self.capstone > 0 {
          self.capstone -= 1;
          return true;
        } else {
          return false;
        }
      }
    }
  }

  fn has_run_out(&self) -> bool {
    self.flat == 0 && self.capstone == 0
  }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
struct GameHeldStones {
  white: HeldStones,
  black: HeldStones
}

impl GameHeldStones {
  fn new(held_stones: HeldStones) -> Self {
    Self {
      white: held_stones,
      black: held_stones
    }
  }

  fn take_stone(&mut self, kind: StoneKind, color: Color) -> bool {
    match color {
      Color::White => self.white.take_stone(kind),
      Color::Black => self.black.take_stone(kind),
    }
  }

  fn side_has_run_out(&self) -> bool {
    self.white.has_run_out() || self.black.has_run_out()
  }
}

// The state of a game
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Game {
  board: Board,
  held_stones: GameHeldStones,
  state: GameState,
  moves: u32
}

#[derive(Error, Debug)]
pub enum GameFromDataError {
  #[error("Invalid size")]
  InvalidSize, //TODO supply attempted size
  #[error("The board contains more pieces than are available at this game size")]
  TooManyPiecesForSize // TODO supply stone kind that was too many, and placed number vs allowed number
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum WinKind {
  Road,
  BoardFilled,
  PlayedAllStones(Color)
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum GameState {
  Ongoing,
  Win(Color, WinKind),
  Draw
}

impl Game {
  pub fn new(size: BoardSize) -> Self {
    Self {
      board: Board::new(size),
      held_stones: GameHeldStones::new(HeldStones::for_size(size)),
      state: GameState::Ongoing,
      moves: 0
    }
  }

  pub fn from_data(stacks: Vec<StoneStack>, moves: u32) -> Result<Self, GameFromDataError> {
    let board = Board::from_data(stacks).map_err(|_| GameFromDataError::InvalidSize)?;

    let stack_count_stones = |stack: &StoneStack, filter_color| {
      stack.iter().fold((0,0), |(flats, caps), stone| {
        if stone.color == filter_color {
          if stone.kind == StoneKind::Capstone {
            (flats, caps+1)
          } else {
            (flats+1, caps)
          }
        } else {
          (flats, caps)
        }
      })
    };

    let count_stones = |stacks: std::slice::Iter<StoneStack>, color| {
      stacks.fold((0, 0), 
        |(flats, caps), stack| {
          let (add_flats, add_caps) = stack_count_stones(stack, color);
          (flats + add_flats, caps + add_caps)
        })
    };
    
    let (white_flats, white_caps) = count_stones(board.stacks.iter(), Color::White);
    let (black_flats, black_caps) = count_stones(board.stacks.iter(), Color::Black);

    let full_held_stones = HeldStones::for_size(board.size);
    if white_flats > full_held_stones.flat || white_caps > full_held_stones.capstone ||
       black_flats > full_held_stones.flat || black_caps > full_held_stones.capstone {
        return Err(GameFromDataError::TooManyPiecesForSize);
    }

    let white_held_stones = HeldStones {
      flat: full_held_stones.flat - white_flats,
      capstone: full_held_stones.capstone - white_caps
    };

    let black_held_stones = HeldStones {
      flat: full_held_stones.flat - black_flats,
      capstone: full_held_stones.capstone - black_caps
    };

    let held_stones = GameHeldStones {
      white: white_held_stones,
      black: black_held_stones
    };

    let mut out = Self {
      board: board,
      held_stones,
      moves,
      state: GameState::Ongoing
    };

    out.state = out.evaluate_state(Color::White);
    Ok(out)
  }

  pub fn make_move(&mut self, m: Move) -> Result<GameState, MoveInvalidReason> {
    let mut game_clone = self.clone();

    let new_state = game_clone.internal_make_move(m)?;

    // If we got here, the move was okay and applied to the cloned game.
    // Now write the changes back to ourselves.
    *self = game_clone;
    Ok(new_state)
  }

  fn internal_make_move(&mut self, m: Move) -> Result<GameState, MoveInvalidReason> {
    if self.state != GameState::Ongoing {
      return Err(MoveInvalidReason::GameHasEnded);
    }

    let move_color = self.get_active_color();

    let mut effective_move_color = move_color;
    if self.get_turn() == 1 {
        if let Move::Placement { kind, ..} = m {
          if kind != StoneKind::FlatStone { return Err(PlacementInvalidReason::StoneKindNotValid.into()) }
        }
        
        effective_move_color.swap();
      }

    if let Move::Placement { kind, .. } = m {
      if !self.held_stones.take_stone(kind, effective_move_color) {
        return Err(PlacementInvalidReason::NoStoneAvailable.into());
      }
    }

    self.board.make_move(&ColorMove { r#move: m, color: effective_move_color } )?;

    self.moves += 1;
    self.state = self.evaluate_state(move_color);
    Ok(self.state)
  }


  fn evaluate_state(&self, last_move_color: Color) -> GameState {
    if let Some(color) = self.board.check_road_win(last_move_color) {
      GameState::Win(color, WinKind::Road)
    } else {
      let run_out = self.held_stones.side_has_run_out();
      let all_occupied = self.board.all_spaces_occupied();

      if run_out || all_occupied {
        let win_kind = if run_out { WinKind::PlayedAllStones(last_move_color) } else { WinKind::BoardFilled };

        let (white_controlled, black_controlled) = self.board.count_flats_control();
        if white_controlled > black_controlled {
          GameState::Win(Color::White, win_kind)
        } else if black_controlled > white_controlled {
          GameState::Win(Color::Black, win_kind)
        } else {
          GameState::Draw
        }
      } else {
        GameState::Ongoing
      }
    }
  }

  pub fn get_turn(&self) -> u32 {
    self.moves / 2 + 1
  }

  pub fn get_active_color(&self) -> Color {
    match self.moves % 2 {
      0 => Color::White,
      1 => Color::Black,
      _ => unreachable!()
    }
  }

  pub fn get_state(&self) -> GameState {
    self.state
  }

}

// Display impls

impl fmt::Display for Location {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let file = FILE_CHARS.chars().nth(self.x as usize).unwrap();
    let rank = self.y + 1;
    write!(f, "{}{}", file, rank)
  }
}

impl fmt::Display for StoneKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", match self {
      StoneKind::FlatStone => 'F',
      StoneKind::StandingStone => 'S',
      StoneKind::Capstone => 'C'
    })
  }
}

impl fmt::Display for Color {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", match self {
      Color::White => "White",
      Color::Black => "Black",
    })
  }
}

impl fmt::Display for Direction {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", match self {
      Direction::Up => '+',
      Direction::Down => '-',
      Direction::Left => '<',
      Direction::Right => '>'
    })
  }
}

impl fmt::Display for Move {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Move::Placement { location, kind } => {
        let stone_ident = match kind {
          StoneKind::FlatStone => "".to_string(),
          StoneKind::StandingStone | StoneKind::Capstone => kind.to_string()
        };
        write!(f, "{}{}", stone_ident, location)
      },
      Move::Movement { start, direction, drops } => {
        let pickup: usize = drops.iter().sum();

        let omit_pickup = pickup == 1;
        let pickup_text = 
          if omit_pickup {
            String::from("")
          } else {
            format!("{}", pickup)
          };

        let omit_drops = drops.len() == 1;
        let drops_sequence = 
          if omit_drops {
            String::from("")
          } else {
            drops.iter().map(|x| x.to_string()).collect::<Vec<String>>().join("")
          };

        write!(f, "{}{}{}{}", pickup_text, start, direction, drops_sequence)
      }
    }
  }
}

use colored::*;
const STONE_CHARS: [char; 3] = ['f', 's', 'c'];

fn apply_color(text: &str, color: Color) -> ColoredString {
  match color {
    Color::White => text.black().on_white(),
    Color::Black => text.white().on_black()
  }
}

impl Stone {
  fn to_colored_string(&self) -> ColoredString {
    let c = match self.kind {
      StoneKind::FlatStone => STONE_CHARS[0],
      StoneKind::StandingStone => STONE_CHARS[1],
      StoneKind::Capstone => STONE_CHARS[2]
    };

    apply_color(&c.to_string(), self.color)    
  }

  // TODO temp
  fn basic_notation(&self) -> String {
    let color_char = match self.color {
      Color::White => 'w',
      Color::Black => 'b'
    };

    let kind_char = match self.kind {
      StoneKind::FlatStone => STONE_CHARS[0],
      StoneKind::StandingStone => STONE_CHARS[1],
      StoneKind::Capstone => STONE_CHARS[2]
    };

    format!("{}{}", color_char, kind_char)
  }
}

impl fmt::Display for StoneStack {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.count() == 0 {
      write!(f, " ")?;
    } else {
      for stone in &self.0 {
        write!(f, "{}", stone.to_colored_string())?;
      }
    }
    
    Ok(())
  }
}

fn repeat_char(c: char, count: usize) -> String {
  (0..count).map(|_| c).collect()
}

fn stone_string_width(s: &String) -> usize {
  s.chars().filter(|c| *c == ' ' || STONE_CHARS.contains(c)).count()
}

fn pad_stone_string(s: &mut String, width: usize) {
  let s_width = stone_string_width(s);
  if s_width >= width {
    return;
  }

  let add_width = width - s_width;
  let left = add_width / 2;
  let right = add_width - left;

  *s = format!("{}{}{}", repeat_char(' ', left), s, repeat_char(' ', right))
}

impl fmt::Display for Board {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let size = self.size.get();
    let mut stack_strings = Grid::from_vec(self.stacks.iter().map(|stack| stack.to_string()).collect(), size);
    let col_widths: Vec<usize> = (0..size).map(|col_idx| stack_strings.iter_col(col_idx).map(stone_string_width).max().unwrap()).collect();

    for row_idx in 0..size {
      for (col_idx, stack_string) in stack_strings.iter_row_mut(row_idx).enumerate() {
        pad_stone_string(stack_string, col_widths[col_idx]);
      }
    }

    let row_strings = 
      (0..size)
      .map(|row_idx| stack_strings.iter_row(row_idx).cloned().collect::<Vec<String>>().join(" ┃ "))
      .enumerate()
      .map(|(row_idx, row_str)| format!("{} ┃ {} ┃", row_idx + 1, row_str))
      .rev();

    let cell_separators: Vec<_> = col_widths.iter().map(|&width| repeat_char('━', width)).collect();

    let start_row     = format!("  ┏━{}━┓", cell_separators.join("━┳━"));
    let separator_row = format!("  ┣━{}━┫", cell_separators.join("━╋━"));
    let end_row       = format!("  ┗━{}━┛", cell_separators.join("━┻━"));

    let file_cells: Vec<_> = 
      col_widths.iter()
      .zip((0..size).map(|col_idx| FILE_CHARS.chars().nth(col_idx).unwrap()))
      .map(|(&width, file_char)| format!("{:^width$}", file_char, width = width))
      .collect();

    let files_row = format!("    {}  ", file_cells.join("   "));

    writeln!(f, "{}", start_row)?;
    let mut first = true;
    for row_string in row_strings {
      if !first {
        writeln!(f, "{}", separator_row)?;
      } else {
        first = false;
      }

      writeln!(f, "{}", row_string)?;
    }
    writeln!(f, "{}", end_row)?;
    writeln!(f, "{}", files_row)?;

    Ok(())
  }
}

use std::fmt::Write;

impl StoneStack {
  // TODO temp
  fn basic_notation(&self) -> String {
    let mut out = String::new();
    for stone in &self.0 {
      write!(out, "{}", stone.basic_notation()).unwrap();
    }

    out
  }
}

impl Board {
  // TODO temp
  fn basic_notation(&self) -> String {
    let mut out = String::new();

    let stack_notations = self.stacks.iter().map(|stack| stack.basic_notation());
    for stack in stack_notations {
      write!(out, "{};", stack).unwrap();
    }

    out
  }
}

impl fmt::Display for Game {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.board)?;
    write!(f, "Turn {}, {} | W: {}F {}C, B: {}F {}C", 
      self.get_turn(),
      apply_color(&self.get_active_color().to_string(), self.get_active_color()),
      self.held_stones.white.flat, 
      self.held_stones.white.capstone, 
      self.held_stones.black.flat, 
      self.held_stones.black.capstone)
  }
}

impl Game {
  // TODO temp
  pub fn basic_notation(&self) -> String {
    let mut out = String::new();
    write!(out, "{}", self.board.basic_notation()).unwrap();
    write!(out, "S{}T{},A{},WF{}C{},BF{}C{}",
      match self.state {
        GameState::Ongoing => "O".to_string(),
        GameState::Draw => "D".to_string(),
        GameState::Win(color, kind) => format!("W{}{}", 
          match color {
            Color::White => 'W',
            Color::Black => 'B',
          }, match kind {
            WinKind::Road => 'R',
            WinKind::BoardFilled => 'F',
            WinKind::PlayedAllStones(_color) => 'F'
          })
      },
      self.get_turn(),
      match self.get_active_color() {
        Color::White => 'W',
        Color::Black => 'B',
      },
      self.held_stones.white.flat, 
      self.held_stones.white.capstone, 
      self.held_stones.black.flat, 
      self.held_stones.black.capstone).unwrap();

    out
  }
}