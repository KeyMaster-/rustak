use std::fmt;
use grid::Grid;
use thiserror::Error;
use bounded_integer::bounded_integer;

#[cfg(feature = "serde_support")]
use serde::{Serialize, Deserialize};

pub mod parse;

bounded_integer! {
  #[repr(usize)]
  pub struct BoardSize { 3..=8 }
}

const FILE_CHARS: &str = "abcdefgh";
const RANK_CHARS: &str = "12345678";

pub fn file_idx_to_char(idx: usize) -> Option<char> {
  FILE_CHARS.chars().nth(idx)
}

// Location on the board
// x is the file, i.e. the lettered direction
// y is the rank, i.e. the numbered direction
// note that x and y are 0-indexed, while ranks are 1-indexed
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[cfg_attr(feature = "serde_support", derive(Serialize, Deserialize))]
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

  pub fn neighbours(&self, board_size: BoardSize) -> Vec<Self> {
    use Direction::*;
    [Left, Up, Right, Down].iter().map(|&dir| self.move_along(dir, board_size)).filter_map(|loc| loc).collect()
    // [self.move_along(Left, board_size), self.move_along(Up, board_size), self.move_along(Right, board_size), self.move_along(Down, board_size)].iter().filter_map(|&loc| loc).collect()
  }

  pub fn neighbours_with_direction(&self, board_size: BoardSize) -> Vec<(Self, Direction)> {
    use Direction::*;
    [Left, Up, Right, Down].iter()
      .map(|&dir| (self.move_along(dir, board_size), dir))
      .filter_map(|(loc, dir)| if let Some(loc) = loc { Some((loc, dir)) } else { None })
      .collect()
  }

  pub fn x(&self) -> usize { self.x }
  pub fn y(&self) -> usize { self.y }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[cfg_attr(feature = "serde_support", derive(Serialize, Deserialize))]
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

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[cfg_attr(feature = "serde_support", derive(Serialize, Deserialize))]
pub enum Direction {
  Up,
  Down,
  Left,
  Right
}

#[derive(Debug)]
#[cfg_attr(feature = "serde_support", derive(Serialize, Deserialize))]
pub enum Move {
  Placement { kind: StoneKind, location: Location },
  Movement { start: Location, direction: Direction, drops: Vec<usize> }
}

pub struct ColorMove {
  color: Color,
  r#move: Move
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[cfg_attr(feature = "serde_support", derive(Serialize, Deserialize))]
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
#[cfg_attr(feature = "serde_support", derive(Serialize, Deserialize))]
pub struct Stone {
  pub kind: StoneKind,
  pub color: Color
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
#[cfg_attr(feature = "serde_support", derive(Serialize, Deserialize))]
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

  pub fn count(&self) -> usize {
    self.0.len()
  }

  fn take(&mut self, count: usize) -> Result<StoneStack, ()> {
    if count > self.count() { return Err(()) }
    Ok(StoneStack(self.0.split_off(self.count() - count)))
  }

  fn drop_split(mut self, count: usize) -> Result<(StoneStack, StoneStack), ()> {
    let bot_stack = self.drop_stones(count)?;
    Ok((bot_stack, self))
  }

  fn drop_stones(&mut self, count: usize) -> Result<StoneStack, ()> {
    if count > self.count() { return Err(()); }
    let top_stack = Self(self.0.split_off(count));
    let bot_stack = std::mem::replace(self, top_stack);
    Ok(bot_stack)
  }

  pub fn top_stone(&self) -> Option<Stone> {
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

  pub fn iter(&self) -> std::slice::Iter<Stone> {
    self.0.iter()
  }
}

// The actual state on the board
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Board {
  stacks: Grid<StoneStack>,
  size: BoardSize
}

// Doing an impl on board instead of Grid since Grid would equire a newtype
// Also, we want to omit the board size anyway, and check the provided sequence
// length during deserialization
#[cfg(feature = "serde_support")]
impl Serialize for Board {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    serializer.collect_seq(self.stacks.flatten())
  }
}

#[cfg(feature = "serde_support")]
struct BoardVisitor;

#[cfg(feature = "serde_support")]
impl<'de> serde::de::Visitor<'de> for BoardVisitor {
  type Value = Board;

  fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
    write!(formatter, "a sequence of stone stacks of length ")?;
    for s in BoardSize::MIN_VALUE..=BoardSize::MAX_VALUE {
      write!(formatter, "{}", s*s)?;
      if s == BoardSize::MAX_VALUE - 1 {
        write!(formatter, ", or ")?;
      } else if s != BoardSize::MAX_VALUE {
        write!(formatter, ", ")?;
      }
    }
    Ok(())
  }

  fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
  where
    A: serde::de::SeqAccess<'de>
  {
    let mut stacks = Vec::new();
    while let Some(stack) = seq.next_element()? {
      stacks.push(stack);
    }

    let stacks_len = stacks.len();

    Ok(Board::from_data(stacks).map_err(|_| serde::de::Error::invalid_length(stacks_len, &self))?)
  }
}
#[cfg(feature = "serde_support")]
impl<'de> Deserialize<'de> for Board {
  fn deserialize<D>(deserializer: D) -> Result<Self, D::Error> 
  where
    D: serde::Deserializer<'de>
  {
    deserializer.deserialize_seq(BoardVisitor)
  }
  //deserialize sequence of stonestacks
  //build boardsize from sqrt(len) (convenience method for that on baord size?)
  // return err if boardsize errors
  //build grid out of sequence
  //return self with sequence and boardsize
}

impl Board {
  pub fn new(size: BoardSize) -> Self {
    let size_prim = size.get();
    Self {
      stacks: Grid::new(size_prim, size_prim),
      size
    }
  }

  pub fn from_data(stacks: Vec<StoneStack>) -> Result<Self, ()> {
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

  // TODO turn into Index trait similar to what Grid does with Index<usize>
  pub fn get(&self, x: usize, y: usize) -> &StoneStack { 
    &self.stacks[y][x]
  }

  pub fn size(&self) -> BoardSize {
    self.size
  }
}

impl core::ops::Index<Location> for Board {
  type Output = StoneStack;
  fn index(&self, loc: Location) -> &Self::Output {
    &self.stacks[loc]
  }
}

// Clone constraint is required by Grid itself
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
#[cfg_attr(feature = "serde_support", derive(Serialize, Deserialize))]
pub struct HeldStones {
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

  pub fn flat(&self) -> u8 {
    self.flat
  }

  pub fn capstone(&self) -> u8 {
    self.capstone
  }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
#[cfg_attr(feature = "serde_support", derive(Serialize, Deserialize))]
pub struct GameHeldStones {
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

  fn take_stone(&mut self, kind: StoneKind, color: Color) -> Option<Stone> {
    let had_stone = match color {
      Color::White => self.white.take_stone(kind),
      Color::Black => self.black.take_stone(kind),
    };

    if had_stone {
      Some(Stone { color, kind })
    } else {
      None
    }
  }

  fn side_has_run_out(&self) -> bool {
    self.white.has_run_out() || self.black.has_run_out()
  }

  pub fn get(&self, color: Color) -> HeldStones {
    match color {
      Color::White => self.white,
      Color::Black => self.black
    }
  }
}

// The state of a game
#[derive(Debug, Eq, PartialEq, Clone)]
#[cfg_attr(feature = "serde_support", derive(Serialize, Deserialize))]
pub struct Game {
  board: Board,
  held_stones: GameHeldStones,
  state: GameState,
  moves: u32,
  move_state: MoveState
}

#[derive(Error, Debug)]
pub enum GameFromDataError {
  #[error("Invalid size")]
  InvalidSize, //TODO supply attempted size
  #[error("The board contains more pieces than are available at this game size")]
  TooManyPiecesForSize // TODO supply stone kind that was too many, and placed number vs allowed number
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
#[cfg_attr(feature = "serde_support", derive(Serialize, Deserialize))]
pub enum WinKind {
  Road,
  BoardFilled,
  PlayedAllStones(Color)
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
#[cfg_attr(feature = "serde_support", derive(Serialize, Deserialize))]
pub enum GameState {
  Ongoing,
  Win(Color, WinKind),
  Draw
}

#[derive(Debug, Eq, PartialEq, Clone)]
#[cfg_attr(feature = "serde_support", derive(Serialize, Deserialize))]
pub enum MoveState {
  Start,
  Placed { loc: Location, kind: StoneKind },
  Movement { start: Location, cur_loc: Location, carry: StoneStack, dir: Option<Direction>, drops: Vec<usize> }
}

pub enum MoveAction {
  Place { loc: Location, kind: StoneKind },
  Pickup { loc: Location, count: usize },
  MoveAndDropOne { dir: Direction },
  Drop { count: usize }
}

#[derive(Error, Debug, Eq, PartialEq)]
pub enum ActionInvalidReason {
  #[error("Action is not applicable to the current state")]
  InvalidState, // TODO maybe add expected and observed state

  #[error("The first move for each player must be a placement")]
  FirstMoveMustBePlacement,

  #[error("Game has ended")]
  GameHasEnded,

  #[error("The move start location is outside the board")]
  MoveStartOutsideBoard, // TODO maybe report the location vs board size

  #[error("The direction of the move action does not match the ongoing move's direction.")]
  MovementDirMismatch, // TODO this feels like it should be part of MovementInvalidReason, but also it's action-specific

  #[error("Tried to drop more stones than are in the carry stack.")]
  DropTooLarge, // TODO should be part of the movement reasons, but atm those are still also part of MoveInvalidReason, where this wouldn't make sense

  #[error("Invalid placement action: {0}")]
  PlacementInvalid(#[from] PlacementInvalidReason),

  #[error("Invalid movement action: {0}")]
  MovementInvalid(#[from] MovementInvalidReason),

}

impl Game {
  pub fn new(size: BoardSize) -> Self {
    Self {
      board: Board::new(size),
      held_stones: GameHeldStones::new(HeldStones::for_size(size)),
      state: GameState::Ongoing,
      moves: 0,
      move_state: MoveState::Start
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
      state: GameState::Ongoing,
      move_state: MoveState::Start
    };

    out.state = out.evaluate_state(Color::White);
    Ok(out)
  }

  pub fn do_action(&mut self, action: MoveAction) -> Result<(), ActionInvalidReason> {
    let mut game_clone = self.clone();
    game_clone.do_action_internal(action)?;

    // If we got here, the action was okay and was fully applied to the cloned game.
    // Now write the changes back to ourselves.
    *self = game_clone;
    Ok(())
  }

    // TODO can be local closure in do_action
  fn do_action_internal(&mut self, action: MoveAction) -> Result<(), ActionInvalidReason> {
    use ActionInvalidReason::*;

    if self.state != GameState::Ongoing {
      return Err(ActionInvalidReason::GameHasEnded);
    }

    match action {
      MoveAction::Place { loc, kind } => {
        use PlacementInvalidReason::*;

        if self.move_state != MoveState::Start { return Err(InvalidState); }

        let mut stone_color = self.active_color();
        if self.turn() == 1 {
          if kind != StoneKind::FlatStone { return Err(StoneKindNotValid.into()); }
          stone_color.swap();
        }

        let stone = self.held_stones.take_stone(kind, stone_color).ok_or(ActionInvalidReason::from(NoStoneAvailable))?;

        if !self.board.loc_inside(&loc) { return Err(MoveStartOutsideBoard); }
        let stack = &mut self.board.stacks[loc];
        stone.place_onto(stack).map_err(|_| ActionInvalidReason::from(SpaceOccupied))?;

        self.move_state = MoveState::Placed { loc, kind };
        Ok(())
      },
      MoveAction::Pickup { loc, count } => {
        use MovementInvalidReason::*;

        if self.move_state != MoveState::Start { return Err(InvalidState); }
        if self.turn() == 1 { return Err(FirstMoveMustBePlacement); }

        if !self.board.loc_inside(&loc) { return Err(MoveStartOutsideBoard); }

        let active_color = self.active_color(); // cache this because we can't borrow self again after we mutably borrow the stack

        let stack = &mut self.board.stacks[loc];
        match stack.controlling_color() {
          None => return Err(SpaceEmpty.into()),
          Some(controlling_color) => if active_color != controlling_color { return Err(StartNotControlled.into()); }
        }

        if count > self.board.size.get() { return Err(PickupTooLarge.into()); }
        let carry = stack.take(count).map_err(|_| ActionInvalidReason::from(PickupTooLarge))?;

        self.move_state = MoveState::Movement { start: loc, cur_loc: loc, carry, dir: None, drops: vec![] };

        Ok(())
      },
      MoveAction::MoveAndDropOne { dir } => {
        use MovementInvalidReason::*;

        if let MoveState::Movement { cur_loc, carry, dir: state_dir, drops, .. } = &mut self.move_state {

          if let &mut Some(state_dir) = state_dir {
            if state_dir != dir { return Err(MovementDirMismatch); }
          } else {
            *state_dir = Some(dir);
          }

          *cur_loc = cur_loc.move_along(dir, self.board.size()).ok_or(ActionInvalidReason::from(MoveOutsideBoard))?;

          let drop_stack = carry.drop_stones(1).map_err(|_| DropTooLarge)?;
          let target_stack = &mut self.board.stacks[*cur_loc];
          drop_stack.move_onto(target_stack).map_err(|_| ActionInvalidReason::from(DropNotAllowed))?;

          drops.push(1);

          Ok(())
        } else {
          Err(InvalidState)
        }
      },
      MoveAction::Drop { count } => {
        use MovementInvalidReason::*;

        let return_to_start = 
          if let MoveState::Movement { cur_loc, carry, drops, .. } = &mut self.move_state {
            let drop_stack = carry.drop_stones(count).map_err(|_| DropTooLarge)?;
            let target_stack = &mut self.board.stacks[*cur_loc];
            drop_stack.move_onto(target_stack).map_err(|_| ActionInvalidReason::from(DropNotAllowed))?;

            let drops_len = drops.len(); // cache the length since we will need to know it while mutably borrowing `drops`, and `.len()` borrows drops again
            if drops_len > 0 {
              drops[drops_len - 1] += count;
            }

            if carry.count() == 0 && drops.len() == 0 {
              true
            } else {
              false
            }
          } else {
            return Err(InvalidState)
          };

        if return_to_start {
          self.move_state = MoveState::Start;
        }
        Ok(())
      }
    }
  }

  pub fn finish_move(&mut self) -> Result<(), ()> { // TODO error type
    match &self.move_state {
      MoveState::Start => Err(()),
      MoveState::Placed { .. } => Ok(()),
      MoveState::Movement { carry, .. } => if carry.count() == 0 { Ok(()) } else { Err(()) }
    }?;

    let move_color = self.active_color();
    self.state = self.evaluate_state(move_color);
    self.moves += 1;
    self.move_state = MoveState::Start;
    Ok(())
  }

  pub fn make_move(&mut self, m: Move) -> Result<GameState, MoveInvalidReason> {
    let mut game_clone = self.clone();

      // TODO this is very temp
    fn action_to_move_reason(action_reason: ActionInvalidReason) -> MoveInvalidReason {
      use ActionInvalidReason::*;

      match action_reason {
        InvalidState => panic!("make_move tried to do an action on an invalid state"),

        FirstMoveMustBePlacement => MovementInvalidReason::SpaceEmpty.into(),

        GameHasEnded => MoveInvalidReason::GameHasEnded,

        MoveStartOutsideBoard => MovementInvalidReason::StartOutsideBoard.into(), // technically could have been a placement as well

        MovementDirMismatch => panic!("make_move gave a mismatching direction during a move"),

        DropTooLarge => panic!("make_move tried to drop more stones than it picked up"),

        PlacementInvalid(reason) => reason.into(),

        MovementInvalid(reason) => reason.into(),
      }
    }

    match m {
      Move::Placement { kind, location } => {
        game_clone.do_action_internal(MoveAction::Place { loc: location, kind: kind }).map_err(|e| action_to_move_reason(e))?;
      },
      Move::Movement { start, direction, drops } => {
        let pickup_count = drops.iter().sum();
        game_clone.do_action_internal(MoveAction::Pickup { loc: start, count: pickup_count }).map_err(|e| action_to_move_reason(e))?;
        for drop_count in drops {
          game_clone.do_action_internal(MoveAction::MoveAndDropOne { dir: direction }).map_err(|e| action_to_move_reason(e))?;
          game_clone.do_action_internal(MoveAction::Drop { count: drop_count - 1}).map_err(|e| action_to_move_reason(e))?;
        }
      }
    }

    // If we got here, all actions were okay and were fully applied to the cloned game.
    let finish_res = game_clone.finish_move();
    if finish_res.is_err() {
      panic!("make_move tried to finish an incomplete move");
    }

    // Now write the changes back to ourselves.
    *self = game_clone;
    Ok(self.state)
  }

  // pub fn make_move(&mut self, m: Move) -> Result<GameState, MoveInvalidReason> {
  //   let mut game_clone = self.clone();

  //   let new_state = game_clone.internal_make_move(m)?;

  //   // If we got here, the move was okay and applied to the cloned game.
  //   // Now write the changes back to ourselves.
  //   *self = game_clone;
  //   Ok(new_state)
  // }

  // fn internal_make_move(&mut self, m: Move) -> Result<GameState, MoveInvalidReason> {
  //   if self.state != GameState::Ongoing {
  //     return Err(MoveInvalidReason::GameHasEnded);
  //   }

  //   let move_color = self.active_color();

  //   let mut effective_move_color = move_color;
  //   if self.turn() == 1 {
  //       if let Move::Placement { kind, ..} = m {
  //         if kind != StoneKind::FlatStone { return Err(PlacementInvalidReason::StoneKindNotValid.into()) }
  //       }
        
  //       effective_move_color.swap();
  //     }

  //   if let Move::Placement { kind, .. } = m {
  //     if !self.held_stones.take_stone(kind, effective_move_color) {
  //       return Err(PlacementInvalidReason::NoStoneAvailable.into());
  //     }
  //   }

  //   self.board.make_move(&ColorMove { r#move: m, color: effective_move_color } )?;

  //   self.moves += 1;
  //   self.state = self.evaluate_state(move_color);
  //   Ok(self.state)
  // }


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

  pub fn turn(&self) -> u32 {
    self.moves / 2 + 1
  }

  pub fn active_color(&self) -> Color {
    match self.moves % 2 {
      0 => Color::White,
      1 => Color::Black,
      _ => unreachable!()
    }
  }

  pub fn state(&self) -> GameState {
    self.state
  }

  pub fn move_state(&self) -> &MoveState {
    &self.move_state
  }

  pub fn board(&self) -> &Board {
    &self.board
  }

  pub fn held_stones(&self) -> GameHeldStones {
    self.held_stones
  }

}

impl MoveState {
  pub fn to_move(self) -> Option<Move> {
    match self {
      Self::Start => None,
      Self::Placed { loc, kind } => Some(Move::Placement { kind, location: loc }) ,
      Self::Movement { start, cur_loc: _, carry, dir, drops } => {
        if carry.count() == 0 && dir.is_some() && drops.len() > 0 { // TODO if expected invariants were strictly upheld by the type, carry.count() == 0 would be enough here
          Some(Move::Movement { start, direction: dir.unwrap(), drops: drops })
        } else {
          None
        }
      },
    }
  }
}

// Display impls

impl fmt::Display for Location {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let file = file_idx_to_char(self.x as usize).unwrap();
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
      .zip((0..size).map(|col_idx| file_idx_to_char(col_idx).unwrap()))
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
      self.turn(),
      apply_color(&self.active_color().to_string(), self.active_color()),
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
      self.turn(),
      match self.active_color() {
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