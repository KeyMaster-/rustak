pub mod nom {
  use ::nom::{
    IResult,
    InputLength,
    error::{ParseError, FromExternalError},
    combinator::{map, map_res, all_consuming, verify},
    character::complete::{one_of, digit1}
  };

  pub type Input<'a> = &'a str;
  pub type Result<'a, T, E> = IResult<Input<'a>, T, E>;

  pub fn finalise<I, O, EI: ParseError<I>, E, F>(f: F, input: I) -> std::result::Result<O, E>
  where 
    I: InputLength,
    E: std::convert::From<nom::Err<EI>>,
    F: FnMut(I) -> IResult<I, O, EI>
  {
    let (_, res) = all_consuming(f)(input)?;
    Ok(res)
  }

  pub fn digit<'a, E: ParseError<Input<'a>>>(i: Input<'a>) -> Result<char, E> {
    one_of("0123456789")(i)
  }

  pub fn digit_nonzero<'a, E: ParseError<Input<'a>>>(i: Input<'a>) -> Result<char, E> {
    verify(digit, |&d| d != '0')(i)
  }

  pub fn digit_u8<'a, E: ParseError<Input<'a>>>(i: Input<'a>) -> Result<u8, E> {
    map(digit, |c| c.to_digit(10).unwrap() as u8)(i)
  }

  pub fn digit_nonzero_u8<'a, E: ParseError<Input<'a>>>(i: Input<'a>) -> Result<u8, E> {
    map(digit_nonzero, |c| c.to_digit(10).unwrap() as u8)(i)
  }
  
  pub fn u32<'a, E: ParseError<Input<'a>> + FromExternalError<Input<'a>, std::num::ParseIntError>>(i: Input<'a>) -> Result<u32, E> {
    map_res(digit1, |sub: Input| sub.parse::<u32>())(i)
  }
}