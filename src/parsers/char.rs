use crate::defs::*;

fn update_pos(pos: SourceLoc, c: char) -> SourceLoc {
    if c == '\n' {
        SourceLoc {
            col: 1,
            line: pos.line + 1,
            file: pos.file,
        }
    } else {
        SourceLoc {
            col: pos.col + 1,
            line: pos.line,
            file: pos.file,
        }
    }
}

pub fn satisfy<'input, S, F>(f: F) -> impl Parser<'input, S, char>
where
    F: Fn(char) -> bool,
    S: Stream<Item = char>,
{
    move |input: PState<'input, S>| match input.input.uncons() {
        Some((x, xs)) if f(x) => Ok((
            x,
            PState {
                input: xs,
                location: update_pos(input.location, x),
            },
        )),
        Some((x, _)) => Err((input.location, vec![Reason::Unexpected(x.to_string())])),
        None => Err((input.location, vec![Reason::Unexpected("EOF".to_string())])),
    }
}

pub fn char_p<'input, S>(c: char) -> impl Parser<'input, S, char>
where
    S: Stream<Item = char>,
{
    satisfy(move |x| x == c).named(&c.to_string())
}

pub fn string_p<'input, S>(s: &'input str) -> impl Parser<'input, S, &'input str>
where
    S: Stream<Item = char>,
{
    (move |mut input: PState<'input, S>| {
        let mut generated_string = String::new();
        for c in s.chars() {
            if let Some((x, xs)) = input.input.uncons() {
                generated_string.push(x);
                if x != c {
                    return Err((input.location, vec![Reason::Unexpected(generated_string)]));
                }
                input.location = update_pos(input.location, c);
                input.input = xs;
            } else {
                return Err((input.location, vec![Reason::Unexpected(generated_string)]));
            }
        }
        Ok((s, input))
    })
    .named(s)
}

/// Parses an ascii digit and returns it
pub fn digit<S: Stream<Item = char>>(state: PState<'_, S>) -> PResult<'_, S, char> {
    satisfy(|x| x.is_ascii_digit()).named("Digit").parse(state)
}

/// Parses a whitespace character
pub fn space<S: Stream<Item = char>>(state: PState<'_, S>) -> PResult<'_, S, char> {
    satisfy(|x| x.is_whitespace())
        .named("Whitespace Character")
        .parse(state)
}

/// Parses whitespace
pub fn whitespace<S: Stream<Item = char>>(state: PState<'_, S>) -> PResult<'_, S, ()> {
    space.some().named("Whitespace").ignore().parse(state)
}

pub trait Lexeme<'a, S, T>: Parser<'a, S, T>
where
    S: Stream<Item = char>,
{
    ///Parses `self` and the whitespace after it
    fn lexeme(&self) -> impl Parser<'a, S, T> {
        self.ignore_right(whitespace)
    }
}

impl<'a, S, T, P> Lexeme<'a, S, T> for P
where
    P: Parser<'a, S, T>,
    S: Stream<Item = char>,
{
}

/// Parses an integer, possibly negative or not
pub fn int<S: Stream<Item = char>>(state: PState<'_, S>) -> PResult<'_, S, i64> {
    (move |state| {
        let (x, state) = char_p('-').opt().parse(state)?;
        let neg = x.is_some();

        let (x, state) = digit.some().parse(state)?;

        let mut n: i64 = 0;
        for (p, c) in x.iter().rev().enumerate() {
            n += (c.to_digit(10).unwrap() as i64) * (10_i64.pow(p as u32));
        }

        if neg {
            n *= -1;
        }

        Ok((n, state))
    })
    .named("Integer")
    .parse(state)
}
