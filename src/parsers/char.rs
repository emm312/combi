use std::ops::{Bound, RangeBounds};

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

/// Returns for any character for which `f` returns `true`
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

pub fn satisfy_map<'input, S, F, B>(f: F) -> impl Parser<'input, S, B>
where
    F: Fn(char) -> Option<B>,
    S: Stream<Item = char>,
{
    move |input: PState<'input, S>| match input.input.uncons() {
        Some((c, xs)) => match f(c) {
            Some(x) => Ok((
                x,
                PState {
                    input: xs,
                    location: update_pos(input.location, c),
                },
            )),
            None => Err((input.location, vec![Reason::Unexpected(c.to_string())])),
        },
        None => Err((input.location, vec![Reason::Unexpected("EOF".to_string())])),
    }
}

/// Returns any single character
pub fn any_single<S>(input: PState<'_, S>) -> PResult<'_, S, char>
where
    S: Stream<Item = char>,
{
    satisfy(|_| true).parse(input)
}

/// Parses any single character in `s`
pub fn one_of<'input, S>(s: &'input str) -> impl Parser<'input, S, char>
where
    S: Stream<Item = char>,
{
    move |input: PState<'input, S>| {
        let expectations = s.chars().map(|x| Reason::Expected(x.to_string()));

        match input.input.uncons() {
            Some((x, xs)) if s.contains(x) => Ok((
                x,
                PState {
                    input: xs,
                    location: update_pos(input.location, x),
                },
            )),

            Some((x, _)) => Err((
                input.location,
                expectations
                    .chain(std::iter::once(Reason::Unexpected(x.to_string())))
                    .collect(),
            )),
            None => Err((
                input.location,
                expectations
                    .chain(std::iter::once(Reason::Unexpected("EOF".to_string())))
                    .collect(),
            )),
        }
    }
}

/// Parses any single character not in `s`
pub fn none_of<'input, S>(s: &'input str) -> impl Parser<'input, S, char>
where
    S: Stream<Item = char>,
{
    move |input: PState<'input, S>| {
        let expectation = Reason::Expected(format!(
            "none_of({})",
            s.chars().map(|x| x.to_string()).collect::<String>()
        ));

        match input.input.uncons() {
            Some((x, xs)) if !s.contains(x) => Ok((
                x,
                PState {
                    input: xs,
                    location: update_pos(input.location, x),
                },
            )),

            Some((x, _)) => Err((
                input.location,
                vec![expectation, Reason::Unexpected(x.to_string())],
            )),
            None => Err((
                input.location,
                vec![expectation, Reason::Unexpected("EOF".to_string())],
            )),
        }
    }
}

/// Parses any single character `c`
pub fn char<'input, S>(c: char) -> impl Parser<'input, S, char>
where
    S: Stream<Item = char>,
{
    satisfy(move |x| x == c).named(c)
}

/// Parses any single character but `c`
pub fn not_char<'input, S>(c: char) -> impl Parser<'input, S, char>
where
    S: Stream<Item = char>,
{
    satisfy(move |x| x != c).named(format!("not({c})"))
}

pub fn succ(c: char) -> char {
    (c as u8 + 1) as char 
}

pub fn pred(c: char) -> char {
    (c as u8 - 1) as char 
}

pub fn range<'input, S>(range: impl RangeBounds<char>) -> impl Parser<'input, S, char>
where
    S: Stream<Item = char>,
{
    move |input| {
        let (c, input) = any_single(input)?;
        if range.contains(&c) {
            Ok((c, input))
        } else {
            match (range.start_bound(), range.end_bound()) {
                (Bound::Excluded(l), Bound::Excluded(r)) =>
                    Err((input.location, vec![Reason::Expected(format!("[{}-{}]", succ(*l), pred(*r)))])),
                (Bound::Excluded(l), Bound::Included(r)) =>
                    Err((input.location, vec![Reason::Expected(format!("[{}-{}]", succ(*l), r))])),
                (Bound::Excluded(c), Bound::Unbounded) =>
                    Err((input.location, vec![Reason::Expected(format!("[{}-]", succ(*c)))])),
                (Bound::Included(l), Bound::Excluded(r)) =>
                    Err((input.location, vec![Reason::Expected(format!("[{}-{}]", l, pred(*r)))])),
                (Bound::Included(l), Bound::Included(r)) =>
                    Err((input.location, vec![Reason::Expected(format!("[{}-{}]", l, r))])),
                (Bound::Included(c), Bound::Unbounded) =>
                    Err((input.location, vec![Reason::Expected(format!("[{}-]", c))])),
                (Bound::Unbounded, Bound::Excluded(c)) =>
                    Err((input.location, vec![Reason::Expected(format!("[-{}]", pred(*c)))])),
                (Bound::Unbounded, Bound::Included(c)) =>
                    Err((input.location, vec![Reason::Expected(format!("[-{}]", c))])),
                (Bound::Unbounded, Bound::Unbounded) =>
                    Err((input.location, vec![])),

            }
        }
    }
}

pub fn not_range<'input, S>(range: impl RangeBounds<char>) -> impl Parser<'input, S, char>
where
    S: Stream<Item = char>,
{
    move |input| {
        let (c, input) = any_single(input)?;
        if !range.contains(&c) {
            Ok((c, input))
        } else {
            match (range.start_bound(), range.end_bound()) {
                (Bound::Excluded(l), Bound::Excluded(r)) =>
                    Err((input.location, vec![Reason::Expected(format!("[^{}-{}]", succ(*l), pred(*r)))])),
                (Bound::Excluded(l), Bound::Included(r)) =>
                    Err((input.location, vec![Reason::Expected(format!("[^{}-{}]", succ(*l), r))])),
                (Bound::Excluded(c), Bound::Unbounded) =>
                    Err((input.location, vec![Reason::Expected(format!("[^{}-]", succ(*c)))])),
                (Bound::Included(l), Bound::Excluded(r)) =>
                    Err((input.location, vec![Reason::Expected(format!("[^{}-{}]", l, pred(*r)))])),
                (Bound::Included(l), Bound::Included(r)) =>
                    Err((input.location, vec![Reason::Expected(format!("[^{}-{}]", l, r))])),
                (Bound::Included(c), Bound::Unbounded) =>
                    Err((input.location, vec![Reason::Expected(format!("[^{}-]", c))])),
                (Bound::Unbounded, Bound::Excluded(c)) =>
                    Err((input.location, vec![Reason::Expected(format!("[^-{}]", pred(*c)))])),
                (Bound::Unbounded, Bound::Included(c)) =>
                    Err((input.location, vec![Reason::Expected(format!("[^-{}]", c))])),
                (Bound::Unbounded, Bound::Unbounded) =>
                    Err((input.location, vec![])),

            }
        }
    }
}

/// Parses a sequence of characters provided by `s`
pub fn string<'input, S>(s: &'input str) -> impl Parser<'input, S, &'input str>
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
        move |input| {
            let (x, input) = self.parse(input)?;
            let (_, input) = whitespace.opt().parse(input)?;
            Ok((x, input))
        }
    }
}

/// parses a string `s` and the whitespace after it
pub fn symbol<S>(s: &'_ str) -> impl Parser<'_, S, &'_ str>
where
    S: Stream<Item = char>,
{
    move |input| string(s).lexeme().parse(input)
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
        let (x, state) = char('-').opt().parse(state)?;
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

pub fn float<S: Stream<Item = char>>(input: PState<'_, S>) -> PResult<'_, S, f64> {
    (move |state| {
        let (x, state) = char('-').opt().parse(state)?;
        let neg = x.is_some();

        let (x, state) = digit.or(char('.')).some().parse(state)?;

        let mut n: f64 = x.into_iter().collect::<String>().parse().unwrap();

        if neg {
            n *= -1.0;
        }

        Ok((n, state))
    })
    .named("Float")
    .parse(input)
}

fn lex_esc_char<S>(input: PState<'_, S>) -> PResult<'_, S, char>
where
    S: Stream<Item = char>,
{
    let (x, input) = any_single.parse(input)?;
    match x {
        'n' => Ok(('\n', input)),
        'r' => Ok(('\r', input)),
        't' => Ok(('\t', input)),
        '\\' => Ok(('\\', input)),
        '\'' => Ok(('\'', input)),
        '\"' => Ok(('\"', input)),
        '\0' => Ok(('\0', input)),
        _ => Err((input.location, vec![])),
    }
}

fn lex_base_char<S>(input: PState<'_, S>) -> PResult<'_, S, u32>
where
    S: Stream<Item = char>,
{
    let (x, input) = any_single(input)?;
    match x {
        'o' | 'O' => Ok((8, input)),
        'x' | 'X' => Ok((16, input)),
        _ => Err((input.location, vec![])),
    }
}

fn lex_digits<'a, S>(base: u32) -> impl Parser<'a, S, Vec<u32>>
where
    S: Stream<Item = char>,
{
    move |input: PState<'a, S>| satisfy_map(|c| c.to_digit(base)).some().parse(input)
}

pub fn lex_integer<'a, S>(base: u32) -> impl Parser<'a, S, u32>
where
    S: Stream<Item = char>,
{
    move |input: PState<'a, S>| {
        let (n, input) = lex_digits(base).parse(input)?;
        let n = n.into_iter().fold(0, |accum, x| accum * base + x);
        Ok((n, input))
    }
}

pub fn usize<S>(input: PState<'_, S>) -> PResult<'_, S, usize>
where
    S: Stream<Item = char>,
{
    lex_integer(10).map(|x| x as usize).parse(input)
}

fn lex_numeric<S>(input: PState<'_, S>) -> PResult<'_, S, char>
where
    S: Stream<Item = char> + Copy,
{
    let (base, input) = lex_base_char.or_pure(10).parse(input)?;
    let (n, input) = lex_integer(base).parse(input)?;
    match char::from_u32(n) {
        Some(n) => Ok((n, input)),
        None => Err((input.location, vec![])),
    }
}

pub fn eol<S>(input: PState<'_, S>) -> PResult<'_, S, ()>
where
    S: Stream<Item = char>,
{
    char('\n').ignore().or(string("\r\n").ignore()).parse(input)
}

fn lex_char_e<S>(input: PState<'_, S>) -> PResult<'_, S, (char, bool)>
where
    S: Stream<Item = char>,
{
    let (x, input) = any_single.parse(input)?;
    if x == '\\' {
        let (x, input) = lex_esc_char.or(lex_numeric).parse(input)?;
        Ok(((x, true), input))
    } else {
        Ok(((x, false), input))
    }
}

pub fn char_literal<S: Stream<Item = char>>(input: PState<'_, S>) -> PResult<'_, S, char> {
    (move |input| {
        let ((r, _), input) = lex_char_e(input)?;
        Ok((r, input))
    })
    .named("Char Literal")
    .parse(input)
}

pub fn any_with_escapes<S>(s: &str) -> impl Parser<'_, S, char>
where
    S: Stream<Item = char>,
{
    move |input| {
        let (x, input) = none_of(s).parse(input)?;
        if x == '\\' {
            let (x, input) = any_single.parse(input)?;
            match x {
                'n' => Ok(('\n', input)),
                'r' => Ok(('\r', input)),
                't' => Ok(('\t', input)),
                '\\' => Ok(('\\', input)),
                '\'' => Ok(('\'', input)),
                '\"' => Ok(('\"', input)),
                '\0' => Ok(('\0', input)),
                x => {
                    if s.contains(x) {
                        Ok((x, input))
                    } else {
                        Err((
                            input.location,
                            vec![Reason::Expected("Escape Character".to_string())],
                        ))
                    }
                }
            }
        } else {
            Ok((x, input))
        }
    }
}

/// Regex character classes
pub mod class {
    use crate::{
        defs::{PResult, PState, Reason, Stream},
        parsers::char::any_single,
    };

    pub fn word<S>(input: PState<'_, S>) -> PResult<'_, S, char>
    where
        S: Stream<Item = char>,
    {
        let (x, input) = any_single(input)?;
        if x.is_alphanumeric() || x == '_' {
            Ok((x, input))
        } else {
            Err((input.location, Reason::expecteds("word character")))
        }
    }
    pub fn not_word<S>(input: PState<'_, S>) -> PResult<'_, S, char>
    where
        S: Stream<Item = char>,
    {
        let (x, input) = any_single(input)?;
        if !x.is_alphanumeric() && x != '_' {
            Ok((x, input))
        } else {
            Err((input.location, Reason::expecteds("not word character")))
        }
    }

    pub fn digit<S>(input: PState<'_, S>) -> PResult<'_, S, char>
    where
        S: Stream<Item = char>,
    {
        let (x, input) = any_single(input)?;
        if x.is_ascii_digit() {
            Ok((x, input))
        } else {
            Err((input.location, Reason::expecteds("digit character")))
        }
    }

    pub fn not_digit<S>(input: PState<'_, S>) -> PResult<'_, S, char>
    where
        S: Stream<Item = char>,
    {
        let (x, input) = any_single(input)?;
        if !x.is_ascii_digit() {
            Ok((x, input))
        } else {
            Err((input.location, Reason::expecteds("digit character")))
        }
    }

    pub fn space<S>(input: PState<'_, S>) -> PResult<'_, S, char>
    where
        S: Stream<Item = char>,
    {
        let (x, input) = any_single(input)?;
        if x.is_whitespace() {
            Ok((x, input))
        } else {
            Err((input.location, Reason::expecteds("digit character")))
        }
    }

    pub fn not_space<S>(input: PState<'_, S>) -> PResult<'_, S, char>
    where
        S: Stream<Item = char>,
    {
        let (x, input) = any_single(input)?;
        if !x.is_whitespace() {
            Ok((x, input))
        } else {
            Err((input.location, Reason::expecteds("digit character")))
        }
    }

    pub fn word_boundary<S>(input: PState<'_, S>) -> PResult<'_, S, char>
    where
        S: Stream<Item = char>,
    {
        let (x, input) = word(input)?;
        let (_, _) = not_word(input)?;
        Ok((x, input))
    }

    pub fn not_word_boundary<S>(input: PState<'_, S>) -> PResult<'_, S, char>
    where
        S: Stream<Item = char>,
    {
        let (_, input) = not_word(input)?;
        let (x, input) = word(input)?;
        Ok((x, input))
    }
}
