use std::{borrow::Borrow, fmt::Debug, path::Path, rc::Rc};

use crate::util::DedupI;

/// Can be seen as sort of a copyable iterator, analogous to
/// uncons operations in other functional languages.
///
/// It's implemented here to more easily provide input for branching
/// operations than with iterators
///
/// # Examples
/// ```
/// if let Some(x,xs) = "fo".uncons() {
///     assert_eq!(x, 'f')
///     assert_eq!(xs, "o")
///     if let Some(x,xs) = xs.uncons() {
///         assert_eq!(x, 'o')
///         assert_eq!(None, xs.uncons())
///     }
/// }
/// ```
pub trait Stream: Copy {
    type Item;

    fn uncons(self) -> Option<(Self::Item, Self)>;
}

impl Stream for &str {
    type Item = char;

    fn uncons(self) -> Option<(Self::Item, Self)> {
        let c = self.chars().next()?;
        Some((c, &self[1..]))
    }
}

impl<T: Copy> Stream for &[T] {
    type Item = T;

    fn uncons(self) -> Option<(Self::Item, Self)> {
        let x = *self.first()?;
        Some((x, self.get(1..)?))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourceLoc<'file> {
    pub col: usize,
    pub line: usize,
    pub file: &'file Path,
}

impl<'file> PartialOrd for SourceLoc<'file> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<'file> Ord for SourceLoc<'file> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.line.cmp(&other.line) {
            std::cmp::Ordering::Equal => {}
            ord => return ord,
        }
        self.col.cmp(&other.col)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct PState<'input, S> {
    pub input: S,
    pub location: SourceLoc<'input>,
}

#[derive(Debug, Clone)]
pub enum Reason {
    Expected(String),
    Unexpected(String),
    Message(String),
}

pub type PSuccess<'input, S, O> = (O, PState<'input, S>);
pub type PFailure<'input> = (SourceLoc<'input>, Vec<Reason>);

pub type PResult<'input, S, O> = Result<PSuccess<'input, S, O>, PFailure<'input>>;

pub fn pretty_print_error(error: &PFailure<'_>) -> String {
    let mut top_line = format!(
        "{:?} (line {}, column {}):",
        error.0.file, error.0.line, error.0.col
    );

    let unexpected = error
        .1
        .iter()
        .filter_map(|x| match x {
            Reason::Unexpected(e) if e.is_empty() => Some("EOF"),
            Reason::Unexpected(e) => Some(e),
            _ => None,
        })
        .dedup()
        .fold("".to_owned(), |acc, x| {
            if acc.is_empty() {
                format!("unexpected {x:?}")
            } else {
                format!("{acc}\nunexpected {x:?}")
            }
        });

    if !unexpected.is_empty() {
        top_line += "\n";
        top_line += &unexpected;
    }

    let expected = error
        .1
        .iter()
        .filter_map(|x| match x {
            Reason::Expected(e) => Some(format!("{e:?}")),
            _ => None,
        })
        .dedup()
        .collect::<Vec<String>>()
        .join(" or ");

    if !expected.is_empty() {
        top_line += "\nexpected ";
        top_line += &expected;
    }

    let messages = error
        .1
        .iter()
        .filter_map(|x| match x {
            Reason::Message(e) => Some(e),
            _ => None,
        })
        .dedup()
        .fold("".to_owned(), |acc, x| {
            if acc.is_empty() {
                x.to_string()
            } else {
                acc + "\n" + x
            }
        });

    if !messages.is_empty() {
        top_line += "\n";
        top_line += &messages;
    }

    top_line
}

/// A general trait for parsers
pub trait Parser<'a, S, T> {
    /// A direct implementation intended to be ran by other parsers.
    /// If intended to be ran directly by the user, instead use `parse`
    fn parse(&self, input: PState<'a, S>) -> PResult<'a, S, T>;

    /// A wrapper around `run_parser`
    /// `parser.parse(file, input)` runs the parser over `input`. `file` is used only in error messages
    fn run_parser(&self, file: &'a str, input: S) -> PResult<'a, S, T> {
        self.parse(PState {
            input,
            location: SourceLoc {
                col: 1,
                line: 1,
                file: Path::new(file),
            },
        })
    }

    /// Directly runs `self` against `input` and pretty prints the error
    fn test_parse(&self, input: S)
    where
        T: Debug,
    {
        match self.run_parser("<TEST>", input) {
            Ok(x) => println!("Ok({:?})", x.0),
            Err(e) => println!("{}", pretty_print_error(&e)),
        }
    }

    /// Adds an `expected <name>` error message when the parser fails
    /// Intended to bring more readable parser errors to your parser
    fn named(self, name: &str) -> LabeledParser<Self>
    where
        Self: Sized,
    {
        LabeledParser {
            name: name.to_string(),
            parser: self,
        }
    }

    /// The parser `p1.or(p2)` first applies `p1`, if it succeeds then it returns,
    /// if it fails then `p2` is ran instead
    fn either<P, B>(&self, fallback: P) -> impl Parser<'a, S, Result<T, B>>
    where
        S: Copy,
        P: Parser<'a, S, B>,
    {
        move |input| match self.parse(input) {
            Ok((x, xs)) => Ok((Ok(x), xs)),
            Err((src_loc_1, mut reasons1)) => match fallback.parse(input) {
                Ok((x, xs)) => Ok((Err(x), xs)),
                Err((src_loc_2, mut reasons2)) => match src_loc_1.cmp(&src_loc_2) {
                    std::cmp::Ordering::Less | std::cmp::Ordering::Equal => {
                        reasons1.append(&mut reasons2);
                        Err((src_loc_1, reasons1))
                    }
                    std::cmp::Ordering::Greater => {
                        reasons2.append(&mut reasons1);
                        Err((src_loc_2, reasons2))
                    }
                },
            },
        }
    }

    /// The parser `p1.or(p2)` first applies `p1`, if it succeeds then it returns,
    /// if it fails then `p2` is ran instead
    fn or<P>(&self, fallback: P) -> impl Parser<'a, S, T>
    where
        S: Copy,
        P: Parser<'a, S, T>,
    {
        move |input| match self.parse(input) {
            Ok((x, xs)) => Ok((x, xs)),
            Err((src_loc_1, mut reasons1)) => match fallback.parse(input) {
                Ok((x, xs)) => Ok((x, xs)),
                Err((src_loc_2, mut reasons2)) => match src_loc_1.cmp(&src_loc_2) {
                    std::cmp::Ordering::Less | std::cmp::Ordering::Equal => {
                        reasons1.append(&mut reasons2);
                        Err((src_loc_1, reasons1))
                    }
                    std::cmp::Ordering::Greater => {
                        reasons2.append(&mut reasons1);
                        Err((src_loc_2, reasons2))
                    }
                },
            },
        }
    }

    fn or_pure(&self, fallback: T) -> impl Parser<'a, S, T>
    where
        S: Copy,
        T: Clone,
    {
        move |input| match self.parse(input) {
            Ok((x, xs)) => Ok((x, xs)),
            Err(_) => Ok((fallback.clone(), input))
        }
    }

    /// Sequences 2 parsers and stores their results into a tuple
    fn and_then<P, B>(&self, next: P) -> impl Parser<'a, S, (T, B)>
    where
        P: Parser<'a, S, B>,
    {
        move |input| {
            let (x1, input) = self.parse(input)?;
            let (x2, input) = next.parse(input)?;
            Ok(((x1, x2), input))
        }
    }

    /// Maps over the result of `self`. This does not change the amount of input consumed
    fn map<F, B>(&self, f: F) -> impl Parser<'a, S, B>
    where
        F: Fn(T) -> B,
    {
        move |input| {
            let (x, input) = self.parse(input)?;
            Ok((f(x), input))
        }
    }

    /// Replaces the result of parser with a provided value
    fn fconst<B: Clone>(&self, x: B) -> impl Parser<'a, S, B> {
        move |input| {
            let (_, input) = self.parse(input)?;
            Ok((x.clone(), input))
        }
    }

    /// Parses `self` without consuming any input
    fn lookahead(&self) -> impl Parser<'a, S, T>
    where
        S: Copy,
    {
        move |input| {
            let (x, _) = self.parse(input)?;
            Ok((x, input))
        }
    }

    /// Repeatedly runs `self` until it fails
    fn many(&self) -> impl Parser<'a, S, Vec<T>>
    where
        S: Copy,
    {
        move |mut input| {
            let mut results: Vec<T> = Vec::new();
            while let Ok((x, xs)) = self.parse(input) {
                results.push(x);
                input = xs;
            }
            Ok((results, input))
        }
    }

    /// Parses self multiple times until `till` parses
    fn many_till<P, B>(&self, till: P) -> impl Parser<'a, S, Vec<T>>
    where
        P: Parser<'a, S, B>,
        S: Copy,
    {
        // RC'd to use either for better parser errors
        let till = Rc::new(till);
        move |mut input| {
            let mut out: Vec<T> = Vec::new();

            loop {
                let (x, xs) = self.either(till.clone()).parse(input)?;
                match x {
                    Ok(v) => out.push(v),
                    Err(_) => return Ok((out, xs)),
                }

                input = xs;
            }
        }
    }

    /// Repeatedly parse `self` separated by `sep`
    /// Needs to have atleast 1 element
    fn sep_by1<P, B>(&self, sep: &P) -> impl Parser<'a, S, Vec<T>>
    where
        P: Parser<'a, S, B>,
        S: Copy,
        Self: Sized,
    {
        move |input| {
            let (x, input) = self.parse(input)?;
            let (xs, input) = sep.ignore_left(self).many().parse(input)?;
            Ok((std::iter::once(x).chain(xs).collect(), input))
        }
    }

    /// Repeatedly parse `self` separated by `sep`
    fn sep_by<P, B>(&self, sep: &P) -> impl Parser<'a, S, Vec<T>>
    where
        P: Parser<'a, S, B>,
        S: Copy,
        Self: Sized,
    {
        move |input| {
            match self.sep_by1(sep).parse(input) {
                Ok((x,xs)) => Ok((x,xs)),
                Err(_) => Ok((vec![], input))
            }
        }
    }

    /// Ignore parser output, if the original parser consumes input, this also consumes input
    fn ignore(&self) -> impl Parser<'a, S, ()> {
        self.map(|_| ())
    }

    /// ignores the output of `self` in `self.ignore_left(p)`
    /// `self` still consumes input
    fn ignore_left<P, U>(&self, p: &P) -> impl Parser<'a, S, U>
    where
        P: Parser<'a, S, U>,
    {
        move |input| {
            let (_, input) = self.parse(input)?;
            p.parse(input)
        }
    }

    /// ignores the output of `p2` in `self.ignore_left(p)`
    /// `p` still consumes input
    fn ignore_right<P, U>(&self, p: P) -> impl Parser<'a, S, T>
    where
        P: Parser<'a, S, U>,
    {
        move |input| {
            let (x, input) = self.parse(input)?;
            let (_, input) = p.parse(input)?;
            Ok((x, input))
        }
    }

    /// Repeatedly runs `self` until it fails. the parser `self.some()` should succeed atleast once
    fn some(&self) -> impl Parser<'a, S, Vec<T>>
    where
        S: Copy,
    {
        move |input| {
            let (x, mut input) = self.parse(input)?;
            let mut results = vec![x];
            while let Ok((x, xs)) = self.parse(input) {
                results.push(x);
                input = xs;
            }
            Ok((results, input))
        }
    }

    /// parser.opt() succeeds regardless of `parser` succeeds or not.
    /// Returns `Some(_)` on success, and `None` on failure, as well as not consuming input
    fn opt(&self) -> impl Parser<'a, S, Option<T>>
    where
        S: Copy,
    {
        move |input| match self.parse(input) {
            Err(_) => Ok((None, input)),
            Ok((x, s)) => Ok((Some(x), s)),
        }
    }

    /// Creates a parser that parses a minumum of `min` times and a maximum of `max` times
    fn count(&self, min: usize, max: usize) -> impl Parser<'a, S, Vec<T>> 
    where 
        S: Copy,
    {
        move |mut input| {
            let mut out = Vec::new();

            while let Ok((x, new_input)) = self.parse(input) {
                out.push(x);
                if out.len() == max {
                    return Ok((out, new_input));
                }
                input = new_input;
            }

            if out.len() < min {
                match self.parse(input) {
                    Ok(_) => panic!("Should not occur"),
                    Err(e) => Err(e)
                }
            } else {
                Ok((out, input))
            }
        }
    }
}

impl<'a, S, T, F> Parser<'a, S, T> for F
where
    F: Fn(PState<'a, S>) -> PResult<'a, S, T>,
{
    fn parse(&self, state: PState<'a, S>) -> PResult<'a, S, T> {
        self(state)
    }
}

/// A parser for named parsers, intended for more readable parser errors
pub struct LabeledParser<P> {
    name: String,
    parser: P,
}

impl<'a, S, T, P> Parser<'a, S, T> for Rc<P>
where
    P: Parser<'a, S, T>,
{
    fn parse(&self, state: PState<'a, S>) -> PResult<'a, S, T> {
        let x: &P = self.borrow();
        x.parse(state)
    }
}

impl<'a, S, T, P> Parser<'a, S, T> for LabeledParser<P>
where
    P: Parser<'a, S, T>,
{
    fn parse(&self, state: PState<'a, S>) -> PResult<'a, S, T> {
        let mut res = self.parser.parse(state);
        if let Err((_, vec)) = res.as_mut() {
            vec.push(Reason::Expected(self.name.clone()))
        }
        res
    }
}

/// `ParserFail` is a parser that will always fail. Failing with a provided
/// message
pub struct ParserFail {
    pub message: String,
}

impl ParserFail {
    pub fn new(message: &str) -> Self {
        Self {
            message: message.to_string(),
        }
    }
}

impl<'a, S, T> Parser<'a, S, T> for ParserFail {
    fn parse(&self, state: PState<'a, S>) -> PResult<'a, S, T> {
        Err((state.location, vec![Reason::Message(self.message.clone())]))
    }
}

/// `ParserPure` is a parser that will always succeed. Always returning `T`.
/// It does not consume input
pub struct ParserPure<T> {
    pub item: T,
}

impl<T> ParserPure<T> {
    pub fn new(item: T) -> Self {
        Self { item }
    }
}

impl<'a, S, T: Clone> Parser<'a, S, T> for ParserPure<T> {
    fn parse(&self, state: PState<'a, S>) -> PResult<'a, S, T> {
        Ok((self.item.clone(), state))
    }
}

/// Only parses succesfully when at the end of input.
/// It is useful to add this to the end of your parser to ensure that it parses the whole input
pub fn eof<S>(state: PState<'_, S>) -> PResult<'_, S, ()>
where
    S: Stream,
{
    if state.input.uncons().is_some() {
        Err((state.location, vec![Reason::Expected("EOF".to_string())]))
    } else {
        Ok(((), state))
    }
}
