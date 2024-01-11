use std::{borrow::Borrow, fmt::Debug, path::Path, rc::Rc};

pub use crate::combinators::branching::Branching;
pub use crate::combinators::repeating::Repeating;
pub use crate::combinators::sequential::Sequential;
pub use crate::error::*;

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
        Some((c, &self[c.len_utf8()..]))
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

pub type PSuccess<'input, S, O> = (O, PState<'input, S>);

/// A general trait for parsers
pub trait Parser<'a, S, T>
where
    S: Stream + Copy,
{
    /// A direct implementation intended to be ran by other parsers.
    /// If intended to be ran directly by the user, instead use `parse`
    fn parse(&self, input: PState<'a, S>) -> PResult<'a, S, T>;

    /// A wrapper around `parse`
    /// `parser.run_parser(file, input)` runs the parser over `input`. `file` is used only in error messages
    #[inline]
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
    #[inline]
    fn test_parse(&self, input: S)
    where
        T: Debug,
    {
        match self.run_parser("<TEST>", input) {
            Ok(x) => println!("{:#?}", x.0),
            Err(e) => println!("{}", pretty_print_error(&e)),
        }
    }

    /// Adds an `expected <name>` error message when the parser fails
    /// Intended to bring more readable parser errors to your parser
    #[inline]
    fn named<Str: ToString>(self, name: Str) -> impl Parser<'a, S, T>
    where
        Self: Sized
    {
        let name = name.to_string();
        move |input| match self.parse(input) {
            Ok((x, input)) => Ok((x, input)),
            Err((loc, mut errors)) => {
                errors.push(Reason::Expected(name.clone()));
                Err((loc, errors))
            }
        }
    }

    /// Maps over the result of `self`. This does not change the amount of input consumed
    #[inline]
    fn map<F, B>(&self, f: F) -> impl Parser<'a, S, B>
    where
        F: Fn(T) -> B,
    {
        move |input| {
            let (x, input) = self.parse(input)?;
            Ok((f(x), input))
        }
    }

    /// Parses `self` without consuming any input
    #[inline]
    fn lookahead(&self) -> impl Parser<'a, S, T> {
        move |input| {
            let (x, _) = self.parse(input)?;
            Ok((x, input))
        }
    }

    /// Makes it sure the parses uses all input
    #[inline]
    fn exhaustive(&self) -> impl Parser<'a, S, T> {
        move |input| {
            let (x, input) = self.parse(input)?;
            let (_, input) = eof.parse(input)?;
            Ok((x, input))
        }
    }

    /// Creates a new parser based on previous input and makes it parse
    #[inline]
    fn bind<B, P, F>(&self, f: F) -> impl Parser<'a, S, B>
    where
        F: Fn(T) -> P,
        P: Parser<'a, S, B>,
    {
        move |input| {
            let (x, input) = self.parse(input)?;
            let parser = f(x);
            parser.parse(input)
        }
    }
}

impl<'a, S, T, F> Parser<'a, S, T> for F
where
    F: Fn(PState<'a, S>) -> PResult<'a, S, T>,
    S: Stream,
{
    fn parse(&self, state: PState<'a, S>) -> PResult<'a, S, T> {
        self(state)
    }
}

impl<'a, S, T, P> Parser<'a, S, T> for Rc<P>
where
    P: Parser<'a, S, T>,
    S: Stream,
{
    fn parse(&self, state: PState<'a, S>) -> PResult<'a, S, T> {
        let x: &P = self.borrow();
        x.parse(state)
    }
}

/// Creates a parser that doesn't consume input and always returns what was passed to it
pub fn pure<'a, T, S>(val: T) -> impl Parser<'a, S, T>
where
    S: Stream,
    T: Clone,
{
    move |input| Ok((val.clone(), input))
}

/// Creates a parser that always fails with a chosen error message
pub fn fail<'a, T, S>(message: &'_ str) -> impl Parser<'a, S, T>
where
    S: Stream,
{
    let message = message.to_string();
    move |input: PState<'a, S>| Err((input.location, vec![Reason::Message(message.clone())]))
}

/// Only parses succesfully when at the end of input.
pub fn eof<S>(state: PState<'_, S>) -> PResult<'_, S, ()>
where
    S: Stream,
{
    if state.input.uncons().is_some() {
        Err((state.location, vec![Reason::expected("EOF")]))
    } else {
        Ok(((), state))
    }
}
