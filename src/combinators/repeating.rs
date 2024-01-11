use super::branching::Branching;
use crate::combinators::sequential::Sequential;
use crate::defs::Parser;
use crate::defs::Stream;

pub trait Repeating<'a, S, T>: Parser<'a, S, T>
where
    S: Stream + Copy,
{
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
    fn many_till<P, B>(&self, till: &P) -> impl Parser<'a, S, Vec<T>>
    where
        P: Parser<'a, S, B>,
        S: Copy,
        Self: Sized,
    {
        // RC'd to use either for better parser errors
        move |mut input| {
            let mut out: Vec<T> = Vec::new();

            loop {
                let (x, xs) = till.either(self).parse(input)?;
                match x {
                    Err(v) => out.push(v),
                    Ok(_) => return Ok((out, xs)),
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
        S: Copy + Stream,
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
        S: Copy + Stream,
        Self: Sized,
    {
        move |input| match self.sep_by1(sep).parse(input) {
            Ok((x, xs)) => Ok((x, xs)),
            Err(_) => Ok((vec![], input)),
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
    /// Repeatedly parses self `n` times
    fn repeat_n(&self, n: usize) -> impl Parser<'a, S, Vec<T>>
    where
        S: Copy,
    {
        move |mut input| {
            let mut matches = Vec::new();

            for _ in 0..n {
                let (x, new_input) = self.parse(input)?;
                matches.push(x);
                input = new_input;
            }

            Ok((matches, input))
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
                    Err(e) => Err(e),
                }
            } else {
                Ok((out, input))
            }
        }
    }
}

impl<'a, S, T, P> Repeating<'a, S, T> for P
where
    S: Stream + Copy,
    P: Parser<'a, S, T>,
{
}
