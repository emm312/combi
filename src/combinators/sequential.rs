use crate::defs::Parser;
use crate::defs::Stream;

pub trait Sequential<'a, S, T>: Parser<'a, S, T>
where
    S: Stream + Copy,
{
    /// Sequences 2 parsers and stores their results into a tuple
    #[inline]
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

    /// Parses 2 parsers and applies a function over their result
    #[inline]
    fn seq<F, P, B, C>(&self, p: P, f: F) -> impl Parser<'a, S, C>
    where
        F: Fn(T, B) -> C,
        P: Parser<'a, S, B>,
    {
        move |input| {
            let (a, input) = self.parse(input)?;
            let (b, input) = p.parse(input)?;
            Ok((f(a,b), input))
        }
    }

    /// ignores the output of `self` in `self.ignore_left(p)`
    /// `self` still consumes input
    #[inline]
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
    #[inline]
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

}

impl<'a, S, T, P> Sequential<'a, S, T> for P
where
    S: Stream + Copy,
    P: Parser<'a, S, T>,
{
}
