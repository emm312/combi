use crate::defs::Parser;
use crate::defs::Stream;

pub trait Branching<'a, S, T>: Parser<'a, S, T>
where
    S: Stream + Copy,
{
    /// The parser `p1.or(p2)` first applies `p1`, if it succeeds then it returns,
    /// if it fails then `p2` is ran instead
    #[inline]
    fn either<P, B>(&self, fallback: &P) -> impl Parser<'a, S, Result<T, B>>
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
    #[inline]
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

    /// Places a result in the case of failure
    #[inline]
    fn or_pure(&self, fallback: T) -> impl Parser<'a, S, T>
    where
        S: Copy,
        T: Clone,
    {
        move |input| match self.parse(input) {
            Ok((x, xs)) => Ok((x, xs)),
            Err(_) => Ok((fallback.clone(), input)),
        }
    }

    /// parser.opt() succeeds regardless of `parser` succeeds or not.
    /// Returns `Some(_)` on success, and `None` on failure, as well as not consuming input
    #[inline]
    fn opt(&self) -> impl Parser<'a, S, Option<T>>
    where
        S: Copy,
    {
        move |input| match self.parse(input) {
            Err(_) => Ok((None, input)),
            Ok((x, s)) => Ok((Some(x), s)),
        }
    }

    /// Ignore parser output, if the original parser consumes input, this also consumes input
    #[inline]
    fn ignore(&self) -> impl Parser<'a, S, ()> {
        self.map(|_| ())
    }

    /// Replaces the result of parser with a provided value
    #[inline]
    fn fconst<B: Clone>(&self, x: B) -> impl Parser<'a, S, B> {
        move |input| {
            let (_, input) = self.parse(input)?;
            Ok((x.clone(), input))
        }
    }

}

impl<'a, S, T, P> Branching<'a, S, T> for P
where
    S: Stream + Copy,
    P: Parser<'a, S, T>,
{
}
