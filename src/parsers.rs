use crate::defs::{SourceSpan, Parser};

pub fn c<'a>(c: &str) -> impl Parser<'a, &str> {
    move |input: &'a str| { 
        let d = &input[0..c.len()];
        if d == c {
            Ok((c, &input[c.len()..]))
        } else {
            Err((SourceSpan {
                start: 0,
                end: 0,
                file: input,
            }, format!("Expected {}", c)))
        }
    }
}

pub fn map<'a, P, F, A, B>(parser: P, map: F) -> impl Parser<'a, B> 
where
    P: Parser<'a, A>,
    F: Fn(A) -> B,
{
    move |input: &'a str| {
        let (result, rest) = parser.parse(input)?;
        Ok((map(result), rest))
    }
}

pub fn and_then<'a, P1, P2, R1, R2>(a: P1, b: P2) -> impl Parser<'a, (R1, R2)>
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>,
{
    move |input: &'a str| {
        let (res1, r) = a.parse(input)?;
        let (res2, rest) = b.parse(r)?;
        Ok(((res1, res2), rest))
    }
}

pub fn ignore_then<'a, P1, P2, R1, R2>(a: P1, b: P2) -> impl Parser<'a, R2> 
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>
{
    move |input| {
        let (_, rest) = a.parse(input)?;
        b.parse(rest)
    }
}

pub fn then_ignore<'a, P1, P2, R1, R2>(a: P1, b: P2) -> impl Parser<'a, R1> 
where
    P1: Parser<'a, R1>,
    P2: Parser<'a, R2>
{
    move |input| {
        let (res, next) = a.parse(input)?;
        let (_, to_ret) = b.parse(next)?;
        Ok((res, to_ret))
    }
}

pub fn one_or_more<'a, P, R>(parser: P) -> impl Parser<'a, Vec<R>>
where
    P: Parser<'a, R>
{
    move |input| {
        let (first, rest) = parser.parse(input)?;
        let mut res = vec![first];
        let mut r = rest;
        while parser.parse(r).is_ok() {
            let (v, rest) = parser.parse(r)?;
            r = rest;
            res.push(v);
        }
        Ok((res, r))
    }
}

pub fn zero_or_more<'a, P, R>(parser: P) -> impl Parser<'a, Vec<R>>
where
    P: Parser<'a, R>
{
    move |input| {
        let mut res = vec![];
        let mut r = input;
        while parser.parse(r).is_ok() {
            let (v, rest) = parser.parse(r)?;
            r = rest;
            res.push(v);
        }
        Ok((res, r))
    }
}

pub fn padded_by<'a, P, B, R1, R2>(parser: P, by: B) -> impl Parser<'a, R1>
where
    P: Parser<'a, R1>,
    B: Parser<'a, R2>
{
    move |input| {
        let (_, rest) = by.parse(input)?;
        let (res, rest) = parser.parse(rest)?;
        let (_, rest) = by.parse(rest)?;
        Ok((res, rest))
    }
}

pub fn padded_by_opt<'a, P, B, R1, R2>(parser: P, by: B) -> impl Parser<'a, R1>
where
    P: Parser<'a, R1>,
    B: Parser<'a, R2>
{
    let o = opt(by);
    move |input| {
        let (_, rest) = o.parse(input)?;
        let (res, rest) = parser.parse(rest)?;
        let (_, rest) = o.parse(rest)?;
        Ok((res, rest))
    }
}

pub fn opt<'a, P, R>(parser: P) -> impl Parser<'a, Option<R>>
where
    P: Parser<'a, R>
{
    move |input| {
        let res = parser.parse(input).ok();
        Ok(match res {
            Some((res, rest)) => (Some(res), rest),
            None => (None, input)
        })
    }
}
