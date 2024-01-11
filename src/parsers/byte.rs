use crate::defs::*;

fn update_pos(pos: SourceLoc, c: u8) -> SourceLoc {
    if c == 10 {
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

pub fn satisfy<'input, S, F>(f: F) -> impl Parser<'input, S, u8>
where
    F: Fn(u8) -> bool,
    S: Stream<Item = u8>,
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

pub fn byte_p<'input, S>(c: u8) -> impl Parser<'input, S, u8>
where
    S: Stream<Item = u8>,
{
    satisfy(move |x| x == c).named(format!("{c}"))
}

pub fn bytes_p<'input, S>(s: &'input [u8]) -> impl Parser<'input, S, &'input [u8]>
where
    S: Stream<Item = u8>,
{
    (move |mut input: PState<'input, S>| {
        let mut generated_vec = Vec::new();
        for c in s.iter() {
            if let Some((x, xs)) = input.input.uncons() {
                generated_vec.push(x);
                if x != *c {
                    return Err((
                        input.location,
                        vec![Reason::Unexpected(format!("{generated_vec:?}"))],
                    ));
                }
                input.location = update_pos(input.location, *c);
                input.input = xs;
            } else {
                return Err((
                    input.location,
                    vec![Reason::Unexpected(format!("{generated_vec:?}"))],
                ));
            }
        }
        Ok((s, input))
    })
    .named(format!("{s:?}"))
}
