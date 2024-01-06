pub trait Parser<'a, T> {
    fn parse(&self, input: &'a str) -> PResult<'a, T>;
}

pub type PResult<'a, O> = Result<(O, &'a str), (SourceSpan<'a>, String)>;

#[derive(Debug, Clone, Copy)]
pub struct SourceSpan<'a> {
    pub start: usize,
    pub end: usize,
    pub file: &'a str,
}

impl<'a, F, O> Parser<'a, O> for F
where
    F: Fn(&'a str) -> PResult<'a, O>,
{
    fn parse(&self, input: &'a str) -> PResult<'a, O> {
        self(input)
    }
}
