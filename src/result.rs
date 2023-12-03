pub enum PResult<'a, T: Clone> {
    Ok(T),
    Err(SourceSpan<'a>, String)
}

#[derive(Debug, Clone, Copy)]
pub struct SourceSpan<'a> {
    pub start: usize,
    pub end: usize,
    pub file: &'a str,
}

impl<'a, T: Clone> PResult<'a, T> {
    pub fn unwrap(&self) -> T {
        match self {
            PResult::Ok(t) => t.clone(),
            PResult::Err(span, msg) => {
                panic!("Error: {} at {}:{}-{}", msg, span.file, span.start, span.end)
            }
        }
    }

    pub fn unwrap_err(&self) -> (SourceSpan<'a>, String) {
        match self {
            PResult::Ok(_) => panic!("Expected error, got Ok"),
            PResult::Err(span, msg) => (span.clone(), msg.clone())
        }
    }
}