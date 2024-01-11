use std::fmt::Display;

use combi::{
    defs::{eof, pretty_print_error, PResult, PState, Parser, Reason, Branching, Repeating, Sequential},
    parsers::char::*,
};

#[derive(Debug, Clone, Copy)]
enum QuantifierRange {
    Exact(usize),
    From(usize),
    FromTo(usize, usize),
}

#[derive(Debug, Clone, Copy)]
enum CharClass {
    Word,
    NotWord,
    Digit,
    NotDigit,
    Whitespace,
    NotWhitespace,
    WordBoundary,
    NotWordBoundary,
}

impl Display for CharClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            CharClass::Word => write!(f, "\\w"),
            CharClass::NotWord => write!(f, "\\W"),
            CharClass::Digit => write!(f, "\\d"),
            CharClass::NotDigit => write!(f, "\\D"),
            CharClass::Whitespace => write!(f, "\\s"),
            CharClass::NotWhitespace => write!(f, "\\S"),
            CharClass::WordBoundary => write!(f, "\\b"),
            CharClass::NotWordBoundary => write!(f, "\\B"),
        }
    }
}

impl CharClass {
    fn negate(self) -> CharClass {
        match self {
            CharClass::Word => CharClass::NotWord,
            CharClass::NotWord => CharClass::Word,
            CharClass::Digit => CharClass::NotDigit,
            CharClass::NotDigit => CharClass::Digit,
            CharClass::Whitespace => CharClass::NotWhitespace,
            CharClass::NotWhitespace => CharClass::Whitespace,
            CharClass::WordBoundary => CharClass::NotWordBoundary,
            CharClass::NotWordBoundary => CharClass::WordBoundary,
        }
    }
}

#[derive(Debug, Clone)]
enum CharSetItem {
    Char(char),
    Class(CharClass),
    Range(char, char),
}

#[derive(Debug, Clone)]
enum CharSet {
    CharSet(Vec<CharSetItem>),
    NegatedSet(Vec<CharSetItem>),
}

#[derive(Debug, Clone)]
enum RegexToken {
    Dot,
    Plus(Box<RegexToken>),
    Star(Box<RegexToken>),
    Optional(Box<RegexToken>),
    Quantifier(Box<RegexToken>, QuantifierRange),
    Beginning,
    End,
    Character(char),
    CaptureGroup(Vec<RegexToken>),
    CharacterClass(CharClass),
    CharacterSet(CharSet),
    Alternation(Vec<RegexToken>, Vec<RegexToken>),
}

type Output<'a, T> = PResult<'a, &'a str, T>;
type Input<'a> = PState<'a, &'a str>;

fn dot(input: Input<'_>) -> Output<'_, RegexToken> {
    char('.').fconst(RegexToken::Dot).parse(input)
}

fn beginning(input: Input<'_>) -> Output<'_, RegexToken> {
    char('^').fconst(RegexToken::Beginning).parse(input)
}

fn end(input: Input<'_>) -> Output<'_, RegexToken> {
    char('$').fconst(RegexToken::End).parse(input)
}

fn character(input: Input<'_>) -> Output<'_, RegexToken> {
    any_with_escapes("+*?^$.[]{}()|/")
        .map(RegexToken::Character)
        .parse(input)
}

fn capture_group(input: Input<'_>) -> Output<'_, RegexToken> {
    let (_, input) = char('(').parse(input)?;
    let (v, input) = regex_expr.parse(input)?;
    let (_, input) = char(')').parse(input)?;
    Ok((RegexToken::CaptureGroup(v), input))
}

fn char_class(input: Input<'_>) -> Output<'_, CharClass> {
    let (c, input) = any_single(input)?;
    if c == '\\' {
        let (c, input) = any_single(input)?;
        match c {
            'w' => Ok((CharClass::Word, input)),
            'W' => Ok((CharClass::NotWord, input)),
            'd' => Ok((CharClass::Digit, input)),
            'D' => Ok((CharClass::NotDigit, input)),
            's' => Ok((CharClass::Whitespace, input)),
            'S' => Ok((CharClass::NotWhitespace, input)),
            'b' => Ok((CharClass::WordBoundary, input)),
            'B' => Ok((CharClass::NotWordBoundary, input)),
            _ => Err((input.location, vec![Reason::expected("Character Class")])),
        }
    } else {
        Err((input.location, vec![Reason::expected("Character Class")]))
    }
}

fn character_class(input: Input<'_>) -> Output<'_, RegexToken> {
    char_class.map(RegexToken::CharacterClass).parse(input)
}

fn char_set_range(input: Input<'_>) -> Output<'_, CharSetItem> {
    let (from, input) = any_with_escapes("\\-]").parse(input)?;
    let (_, input) = char('-').parse(input)?;
    let (to, input) = any_with_escapes("\\-]").parse(input)?;
    Ok((CharSetItem::Range(from, to), input))
}

fn char_set_item(input: Input<'_>) -> Output<'_, CharSetItem> {
    char_class
        .map(CharSetItem::Class)
        .or(char_set_range)
        .or(any_with_escapes("\\]").map(CharSetItem::Char))
        .parse(input)
}

fn character_set(input: Input<'_>) -> Output<'_, RegexToken> {
    let (_, input) = char('[').parse(input)?;
    let (negated, input) = char('^').opt().map(|x| x.is_some()).parse(input)?;
    let (set, input) = char_set_item.many().parse(input)?;
    let (_, input) = char(']').parse(input)?;
    if negated {
        Ok((RegexToken::CharacterSet(CharSet::NegatedSet(set)), input))
    } else {
        Ok((RegexToken::CharacterSet(CharSet::CharSet(set)), input))
    }
}

fn lexeme(input: Input<'_>) -> Output<'_, RegexToken> {
    beginning
        .or(end)
        .or(dot)
        .or(capture_group)
        .or(character_set)
        .or(character_class)
        .or(character)
        .parse(input)
}

fn range(input: Input<'_>) -> Output<'_, QuantifierRange> {
    let (n, input) = usize.parse(input)?;
    let (to, input) = (char(',').ignore_left(&usize.opt())).opt().parse(input)?;
    match to {
        None => Ok((QuantifierRange::Exact(n), input)),
        Some(None) => Ok((QuantifierRange::From(n), input)),
        Some(Some(x)) => Ok((QuantifierRange::FromTo(n, x), input)),
    }
}

fn plus(input: Input<'_>) -> Output<'_, RegexToken> {
    let (pattern, input) = lexeme.parse(input)?;
    let (_, input) = char('+').parse(input)?;
    Ok((RegexToken::Plus(Box::new(pattern)), input))
}

fn star(input: Input<'_>) -> Output<'_, RegexToken> {
    let (pattern, input) = lexeme.parse(input)?;
    let (_, input) = char('*').parse(input)?;
    Ok((RegexToken::Star(Box::new(pattern)), input))
}

fn optional(input: Input<'_>) -> Output<'_, RegexToken> {
    let (pattern, input) = lexeme.parse(input)?;
    let (_, input) = char('?').parse(input)?;
    Ok((RegexToken::Optional(Box::new(pattern)), input))
}

fn quantifier(input: Input<'_>) -> Output<'_, RegexToken> {
    let (exp, input) = lexeme.parse(input)?;
    let (_, input) = char('{').parse(input)?;
    let (range, input) = range.parse(input)?;
    let (_, input) = char('}').parse(input)?;
    Ok((RegexToken::Quantifier(Box::new(exp), range), input))
}

fn alternation(input: Input<'_>) -> Output<'_, RegexToken> {
    fn alternationless_regex_token(input: Input<'_>) -> Output<'_, RegexToken> {
        quantifier
            .or(optional)
            .or(plus)
            .or(star)
            .or(lexeme)
            .parse(input)
    }

    let (l, input) = alternationless_regex_token.some().parse(input)?;
    let (_, input) = char('|').parse(input)?;
    let (r, input) = alternationless_regex_token.some().parse(input)?;
    Ok((RegexToken::Alternation(l, r), input))
}

fn regex_token(input: Input<'_>) -> Output<'_, RegexToken> {
    alternation
        .or(quantifier)
        .or(optional)
        .or(plus)
        .or(star)
        .or(lexeme)
        .parse(input)
}

fn regex_expr(input: Input<'_>) -> Output<'_, Vec<RegexToken>> {
    regex_token.some().parse(input)
}

fn parser_from_class<'a>(class: CharClass) -> impl Parser<'a, &'a str, char> {
    move |input| match class {
        CharClass::Word => class::word(input),
        CharClass::NotWord => class::not_word(input),
        CharClass::Digit => class::digit(input),
        CharClass::NotDigit => class::not_digit(input),
        CharClass::Whitespace => class::space(input),
        CharClass::NotWhitespace => class::not_space(input),
        CharClass::WordBoundary => class::word_boundary(input),
        CharClass::NotWordBoundary => class::not_word_boundary(input),
    }
}

fn parser_from_set_item<'a>(item: CharSetItem, negated: bool) -> impl Parser<'a, &'a str, char> {
    move |input| match item.clone() {
        CharSetItem::Char(c) => {
            if negated {
                not_char(c).parse(input)
            } else {
                char(c).parse(input)
            }
        }

        CharSetItem::Class(class) => {
            if negated {
                parser_from_class(class.negate()).parse(input)
            } else {
                parser_from_class(class).parse(input)
            }
        }

        CharSetItem::Range(from, to) => {
            if negated {
                not_range(from..=to).parse(input)
            } else {
                combi::parsers::char::range(from..=to).parse(input)
            }
        }
    }
}

fn set_items_name(set: CharSet) -> String {
    match set.clone() {
        CharSet::CharSet(set) => {
            let mut pattern = String::new();
            for i in set.into_iter() {
                match i {
                    CharSetItem::Char(c) => pattern.push(c),
                    CharSetItem::Range(from, to) => pattern.push_str(&format!("{from}-{to}")),
                    CharSetItem::Class(c) => pattern.push_str(&format!("{c}")),
                }
            }
            format!("[{pattern}]")
        }
        CharSet::NegatedSet(set) => {
            let mut pattern = String::new();
            for i in set.into_iter() {
                match i {
                    CharSetItem::Char(c) => pattern.push(c),
                    CharSetItem::Range(from, to) => pattern.push_str(&format!("{from}-{to}")),
                    CharSetItem::Class(c) => pattern.push_str(&format!("{c}")),
                }
            }
            format!("[^{pattern}]")
        }
    }
}

fn parser_from_set<'a>(set: CharSet) -> impl Parser<'a, &'a str, char> {
    move |input| match set.clone() {
        CharSet::CharSet(set_items) => {
            let (c, _) = any_single(input)?;
            for i in set_items.into_iter() {
                match parser_from_set_item(i, false).parse(input) {
                    Err(_) => {}
                    x => return x,
                }
            }
            Err((
                input.location,
                vec![
                    Reason::Unexpected(c.to_string()),
                    Reason::Expected(set_items_name(set.clone())),
                ],
            ))
        }
        CharSet::NegatedSet(set_items) => {
            let (c, _) = any_single(input)?;
            for i in set_items.into_iter() {
                match parser_from_set_item(i, true).parse(input) {
                    Err(_) => {}
                    x => return x,
                }
            }
            Err((
                input.location,
                vec![Reason::Unexpected(c.to_string()) ,Reason::Expected(set_items_name(set.clone()))],
            ))
        }
    }
}

fn parser_from_token<'a>(tok: RegexToken) -> impl Parser<'a, &'a str, String> {
    move |input| match tok.clone() {
        RegexToken::Dot => none_of("\n").map(|x| x.to_string()).parse(input),
        RegexToken::Beginning => {
            if input.location.col == 1 {
                Ok(("".to_string(), input))
            } else {
                Err((input.location, vec![Reason::expected("Start of Line")]))
            }
        }
        RegexToken::End => eol.or(eof).fconst("".to_string()).parse(input),
        RegexToken::Character(c) => char(c).map(|x| x.to_string()).parse(input),
        RegexToken::Plus(tok) => parser_from_token(*tok)
            .some()
            .map(|x| x.concat())
            .parse(input),
        RegexToken::Star(tok) => parser_from_token(*tok)
            .many()
            .map(|x| x.concat())
            .parse(input),
        RegexToken::Optional(tok) => parser_from_token(*tok)
            .opt()
            .map(|x| x.unwrap_or("".to_string()))
            .parse(input),
        RegexToken::CaptureGroup(toks) => {
            let mut input = input;
            let mut matches = Vec::new();
            for tok in toks.into_iter() {
                let (x, new_input) = parser_from_token(tok).parse(input)?;
                matches.push(x);
                input = new_input;
            }
            Ok((matches.concat(), input))
        }
        RegexToken::Quantifier(tok, range) => {
            let parser = parser_from_token(*tok);
            match range {
                QuantifierRange::Exact(n) => parser.repeat_n(n).map(|x| x.concat()).parse(input),
                QuantifierRange::From(n) => parser
                    .repeat_n(n)
                    .seq(parser.many(), |a, b| a.concat() + &b.concat())
                    .parse(input),
                QuantifierRange::FromTo(from, to) => {
                    parser.count(from, to).map(|x| x.concat()).parse(input)
                }
            }
        }

        RegexToken::CharacterClass(class) => {
            parser_from_class(class).map(|x| x.to_string()).parse(input)
        }
        RegexToken::CharacterSet(char_set) => parser_from_set(char_set)
            .map(|x| x.to_string())
            .parse(input),
        RegexToken::Alternation(toks1, toks2) => {
            fn toks_to_parser<'a>(toks: Vec<RegexToken>) -> impl Parser<'a, &'a str, String> {
                move |mut input| {
                    let mut matches = String::new();
                    for t in toks.clone() {
                        let (s, new_input) = parser_from_token(t).parse(input)?;
                        matches += &s;
                        input = new_input;
                    }
                    Ok((matches, input))
                }
            }
            toks_to_parser(toks1).or(toks_to_parser(toks2)).parse(input)
        }
    }
}

fn regex(pattern: &str) -> impl Parser<'_, &str, String> {
    move |mut input| match regex_expr.exhaustive().run_parser("regex()", pattern) {
        Ok((pattern, _)) if pattern.is_empty() => Ok(("".to_string(), input)),
        Ok((pattern, _)) => {
            let mut matches = Vec::new();
            for tok in pattern.iter() {
                let (x, new_input) = parser_from_token(tok.clone()).parse(input)?;
                if !x.is_empty() {
                    matches.push(x);
                }
                input = new_input;
            }
            Ok((matches.concat(), input))
        }
        Err(e) => panic!("Invalid regex pattern passed: {}", pretty_print_error(&e)),
    }
}

fn main() {
    regex("reg(ex|ular\\sexpressions?)")
        .exhaustive()
        .test_parse("regular expression");
    regex("reg(ex|ular\\sexpressions?)")
        .exhaustive()
        .test_parse("regular expressions");
    regex("reg(ex|ular\\sexpressions?)")
        .exhaustive()
        .test_parse("regex");
    regex("[a-zA-Z0-9._%-]+@[a-zA-Z0-9-]+\\.[a-zA-Z]{2,4}")
        .exhaustive()
        .test_parse("test@mail.com");
    regex("[a-zA-Z0-9._%-]+@[a-zA-Z0-9-]+\\.[a-zA-Z]{2,4}")
        .exhaustive()
        .test_parse("test_mail_2@mail.com");
}
