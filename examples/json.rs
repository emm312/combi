use std::collections::HashMap;
use std::time::Instant;

use combi::defs::{PResult, PState, Parser, Stream, Branching, Sequential, Repeating};
use combi::parsers::char::*;

#[derive(Debug, Clone)]
pub enum JsonObject {
    Map(HashMap<String, JsonObject>),
    Array(Vec<JsonObject>),
    Num(i64),
    String(String),
    Bool(bool),
    Null,
}

fn bool_p<S>(input: PState<'_, S>) -> PResult<'_, S, bool>
where
    S: Stream<Item = char>,
{
    symbol("true")
        .fconst(true)
        .or(symbol("false").fconst(false))
        .parse(input)
}

fn bool<S>(input: PState<'_, S>) -> PResult<'_, S, JsonObject>
where
    S: Stream<Item = char>,
{
    bool_p.map(JsonObject::Bool).parse(input)
}

fn null<S>(input: PState<'_, S>) -> PResult<'_, S, JsonObject>
where
    S: Stream<Item = char>,
{
    symbol("null").fconst(JsonObject::Null).parse(input)
}

fn num<S>(input: PState<'_, S>) -> PResult<'_, S, JsonObject>
where
    S: Stream<Item = char>,
{
    int.map(JsonObject::Num).parse(input)
}

fn string_literal<S>(input: PState<'_, S>) -> PResult<'_, S, String>
where
    S: Stream<Item = char>,
{
    char('"')
        .ignore_left(&char_literal.many_till(&char('"')))
        .map(|x| x.into_iter().collect())
        .parse(input)
}

fn json_string<S>(input: PState<'_, S>) -> PResult<'_, S, JsonObject>
where
    S: Stream<Item = char>,
{
    string_literal.map(JsonObject::String).parse(input)
}

fn json_array<S>(input: PState<'_, S>) -> PResult<'_, S, JsonObject>
where
    S: Stream<Item = char>,
{
    symbol("[")
        .ignore_left(&json_object.lexeme().sep_by(&symbol(",")))
        .ignore_right(char(']'))
        .map(JsonObject::Array)
        .parse(input)
}

fn json_map<S>(input: PState<'_, S>) -> PResult<'_, S, JsonObject>
where
    S: Stream<Item = char>,
{
    fn json_map_item<S>(input: PState<'_, S>) -> PResult<'_, S, (String, JsonObject)>
    where
        S: Stream<Item = char>,
    {
        string_literal
            .lexeme()
            .ignore_right(symbol(":"))
            .and_then(json_object)
            .parse(input)
    }

    let (x, input) = symbol("{")
        .ignore_left(&json_map_item.lexeme().sep_by(&symbol(",")))
        .ignore_right(symbol("}"))
        .parse(input)?;

    Ok((JsonObject::Map(x.into_iter().collect()), input))
}

fn json_object<S>(input: PState<'_, S>) -> PResult<'_, S, JsonObject>
where
    S: Stream<Item = char>,
{
    json_string
        .or(json_map)
        .or(null)
        .or(bool)
        .or(num)
        .or(json_array)
        .parse(input)
}

fn main() -> std::io::Result<()> {
    let file = std::fs::read_to_string("./examples/sample.json")?;
    let instant = Instant::now();
    json_object.test_parse(file.as_str());
    //println!("{:#?}mb/s", instant.elapsed().as_millis()*1000/(file.len() as f64/1024.0/1024.0) as u128);
    Ok(())
}
