use combi::{parsers::int, defs::Parser};

pub enum JsonObject {
    Map(String, Box<JsonObject>),
    Array(Vec<JsonObject>),
    Num(i64),
    String(String)
}

fn main() {
    
}