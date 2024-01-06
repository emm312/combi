use combi::{defs::Parser, parsers::{map, c, and_then}};

fn main() {
    println!("{:#?}", and_then(map(c("s"), |c| c.to_uppercase()), c("n")).parse("snf"));
}