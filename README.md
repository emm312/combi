# Combi
Combi is a fast and lightweight rust parser **combi**nator library, featuring speeds of
325gb/s (parsing a 25mb JSON file). It can parse any stream of input if it implements the
`Stream` trait, and has built-in implementations for `&str`, `&[u8]`, and `std::io::Read`. 
In addition, combi also has built in parsers for bytes and characters.

# Features
- Built in error-handling
- Unicode support *
- Built in parsers for things like integers

\* The unicode support is fairly new and needs more testing

# TODO
- [ ] Add more builtin parsers (identifiers, floats, strings)
- [ ] Add unit tests
- [ ] Documentation
- [ ] A small tutorial for new users
- [ ] Error recovery
- [ ] User-defined error types
