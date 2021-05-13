use proc_macro::TokenStream;

mod regex_parser;
mod regex_parser_ref;

/// Creates regex parsers defined as attributes.
/// There is different behaviour depending on the data.
/// This only works on `structs` and `enums`.
/// # Enum parsing
/// If the fields are named, then the regex has to be named, for unnamed fields, the regex doesn't need
/// names.
/// There is even possible tail recursion.
/// ```
///     use std::{sync::Arc, rc::Rc};
///     use regex_procs::RegexParser;
///     #[derive(Debug, RegexParser, PartialEq)]
///     enum Animal {
///         #[rgx = r"^Wolf\s+(?P<name>[A-Z][a-z]+)(\s+(?P<enemy>.+))?"]
///         Wolf {
///             name: String,
///             enemy: Option<Rc<Animal>>,
///         },
///         #[rgx = r"^Sheep\s+(?P<name>[A-Z][a-z]+)\s+(?P<colour>\d+)(\s+(?P<enemy>.+))?"]
///         Sheep {
///             name: String,
///             colour: u8,
///             enemy: Option<Rc<Animal>>
///         },
///         #[rgx = r"^(?P<name>[A-Z][a-z]+)(\s+(?P<likes>.+))?"]
///         Human {
///             name: String,
///             likes: Option<Arc<Animal>>,
///         }
///     }
///
///
///     let animals = Animal::parse_regex("Fero Sheep Mara 3 Wolf Martin Anca").unwrap();
///     // animals = Human { name: "Fero", likes: Some(Sheep { name: "Mara", colour: 3,
///     //              enemy: Some(Wolf { name: "Martin", enemy: Some(Human { name: "Anca", likes: None }) }) }) }
/// ```
#[proc_macro_derive(RegPar, attributes(rgx, no_chain, no_apply))]
pub fn regex_parser(input: TokenStream) -> TokenStream {
    regex_parser::run(input)
}

#[proc_macro_derive(RegParRef, attributes(rgx, no_chain, no_apply))]
pub fn regex_parser_ref(input: TokenStream) -> TokenStream {
    regex_parser_ref::run(input)
}
