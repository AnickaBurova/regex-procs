use proc_macro::TokenStream;
use proc_macro2::TokenStream as TS;
use syn::{DeriveInput, Data};
use quote::quote;

//
// #[cfg(not(feature = "no_use_parsers"))]
// fn crate_regex_parser() -> (syn::Item, TS) {
//     let use_code: syn::Item = syn::parse_quote!{use regex_parsers::{Cap, FromMatch};};
//     let crate_path: TS = quote!{regex_parsers::};
//     (use_code, crate_path)
// }
//
// #[cfg(feature = "no_use_parsers")]
// fn crate_regex_parser() -> (Option<syn::Item>, Option<TS>) {
//     (None, None)
// }
//
mod data_struct;
mod data_enum;

pub fn run(input: TokenStream) -> TokenStream {
    let obj: DeriveInput = syn::parse(input).unwrap();
    match &obj.data {
        Data::Struct(_) => {
            data_struct::process(obj)
        }
        Data::Enum(_) => {
            data_enum::process(obj)
        }
        _ => unreachable!("Only structs and enums are supported"),
    }
}

fn get_generics(obj: &DeriveInput) -> (Option<TS>, Option<TS>) {
    if obj.generics.params.is_empty() {
        (None, None)
    } else {
        let generics = obj.generics.clone();

        let params = generics.params.iter().map(|g| g.clone());
        let where_clause = &obj.generics.where_clause.clone();
        (
            Some(quote! { <#(#params),*>}),
            Some(quote! { #where_clause }),
        )
    }
}