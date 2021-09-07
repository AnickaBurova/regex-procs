use proc_macro::TokenStream;
use proc_macro2::TokenStream as TS;
use syn::{Type, Ident, DeriveInput, Meta, MetaNameValue, Item, Lit, parse_quote, Data, Fields, FieldsNamed, FieldsUnnamed, Stmt, Variant};
use quote::{quote, format_ident};
use std::collections::HashMap;
use proc_macro2::Span;
use crate::regex_parser::get_generics;


pub fn process(obj: DeriveInput) -> TokenStream {
    let mut rgx = Vec::new();
    for attr in obj.attrs.iter() {
        let meta = attr.parse_meta().unwrap();
        if let Meta::NameValue(MetaNameValue { path, lit, .. }) = meta {
            if path.is_ident("rgx") {
                match lit {
                    Lit::Str(lit) => rgx.push(lit.value()),
                    _ => panic!("rgx must be a string literal"),
                }
            }
        }
    }
    if rgx.is_empty() {
        panic!("There are no rgx declared on {}", obj.ident);
    }
    let code = match &obj.data {
        Data::Struct(str) => {
            match &str.fields {
                Fields::Named(fields) => {
                    if rgx.len() == 1 {
                        one_regex_named(&obj, rgx.pop().unwrap(), fields)
                    } else {
                        mul_regex_named(&obj, rgx, fields)
                    }
                    // for field in fields.named.iter() {
                    //     let name = field.ident.as_ref().unwrap();
                    //     outer_fields.insert(name.clone(), field.ty.clone());
                    // }
                }
                Fields::Unnamed(fields) => {
                    if rgx.len() == 1 {
                        one_regex_unnamed(&obj, rgx.pop().unwrap(), fields)
                    } else {
                        mul_regex_unnamed(&obj, rgx, fields)
                    }
                }
                Fields::Unit => {
                   if rgx.len() > 1 {
                       panic!("Only one regex pattern for unit struct to match the pattern");
                   }
                    unit_regex(obj, rgx.pop().unwrap())
                }
            }
        }
        _ => unreachable!("Only structs are supported"),
    };
    TokenStream::from(code)
}

fn one_regex_named(obj: &DeriveInput, rgx: String, fields: &FieldsNamed) -> TS {
    let name = &obj.ident;
    let ( generics, where_clause) = get_generics(obj);
    let expect_msg = format!("Incorrect regex to parse {}", name);
    // for one rgx, we dont need to search for the names or indices, all should be present in the regex
    // let (use_code, from_match, regex_parser) = regex_parsers!();
    let fields = fields.named.iter().map(|field| {
        let name = field.ident.as_ref().unwrap();
        let text = format!("{}", name);
        quote!{#name: Cap::new(cap.name(#text)).convert()}
    }).collect::<Vec<TS>>();
    // FromMatch is only available if we can create the whole struct out of one rgx
    quote!{
        impl#generics regex_parsers::FromMatch for #name#generics
        #where_clause {
            fn from_match<'t>(m: Option<regex::Match<'t>>) -> Self {
                use regex_parsers::*;
                let m = m.expect(#expect_msg);
                Self::parse_regex(m.as_str()).expect(#expect_msg)
            }
        }

        impl#generics regex_parsers::RegexParser for #name#generics
        #where_clause {
            fn parse_regex<'t>(txt: &'t str) -> Option<Self> {
                use regex_parsers::*;
                use regex::Regex;
                lazy_static::lazy_static! {
                    static ref RGX: Regex = Regex::new(#rgx).unwrap();
                }
                if let Some(cap) = RGX.captures(txt) {
                    Some(Self {
                        #(#fields,)*
                    })
                } else {
                    None
                }
            }
        }
    }
}

fn mul_regex_named(obj: &DeriveInput, mut rgx: Vec<String>, fields: &FieldsNamed) -> TS {
    let name = &obj.ident;
    let (generics, where_clause) = get_generics(obj);
    // let (from_match_generics, generics, where_clause) = get_generics(obj);
    // let expect_msg = format!("Incorrect regex to parse {}", name);
    // create N supporting structs for each regex
    // Parsing regex will go in order from the first to the last and each will create more fields in the next
    // supporting structure until the last one will create the main struct
    let mut all_fields = fields.named.iter().map(|field| {
        (field.ident.clone().unwrap(), field.ty.clone())
    }).collect::<HashMap<_,_>>();

    lazy_static::lazy_static! {
        static ref NAMES: regex::Regex = regex::Regex::new(r"\?P<(\w+)>").unwrap();
    }
    let mut prev_fields = Vec::<(Ident, Type)>::new();
    let mut prev_name = None;
    let mut code = Vec::<Item>::new();
    let mut update = Vec::<Stmt>::new();
    let mut statics = Vec::<TS>::new();
    let mut apply_enums = Vec::<Variant>::new();
    let mut apply_rgx = Vec::<TS>::new();
    let mut apply_apply = Vec::<TS>::new();
    let apply_enum_name = format_ident!("{}Apply", name);
    let chain_enum_name = format_ident!("{}Chain", name);
    let mut chain_enums = Vec::<Variant>::new();
    let mut chain_rgx = Vec::<TS>::new();
    let last_index = rgx.len() -1;
    let vis = &obj.vis;
    for (index, rgx) in rgx.drain(..).enumerate() {
        let fields = NAMES.captures_iter(&rgx).map(|cap| {
            let name = Ident::new(&cap[1], Span::call_site());
            if let Some(field) = all_fields.remove(&name) {
                (name, field)
            } else {
                panic!("Incorrect capturing group '{}' in the {} rgx", name, index);
            }
        }).collect::<Vec<_>>();
        let is_last = index == last_index;
        let static_name = format_ident!("{}{}", name.to_string().to_uppercase(), index);
        statics.push(parse_quote!{
            static ref #static_name: regex::Regex = regex::Regex::new(#rgx).unwrap();
        });
        let update_parses = fields.iter().map(|(name, _)| {
            let text = format!("{}", name);
            quote!{ self.#name = Cap::new(cap.name(#text)).convert(); }
        });
        let apply_fields_assign = fields.iter().map(|(name, _)| {
            quote!{ other.#name = #name; }
        });
        let apply_fields = fields.iter().map(|(name, _)| {
            quote!{ #name }
        });
        update.push(parse_quote!{
            if let Some(cap) = #static_name.captures(txt) {
                #(#update_parses)*
                true
            }
        });
        // define the helping structure or nothing if no more fields in the all_fields, because we are
        // finally doing the last parsing
        let sname = format_ident!("{}{}", name, index);
        let prev_field_names = str_field_names(&prev_fields).collect::<Vec<_>>();
        let parses = parse_fields(&fields).collect::<Vec<_>>();
        if prev_fields.is_empty() {
            chain_rgx.push(quote!{
                Self::Start => if let Some(cap) = #static_name.captures(txt) {
                    either::Either::Right(
                        Self::#sname {
                            #(#parses)*
                        }
                    )
                } else {
                    either::Either::Right ( Self::Start )
                }
            });
        } else if !is_last {
            chain_rgx.push(quote!{
                Self::#prev_name { #(#prev_field_names),*} => if let Some(cap) = #static_name.captures(txt) {
                    either::Either::Right(
                        Self::#sname {
                            #(#prev_field_names,)*
                            #(#parses)*
                        }
                    )
                } else {
                    either::Either::Right(
                        Self::#prev_name {
                            #(#prev_field_names),*
                        }
                    )
                }
            });
        } else {
            chain_rgx.push(quote!{
                Self::#prev_name { #(#prev_field_names),*} => if let Some(cap) = #static_name.captures(txt) {
                    either::Either::Left(
                        #name {
                            #(#prev_field_names,)*
                            #(#parses)*
                        }
                    )
                } else {
                    either::Either::Right(
                        Self::#prev_name {
                            #(#prev_field_names),*
                        }
                    )
                }
            });
        }
        let prev_f = str_fields(&prev_fields).collect::<Vec<_>>();
        let current = str_fields(&fields).collect::<Vec<_>>();
        chain_enums.push(parse_quote!{
            #sname {
                #(#prev_f)*
                #(#current)*
            }
        });
        apply_enums.push(parse_quote!{
            #sname {
                #(#current)*
            }
        });
        apply_apply.push(quote! {
            Self::#sname { #(#apply_fields),* } => {
                #(#apply_fields_assign)*
            }
        });

        apply_rgx.push(quote!{
            if let Some(cap) = #static_name.captures(txt) {
                Some(#apply_enum_name::#sname {
                    #(#parses)*
                })
            }
        });
        let next_name = if index != last_index {
            let prev = str_fields(&prev_fields);
            // define the NameX only if the next result is not the main Struct
            code.push(parse_quote! {
                #[derive(Debug)]
                #vis struct #sname {
                    #(#prev)*
                    #(#current)*
                }
            });
            sname.clone()
        } else {
            name.clone()
        };
        if prev_fields.is_empty() {
            // this is the first parsing, function is in the main struct and it is creating Name0
            code.push(parse_quote!{
                impl #name {
                    pub fn parse_regex(txt: &str) -> Option<#sname> {
                        use regex_parsers::*;
                        lazy_static::lazy_static!{
                            static ref RGX: regex::Regex = regex::Regex::new(#rgx).unwrap();
                        }
                        RGX.captures(txt).map(|cap| {
                            #sname {
                                #(#parses)*
                            }
                        })
                    }
                }
            });
        } else {
            // if the parsing doesn't match, the right will return the original object
            let prev = prev_fields.iter().map(|(name, _)| {
                quote!{ #name: self.#name, }
            });
            code.push(parse_quote!{
                impl #prev_name {
                    pub fn parse_regex_chain(self, txt: &str) -> either::Either<#next_name, Self> {
                        use regex_parsers::*;
                        lazy_static::lazy_static!{
                            static ref RGX: regex::Regex = regex::Regex::new(#rgx).unwrap();
                        }
                        if let Some(cap) = RGX.captures(txt) {
                            either::Either::Left(
                                #next_name {
                                    #(#prev)*
                                    #(#parses)*
                                }
                            )
                        } else {
                            either::Either::Right( self )
                        }
                    }
                }
            });
        }
        prev_name = Some(sname);
        prev_fields.extend(fields);
    }

    quote!{
        #[derive(Debug)]
        #vis enum #chain_enum_name#generics {
            Start,
            #(#chain_enums,)*
        }

        impl#generics #chain_enum_name#generics #where_clause {
            #vis fn parse(self, txt: &str) -> either::Either<#name, Self> {
                use regex_parsers::Cap;
                lazy_static::lazy_static! {
                    #(#statics)*
                }
                match self {
                    #(#chain_rgx,)*

                    _ =>  todo!(),
                }
            }
        }

        #[derive(Debug)]
        #vis enum #apply_enum_name#generics {
            #(#apply_enums,)*
        }
        impl#generics #apply_enum_name#generics
         #where_clause {
            #vis fn apply(self, other: &mut #name) {
                match self {
                    #(#apply_apply)*
                }
            }
        }
        impl#generics regex_parsers::RegexParserApply for #name#generics
        #where_clause {
            type Apply = #apply_enum_name;
            fn parse_apply<'t>(txt: &'t str) -> Option<#apply_enum_name> {
                use regex_parsers::Cap;
                lazy_static::lazy_static! {
                    #(#statics)*
                }

                #(#apply_rgx else)*
                { None }
            }
        }

        impl#generics #name#generics
        #where_clause {
            #vis fn parse_update(&mut self, txt: &str) -> bool {
                lazy_static::lazy_static! {
                    #(#statics)*
                }
                use regex_parsers::Cap;

                #(#update else)*
                { false }
            }
            #vis fn start_chain() -> #chain_enum_name {
                #chain_enum_name::Start
            }
        }

        #(#code)*
    }
}

fn str_fields(fields: &[(Ident, Type)]) -> impl Iterator<Item = TS> + '_{
    fields.iter().map(|(name, ty)| quote! { #name: #ty, })
}

fn str_field_names(fields: &[(Ident, Type)]) -> impl Iterator<Item = Ident> + '_{
    fields.iter().map(|(name, _ty)| name.clone())
}


fn parse_fields(fields: &[(Ident, Type)]) -> impl Iterator<Item = TS> + '_ {
    fields.iter().map(|(name, _)| {
        let text = format!("{}", name);
        quote!{ #name: Cap::new(cap.name(#text)).convert(), }
    })
}

fn one_regex_unnamed(obj: &DeriveInput, rgx: String, fields: &FieldsUnnamed) -> TS {
    let name = &obj.ident;
    let (generics, where_clause) = get_generics(obj);
    let expect_msg = format!("Incorrect regex to parse {}", name);
    let fields = (0..fields.unnamed.len()).map(|index|{
        //fields.unnamed.iter().enumerate().map(|(index, field)| {
        let index = index + 1;
        quote!{Cap::new(cap.get(#index)).convert()}
    });
    // for one rgx, we dont need to search for the names or indices, all should be present in the regex
    // FromMatch is only available if we can create the whole struct out of one rgx
    quote!{
        impl#generics regex_parsers::FromMatch for #name#generics
        #where_clause {
            fn from_match<'t>(m: Option<regex::Match<'t>>) -> Self {
                use regex_parsers::*;
                let m = m.expect(#expect_msg);
                Self::parse_regex(m.as_str()).expect(#expect_msg)
            }
        }

        impl#generics regex_parsers::RegexParser for #name#generics
        #where_clause {
            fn parse_regex<'t>(txt: &str) -> Option<Self> {
                use regex_parsers::*;
                use regex::Regex;
                lazy_static::lazy_static! {
                    static ref RGX: Regex = Regex::new(#rgx).unwrap();
                }
                if let Some(cap) = RGX.captures(txt) {
                    Some(Self (
                        #(#fields,)*
                    ))
                } else {
                    None
                }
            }
        }
    }
}

fn mul_regex_unnamed(_obj: &DeriveInput, _rgx: Vec<String>, _fields: &FieldsUnnamed) -> TS {
    // let name = &obj.ident;
    // let expect_msg = format!("Incorrect regex to parse {}", name);
    // let (use_code, from_match, regex_parser) = regex_parsers!();
    quote!{}
}

fn unit_regex(obj: DeriveInput, rgx: String) -> TS {
    let name = &obj.ident;
    let (generics, where_clause) = get_generics(&obj);
    let expect_msg = format!("Incorrect regex to parse {}", name);
    quote!{
        impl#generics regex_parsers::FromMatch for #name#generics
        #where_clause {
            fn from_match<'t>(m: Option<regex::Match<'t>>) -> Self {
                use regex_parsers::FromMatch;
                let m = m.expect(#expect_msg);
                Self::parse_regex(m.as_str()).expect(#expect_msg)
            }
        }

        impl#generics regex_parsers::RegexParser for #name#generics
        #where_clause {
            fn parse_regex<'t>(txt: &str) -> Option<Self> {
                use regex_parsers::Cap;
                use regex::Regex;
                lazy_static::lazy_static! {
                    static ref RGX: Regex = Regex::new(#rgx).unwrap();
                }
                if RGX.is_match(txt) {
                    Some(Self)
                } else {
                    None
                }
            }
        }
    }
}
