use proc_macro::TokenStream;
use proc_macro2::TokenStream as TS;
use syn::{DeriveInput, Meta, MetaNameValue, parse_quote, Data, Fields, Expr, Variant, Ident, Stmt};
use quote::{quote, format_ident};
use crate::regex_parser::get_generics;


pub fn process(obj: DeriveInput) -> TokenStream {
    let (from_match_generics, generics, where_clause) = get_generics(&obj);
    match obj.data {
        Data::Enum(enm) => {
            let name = obj.ident;
            // rgx captures (or matches) and returning the variant
            let mut code = Vec::<Stmt>::new();
            // lazy_static declaration of regexes
            let mut statics = Vec::<TS>::new();

            for variant in enm.variants.iter() {
                process_variant(&name, variant, &mut code, &mut statics);
            }
            let expect_msg = format!("'t regex to parse {}", name);

            #[cfg(feature = "no_log")]
            let trace1 = quote!{};
            #[cfg(not(feature = "no_log"))]
            let trace1 = quote!{ log::trace!("{} from match {:?}", stringify!(#name), m); };
            #[cfg(feature = "no_log")]
            let trace2 = quote!{};
            #[cfg(not(feature = "no_log"))]
            let trace2 = quote!{ log::trace!("'{}' didn't match {}", txt, stringify!(#name));};

            TokenStream::from(quote!{
                impl<#from_match_generics> regex_parsers::FromMatch<'t> for #name#generics
                 #where_clause {
                    fn from_match(m: Option<regex::Match<'t>>) -> Self {
                        use regex_parsers::FromMatch;
                        #trace1
                        let m = m.expect(#expect_msg);
                        Self::parse_regex(m.as_str()).expect(#expect_msg)
                    }
                }

                impl<#from_match_generics> regex_parsers::RegexParser<'t> for #name#generics
                #where_clause {
                    fn parse_regex(txt: &'t str) -> Option<Self> {
                        use regex_parsers::Cap;
                        lazy_static::lazy_static! {
                            #(#statics)*
                        }
                        #(#code else)*
                        {
                            #trace2
                            None
                        }
                    }
                }
            })
        }
        _ => unreachable!(),
    }
}

fn process_variant(_name: &Ident, variant: &Variant, code: &mut Vec<Stmt>, statics: &mut Vec<TS>) {
    // variant need to have rgx attribute to to parse
    // this rgx needs to be named or unnamed (but properly ordered)
    let rgx = {
        let mut rgx = Vec::new();
        for attr in variant.attrs.iter() {
            let meta = attr.parse_meta().unwrap();
            if let Meta::NameValue(MetaNameValue { path, lit, ..}) = meta {
                if path.is_ident("rgx") {
                    rgx.push(lit);
                }
            }
        }
        if rgx.is_empty() {
            panic!("Variant: {} has no rgx attributes", variant.ident);
        }
        rgx
    };
    let variant_name = &variant.ident;

    let unames = rgx.iter().enumerate().map(|(index, rgx)| {
        let uname = if index == 0 {
            format_ident!("{}", variant.ident.to_string().to_uppercase())
        } else {
            format_ident!("{}{}", variant.ident.to_string().to_uppercase(), index)
        };
        statics.push(quote!{
            static ref #uname: regex::Regex = regex::Regex::new(#rgx).unwrap();
        });
        uname
    });

    match &variant.fields {
        // this requires all the matches to be named as well
        Fields::Named(fields) => {
            let mut parsers = Vec::<Expr>::new();
            let mut names = Vec::<Ident>::new();
            for field in fields.named.iter() {
                let name = field.ident.as_ref().unwrap().clone();
                let text = format!("{}", name);
                names.push(name);
                parsers.push(parse_quote!{
                     Cap::new(cap.name(#text)).convert()
                });
            }
            #[cfg(feature = "no_log")]
            let trace = quote!{};
            #[cfg(not(feature = "no_log"))]
            let trace = quote!{ log::trace!("{}: {:?}",stringify!(#variant_name), cap);};

            for uname in unames {
                code.push(parse_quote! {
                    if let Some(cap) = #uname.captures(txt)  {
                        #trace
                        Some(Self::#variant_name {
                            #(#names: #parsers,)*
                        })
                    }
                });
            }
        }
        // this requires all the matches to be properly indexed
        Fields::Unnamed(fields) => {
            let mut parsers = Vec::<Expr>::new();
            for (index, _field) in fields.unnamed.iter().enumerate() {
                let index = index + 1;
                parsers.push(parse_quote!{
                     Cap::new(cap.get(#index)).convert()
                });
            }
            #[cfg(feature = "no_log")]
                let trace = quote!{};
            #[cfg(not(feature = "no_log"))]
                let trace = quote!{ log::trace!("{}: {:?}",stringify!(#variant_name), cap);};
            for uname in unames {
                code.push(parse_quote! {
                    if let Some(cap) = #uname.captures(txt)  {
                        #trace
                        Some(Self::#variant_name (
                            #(#parsers,)*
                        ))
                    }
                });
            }
        }
        // this is just a pattern match without captures
        Fields::Unit => {
            for uname in unames {
                code.push(parse_quote! {
                    if #uname.is_match(txt) {
                        Some(Self::#variant_name)
                    }
                })
            }
        }
    }
}