use proc_macro2::TokenStream;
use quote::quote;
use syn::{parse_quote, Data, DeriveInput, Error, Field, Fields, LitStr, Path};

use crate::attributes::Attributes;

pub fn derive(input: &DeriveInput) -> Result<TokenStream, Error> {
    let name = &input.ident;
    let attributes = Attributes::parse(&input)?;
    match &input.data {
        Data::Struct(data_struct) => {
            if !matches!(data_struct.fields, Fields::Unit) {
                return Err(Error::new_spanned(
                    &input.ident,
                    "struct must be a zero-sized-type to implement `Context`",
                ));
            }
        }
        Data::Enum(_) => {
            return Err(Error::new_spanned(
                &input.ident,
                "enum `Context` derives are currently unsupported",
            ));
        }
        Data::Union(_) => {
            return Err(Error::new_spanned(
                &input.ident,
                "union `Context` derives are currently unsupported",
            ));
        }
    }

    let bigerror = attributes.crate_path;
    Ok(quote! {

        impl std::error::Error for #name {}
        impl ::core::fmt::Debug for #name {
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                f.write_str(stringify!(#name))
            }
        }
        impl ::core::fmt::Display for #name {
            fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
                f.write_str(stringify!(#name))
            }
        }
        impl #bigerror::Reportable for #name {
            fn value() -> Self {
                #name
            }
        }
    })
}
