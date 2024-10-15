use quote::ToTokens;
use syn::{meta::ParseNestedMeta, parse_quote, DeriveInput, Error, Path, Token};

fn try_set_attribute<T: ToTokens>(
    attribute: &mut Option<T>,
    value: T,
    name: &'static str,
) -> Result<(), Error> {
    if attribute.is_some() {
        return Err(Error::new_spanned(
            value,
            format!("{name} already specified"),
        ));
    }
    *attribute = Some(value);
    Ok(())
}

#[derive(Default)]
struct Builder {
    pub display: bool,
    pub crate_path: Option<Path>,
}

impl Builder {
    fn parse_meta(&mut self, meta: &ParseNestedMeta<'_>) -> Result<(), Error> {
        if meta.path.is_ident("crate") {
            if meta.input.parse::<Token![=]>().is_ok() {
                let path = meta.input.parse::<Path>()?;
                try_set_attribute(&mut self.crate_path, path, "crate")
            } else if meta.input.is_empty() || meta.input.peek(Token![,]) {
                try_set_attribute(&mut self.crate_path, parse_quote! { crate }, "crate")
            } else {
                Err(meta.error("expected `crate` or `crate = ...`"))
            }
        } else {
            Ok(())
        }
    }
}

pub struct Attributes {
    // TODO port #[display(...)]
    pub has_display: bool,
    pub crate_path: Path,
}
impl Attributes {
    pub fn parse(input: &DeriveInput) -> Result<Self, Error> {
        let mut builder = Builder::default();

        for attr in &input.attrs {
            if attr.path().is_ident("display") {
                builder.display = true;
            }
            if attr.path().is_ident("bigerror") {
                attr.parse_nested_meta(|meta| builder.parse_meta(&meta))?;
            }
        }
        let has_display = builder.display;
        let crate_path = builder
            .crate_path
            .take()
            .unwrap_or_else(|| parse_quote! { ::bigerror });
        Ok(Self {
            has_display,
            crate_path,
        })
    }
}
