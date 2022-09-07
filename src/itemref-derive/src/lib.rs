use proc_macro::{self, TokenStream};
use quote::quote;
use syn::{parse_macro_input, Attribute, DeriveInput, Ident, Meta, NestedMeta};

#[proc_macro_derive(ItemRef, attributes(item, order))]
pub fn derive(input: TokenStream) -> TokenStream {
    let DeriveInput {
        attrs,
        vis: _,
        ident,
        generics: _,
        data,
    } = parse_macro_input!(input);

    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(ref fields),
        ..
    }) = data
    {
        fields
    } else {
        panic!("ItemRef only supports structs");
    };

    let item_eq = item_eq(&attrs, &ident);

    let order = order_field(fields);
    let order_ident = order.ident.as_ref().expect("Tuple structs not supported");

    let output = quote! {
        impl PartialEq for #ident {
            fn eq(&self, other: &Self) -> bool {
                self.id == other.id
            }
        }

        impl Eq for #ident {}

        impl PartialOrd for #ident {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                Some(self.cmp(other))
            }
        }

        impl Ord for #ident {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                other.#order_ident.cmp(&self.#order_ident).then(self.id.cmp(&other.id))
            }
        }

        #item_eq
    };
    output.into()
}

fn order_field(fields: &syn::FieldsNamed) -> &syn::Field {
    fields
        .named
        .iter()
        .find(|field| attr_by_ident(&field.attrs, "order").is_some())
        .expect("Need to specify an order field with `#[order]`")
}

fn attr_by_ident<'a>(attrs: &'a [Attribute], ident: &str) -> Option<&'a Attribute> {
    attrs.iter().find(|attr| {
        if let Some(attr_ident) = attr.path.get_ident() {
            attr_ident == ident
        } else {
            false
        }
    })
}

fn item_eq(attrs: &[Attribute], ident: &Ident) -> proc_macro2::TokenStream {
    let attr = match attr_by_ident(attrs, "item") {
        Some(x) => x,
        None => return quote!(),
    };

    let meta = attr.parse_meta().expect("Failed to parse attribute");
    if let Meta::List(list) = meta {
        let first = list.nested.first().expect("Should have a nested meta");
        if let NestedMeta::Meta(Meta::Path(item)) = first {
            quote! {
                impl PartialEq<#item> for #ident {
                    fn eq(&self, other: &#item) -> bool {
                        self.id == other.id()
                    }
                }
            }
        } else {
            panic!("Unknown meta");
        }
    } else {
        panic!("`item` should be specified with `#[item(SomeItem)]`");
    }
}
