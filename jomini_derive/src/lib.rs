use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, DeriveInput, Field, Ident, Lit, Meta, NestedMeta, Type};

fn is_duplicated(f: &Field) -> bool {
    f.attrs
        .iter()
        .filter(|attr| attr.path.is_ident("jomini"))
        .map(|attr| attr.parse_meta().unwrap())
        .filter_map(|meta| match meta {
            Meta::List(x) => Some(x),
            _ => None,
        })
        .flat_map(|x| x.nested)
        .filter_map(|x| match x {
            NestedMeta::Meta(m) => Some(m.path().clone()),
            _ => None,
        })
        .filter(|p| p.is_ident("duplicated"))
        .any(|_| true)
}

enum DefaultFallback {
    Path(Ident),
    Yes,
    No,
}

fn can_default(f: &Field) -> DefaultFallback {
    if let Type::Path(x) = ungroup(&f.ty) {
        for segment in x.path.segments.iter() {
            if segment.ident == Ident::new("Option", segment.ident.span()) {
                return DefaultFallback::Yes;
            }
        }
    }

    let defattr = f
        .attrs
        .iter()
        .filter(|attr| attr.path.is_ident("jomini"))
        .map(|attr| attr.parse_meta().unwrap())
        .filter_map(|meta| match meta {
            Meta::List(x) => Some(x),
            _ => None,
        })
        .flat_map(|x| x.nested)
        .filter_map(|x| match x {
            NestedMeta::Meta(m) => Some(m),
            _ => None,
        })
        .filter(|m| m.path().is_ident("default"))
        .next();

    defattr
        .map(|meta| match meta {
            Meta::NameValue(mnv) => {
                if let Lit::Str(lit) = mnv.lit {
                    DefaultFallback::Path(lit.parse().unwrap())
                } else {
                    panic!("expected default function to be a string");
                }
            }
            _ => DefaultFallback::Yes,
        })
        .unwrap_or(DefaultFallback::No)
}

fn can_deserialize_with(f: &Field) -> Option<Ident> {
    let defattr = f
        .attrs
        .iter()
        .filter(|attr| attr.path.is_ident("jomini"))
        .map(|attr| attr.parse_meta().unwrap())
        .filter_map(|meta| match meta {
            Meta::List(x) => Some(x),
            _ => None,
        })
        .flat_map(|x| x.nested)
        .filter_map(|x| match x {
            NestedMeta::Meta(m) => Some(m),
            _ => None,
        })
        .filter(|m| m.path().is_ident("deserialize_with"))
        .next();

    defattr.map(|meta| match meta {
        Meta::NameValue(mnv) => {
            if let Lit::Str(lit) = mnv.lit {
                lit.parse().unwrap()
            } else {
                panic!("expected deserialize_with function to be a string");
            }
        }
        _ => panic!("expected name value for deserialize_with"),
    })
}

fn alias(f: &Field) -> Option<String> {
    f.attrs
        .iter()
        .filter(|attr| attr.path.is_ident("jomini"))
        .map(|attr| attr.parse_meta().unwrap())
        .filter_map(|meta| match meta {
            Meta::List(x) => Some(x),
            _ => None,
        })
        .flat_map(|x| x.nested)
        .filter_map(|x| match x {
            NestedMeta::Meta(m) => Some(m),
            _ => None,
        })
        .filter(|m| m.path().is_ident("alias"))
        .filter_map(|meta| match meta {
            Meta::NameValue(mnv) => Some(mnv),
            _ => None,
        })
        .filter_map(|mnv| match mnv.lit {
            Lit::Str(s) => Some(s.value().clone()),
            _ => None,
        })
        .next()
}

fn ungroup(mut ty: &Type) -> &Type {
    while let Type::Group(group) = ty {
        ty = &group.elem;
    }
    ty
}

#[proc_macro_derive(JominiDeserialize, attributes(jomini))]
pub fn derive(input: TokenStream) -> TokenStream {
    let dinput = parse_macro_input!(input as DeriveInput);
    let struct_ident = dinput.ident;

    let syn_struct = match dinput.data {
        syn::Data::Struct(x) => x,
        _ => panic!("Expected struct"),
    };

    let named_fields = match syn_struct.fields {
        syn::Fields::Named(x) => x,
        _ => panic!("Expected named fields"),
    };

    let builder_init = named_fields.named.iter().map(|f| {
        let name = &f.ident;
        let x = &f.ty;
        if !is_duplicated(f) {
            let field_name_opt = format_ident!("{}_opt", name.as_ref().unwrap());
            quote! { let mut #field_name_opt : ::std::option::Option<#x> = None }
        } else {
            quote! { let mut #name : #x = Default::default() }
        }
    });

    let builder_fields = named_fields.named.iter().map(|f| {
        let name = &f.ident;
        let x = &f.ty;
        let name_str = name
            .as_ref()
            .map(|x| x.to_string())
            .unwrap_or_else(|| String::from("unknown"));
        let match_arm = quote! { __Field::#name };

        if !is_duplicated(f) {
            let field_name_opt = format_ident!("{}_opt", name.as_ref().unwrap());

            let des = if let Some(ident) = can_deserialize_with(f) {
                let fncall = quote! { #ident(__deserializer) };
                quote! {{
                    struct __DeserializeWith {
                        value: #x,
                    }
                    impl<'de> ::serde::Deserialize<'de> for __DeserializeWith {
                        fn deserialize<__D>(
                            __deserializer: __D,
                        ) -> ::std::result::Result<Self, __D::Error>
                        where
                            __D: ::serde::Deserializer<'de>,
                        {
                            Ok(__DeserializeWith { value: #fncall? })
                        }
                    }
                    ::serde::de::MapAccess::next_value::<
                        __DeserializeWith,
                    >(&mut __map).map(|x| x.value)
                }}
            } else {
                quote! { serde::de::MapAccess::next_value::<#x>(&mut __map) }
            };

            quote! {
                #match_arm => match #field_name_opt {
                    None => #field_name_opt = Some(#des?),
                    _ => { return Err(<__A::Error as ::serde::de::Error>::duplicate_field(#name_str)); }
                }
            }
        } else {
            let path = match ungroup(x) {
                syn::Type::Path(ty) => &ty.path,
                _ => panic!("expected path"),
            };
            let seg = match path.segments.last() {
                Some(seg) => seg,
                None => panic!("expected segment"),
            };
            let args = match &seg.arguments {
                syn::PathArguments::AngleBracketed(bracketed) => &bracketed.args,
                _ => panic!("expected brackets"),
            };

            let farg = &args[0];

            quote! {
                #match_arm => { (#name).push(serde::de::MapAccess::next_value::<#farg>(&mut __map)?); }
            }
        }
    });

    let field_extract =  named_fields.named.iter().filter(|x| !is_duplicated(x)).map(|f| {
        let name = &f.ident;
        let field_name_opt = format_ident!("{}_opt", name.as_ref().unwrap());
        let name_str = name
            .as_ref()
            .map(|x| x.to_string())
            .unwrap_or_else(|| String::from("unknown"));

        match can_default(f) {
            DefaultFallback::Yes => quote! {
                let #name = (#field_name_opt).unwrap_or_default();
            },
            DefaultFallback::Path(lit) => quote! {
                let #name = (#field_name_opt).unwrap_or_else(#lit);
            },
            DefaultFallback::No => quote! {
                let #name = (#field_name_opt)
                    .ok_or_else(|| <__A::Error as ::serde::de::Error>::missing_field(#name_str))?;
            }
        }
    });

    let field_constructor = named_fields.named.iter().map(|f| {
        let name = &f.ident;
        quote! { #name }
    });

    let field_enums = named_fields.named.iter().map(|f| {
        let name = &f.ident;
        quote! { #name }
    });

    let field_enum_match = named_fields.named.iter().map(|f| {
        let name = &f.ident;
        let name_str = name
            .as_ref()
            .map(|x| x.to_string())
            .unwrap_or_else(|| String::from("unknown"));
        let match_arm = alias(f).unwrap_or_else(|| name_str.to_string());
        let field_ident = quote! { __Field::#name };
        quote! {
            #match_arm => Ok(#field_ident)
        }
    });

    let expecting = format!("struct {}", struct_ident.to_string());
    let struct_ident_str = struct_ident.to_string();

    let field_names: Vec<_> = named_fields
        .named
        .iter()
        .map(|field| {
            field
                .ident
                .as_ref()
                .map(|x| x.to_string())
                .unwrap_or_else(|| String::from("unknown"))
        })
        .collect();

    let output = quote! {
        impl<'de> ::serde::Deserialize<'de> for #struct_ident {
            fn deserialize<__D>(__deserializer: __D) -> ::std::result::Result<Self, __D::Error>
            where __D: ::serde::Deserializer<'de> {
                #[allow(non_camel_case_types)]
                enum __Field {
                    #(#field_enums),* ,
                    __ignore,
                };

                struct __FieldVisitor;
                impl<'de> ::serde::de::Visitor<'de> for __FieldVisitor {
                    type Value = __Field;
                    fn expecting(
                        &self,
                        __formatter: &mut ::std::fmt::Formatter,
                    ) -> ::std::fmt::Result {
                        write!(__formatter, "field identifier")
                    }
                    fn visit_str<__E>(
                        self,
                        __value: &str,
                    ) -> ::std::result::Result<Self::Value, __E>
                    where
                        __E: ::serde::de::Error,
                    {
                        match __value {
                            #(#field_enum_match),* ,
                            _ => Ok(__Field::__ignore),
                        }
                    }
                }

                impl<'de> serde::Deserialize<'de> for __Field {
                    #[inline]
                    fn deserialize<__D>(
                        __deserializer: __D,
                    ) -> std::result::Result<Self, __D::Error>
                    where
                        __D: ::serde::Deserializer<'de>,
                    {
                        ::serde::Deserializer::deserialize_identifier(__deserializer, __FieldVisitor)
                    }
                }

                struct __Visitor;

                impl<'de> ::serde::de::Visitor<'de> for __Visitor {
                    type Value = #struct_ident;

                    fn expecting(&self, formatter: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                        write!(formatter, #expecting)
                    }

                    #[inline]
                    fn visit_map<__A>(
                        self,
                        mut __map: __A,
                    ) -> ::std::result::Result<Self::Value, __A::Error>
                    where
                        __A: ::serde::de::MapAccess<'de>,
                    {
                        #(#builder_init);* ;

                        while let Some(__key) = ::serde::de::MapAccess::next_key::<__Field>(&mut __map)? {
                            match __key {
                                #(#builder_fields),*
                                _ => { ::serde::de::MapAccess::next_value::<::serde::de::IgnoredAny>(&mut __map)?; }
                            }
                        }

                        #(#field_extract);* ;

                        Ok(#struct_ident {
                            #(#field_constructor),*
                        })
                    }
                }

                const FIELDS: &'static [&'static str] = &[ #(#field_names),* ];
                ::serde::de::Deserializer::deserialize_struct(
                    __deserializer,
                    #struct_ident_str,
                    FIELDS,
                    __Visitor
                )
            }
        }
    };
    output.into()
}
