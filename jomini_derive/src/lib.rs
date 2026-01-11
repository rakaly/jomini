use proc_macro::{Span, TokenStream};
use quote::{format_ident, quote};
use syn::{
    DeriveInput, Error, GenericParam, Ident, Lifetime, LifetimeParam, LitInt, LitStr, Result,
    Token, Type, parse_macro_input, parse_quote, spanned::Spanned,
};

enum DefaultFallback {
    Path(Ident),
    Yes,
    No,
}

fn ungroup(mut ty: &Type) -> &Type {
    while let Type::Group(group) = ty {
        ty = &group.elem;
    }
    ty
}

/// Creates a serde compatible `Deserialize` implementation
///
/// ```rust
/// use jomini_derive::JominiDeserialize;
/// use std::borrow::Cow;
///
/// #[derive(JominiDeserialize)]
/// pub struct Model {
///     #[jomini(default = "default_true")]
///     human: bool,
///     first: Option<u16>,
///     #[jomini(alias = "forth")]
///     fourth: u16,
///     #[jomini(alias = "core", duplicated)]
///     cores: Vec<String>,
///     names: Vec<String>,
/// }
///
/// #[derive(JominiDeserialize)]
/// pub struct BorrowedModel<'a, 'b> {
///     // &str and &[u8] are implicitly borrowed
///     name: &'a str,
///     // Other types need explicit borrow attribute
///     #[jomini(borrow)]
///     description: Cow<'b, str>,
/// }
///
/// fn default_true() -> bool {
///     true
/// }
/// ```
///
/// ## The What
///
/// Most rust programmers should be already familiar with serde's `Deserialize` derive macro. If
/// not, [read the serde docs](https://serde.rs/derive.html).
///
/// The `JominiDeserialize` macro produces an implementation for the serde `Deserialize` trait.
///
/// When unadored with field attributes, both `JominiDeserialize` and `Deserialize` would produce
/// extremely similar `Deserialize` implementations. It's a goal for `JominiDeserialize` to be as
/// compatible as possible.
///
/// The value add for `JominiDeserialize` is the `#[jomini(duplicated)]` field attribute, which can
/// decorate a `Vec<T>` field. The `duplicated` attribute will allow multiple instances of the
/// field, no matter how far separated they are in the data, to be aggregated into a single vector.
/// See "The Why" section below for further info.
///
/// In addition to the `duplicated` attribute, several of the most common serde attributes have
/// been implemented:
///
/// - `#[jomini(alias = "abc")]`
/// - `#[jomini(default)]`
/// - `#[jomini(default = "...")]`
/// - `#[jomini(deserialize_with = "...")]`
/// - `#[jomini(borrow)]`
///
/// Another attribute unique to jomini is `#[jomini(take_last)]` which will take the last occurence
/// of a field. Helpful when a field is duplicated accidentally.
///
/// ## The Why
///
/// Serde's `Deserialize` implementation will raise an error if a field occurs more than once in
/// the data like shown below:
///
/// ```json
/// {
///    "core": "core1",
///    "nums": [1, 2, 3, 4, 5],
///    "core": "core2"
/// }
/// ```
///
/// The `core` field occurs twice in the JSON and serde will be unable to aggregate that into a
/// `Vec<String>` by itself. See this [serde issue](https://github.com/serde-rs/serde/issues/1859)
/// for more information.
///
/// Initial implementations that used the `Deserialize` derive macro natively required a
/// translation layer that would present the aggregated fields to `serde`. This translation layer
/// could have a heavy cost especially as documents increased in size.
///
/// Thus the need was born to be able to generate a `Deserialize` implementation that could
/// aggregate fields. This is why there is a `JominiDeserialize` derive macro and `duplicated` and
/// field attribute.
///
/// ## When to Use
///
/// `JominiDeserialize` is not intended to replace the `Deserialize` derive macro, as
/// `JominiDeserialize` has an incredibly narrow scope -- to allow efficient deserialization of
/// duplicated and non-consecutive fields. Many field / container attributes are not implemented
/// for `JominiDeserialize`, so when needed opt to use serde's `Deserialize` derive macro.
///
/// Since both macros result in an implementation of the `Deserialize` trait for the given data
/// structure, these macros can be used
#[proc_macro_derive(JominiDeserialize, attributes(jomini))]
pub fn derive(input: TokenStream) -> TokenStream {
    let dinput = parse_macro_input!(input as DeriveInput);

    match derive_impl(dinput) {
        Ok(output) => output,
        Err(err) => err.into_compile_error().into(),
    }
}

fn derive_impl(dinput: DeriveInput) -> Result<TokenStream> {
    let struct_ident = dinput.ident.clone();
    let span = struct_ident.span();

    let syn_struct = match dinput.data {
        syn::Data::Struct(x) => x,
        _ => return Err(Error::new(span, "Expected struct")),
    };

    let named_fields = match syn_struct.fields {
        syn::Fields::Named(x) => x,
        _ => return Err(Error::new(span, "Expected named fields")),
    };

    struct FieldAttr {
        ident: Ident,
        display: String,
        typ: Type,
        alias: Option<String>,
        duplicated: bool,
        take_last: bool,
        collect: bool,
        default: DefaultFallback,
        deserialize_with: Option<syn::ExprPath>,
        token: Option<u16>,
        borrow: bool,
    }

    let mut field_attrs = Vec::new();
    for f in &named_fields.named {
        let field_span = f.ident.as_ref().map_or(span, |id| id.span());
        let field_name = f
            .ident
            .as_ref()
            .ok_or_else(|| Error::new(field_span, "Unnamed field in struct"))?
            .to_string();

        let mut duplicated = false;
        let mut take_last = false;
        let mut collect = false;
        let mut default = DefaultFallback::No;
        let mut deserialize_with = None;
        let mut alias = None;
        let mut token = None;
        let mut borrow = false;

        if let Type::Path(x) = ungroup(&f.ty) {
            for segment in x.path.segments.iter() {
                if segment.ident == Ident::new("Option", segment.ident.span()) {
                    default = DefaultFallback::Yes;
                }
            }
        }

        for attr in &f.attrs {
            if !attr.path().is_ident("jomini") {
                continue;
            }

            let parse_result = attr.parse_nested_meta(|meta| {
                if meta.path.is_ident("duplicated") {
                    duplicated = true;
                } else if meta.path.is_ident("take_last") {
                    take_last = true;
                } else if meta.path.is_ident("collect") {
                    collect = true;
                } else if meta.path.is_ident("borrow") {
                    borrow = true;
                } else if meta.path.is_ident("default") {
                    if meta.input.peek(Token![=]) {
                        let val = meta.value()?;
                        let lit: LitStr = val.parse()?;
                        default = DefaultFallback::Path(lit.parse()?);
                    } else {
                        default = DefaultFallback::Yes;
                    }
                } else if meta.path.is_ident("deserialize_with") {
                    let lit: LitStr = meta.value()?.parse()?;
                    deserialize_with = Some(lit.parse()?);
                } else if meta.path.is_ident("alias") {
                    let lit: LitStr = meta.value()?.parse()?;
                    alias = Some(lit.value());
                } else if meta.path.is_ident("token") {
                    let lit: LitInt = meta.value()?.parse()?;
                    token = Some(lit.base10_parse()?);
                    return Ok(());
                }

                Ok(())
            });

            if let Err(e) = parse_result {
                return Err(Error::new(
                    field_span,
                    format!("Failed to parse jomini attribute: {}", e),
                ));
            }
        }

        let attr = FieldAttr {
            ident: f.ident.clone().unwrap(),
            display: field_name,
            typ: f.ty.clone(),
            alias,
            duplicated,
            take_last,
            collect,
            default,
            deserialize_with,
            token,
            borrow,
        };

        if attr.duplicated && attr.take_last {
            return Err(Error::new(
                field_span,
                "Cannot have both duplicated and take_last attributes on a field",
            ));
        }
        if attr.collect && attr.duplicated {
            return Err(Error::new(
                field_span,
                "Cannot have both collect and duplicated attributes on a field",
            ));
        }
        if attr.collect && attr.take_last {
            return Err(Error::new(
                field_span,
                "Cannot have both collect and take_last attributes on a field",
            ));
        }
        if attr.collect && attr.alias.is_some() {
            return Err(Error::new(
                field_span,
                "Cannot have both collect and alias attributes on a field",
            ));
        }
        if attr.collect && attr.token.is_some() {
            return Err(Error::new(
                field_span,
                "Cannot have both collect and token attributes on a field",
            ));
        }

        // Validate borrow attribute usage
        if attr.borrow {
            // Check if the type is suitable for borrowing
            match ungroup(&attr.typ) {
                Type::Reference(_) => {
                    return Err(Error::new(
                        field_span,
                        "borrow attribute is unnecessary for reference types like &str - they are implicitly borrowed",
                    ));
                }
                _ => {
                    // For non-reference types, check if they support borrowing (this is a runtime check in serde)
                    // We could add more validation here for known types like Cow, but it's better to let
                    // the compiler handle this
                }
            }
        }

        field_attrs.push(attr);
    }

    // Check if any fields have the borrow attribute or are implicitly borrowed types like &str
    let has_borrowed_fields = field_attrs
        .iter()
        .any(|f| f.borrow || matches!(ungroup(&f.typ), Type::Reference(_)));

    let mut generics = dinput.generics;
    let mut base_generics = generics.clone();

    // Add 'de lifetime parameter
    let mut de_lifetime = LifetimeParam::new(Lifetime::new("'de", Span::call_site().into()));

    // If we have borrowed fields, add lifetime bounds
    if has_borrowed_fields {
        for param in &generics.params {
            if let GenericParam::Lifetime(lifetime_param) = param {
                de_lifetime.bounds.push(lifetime_param.lifetime.clone());
            }
        }
    }

    base_generics.params = std::iter::once(syn::GenericParam::Lifetime(de_lifetime))
        .chain(base_generics.params)
        .collect();
    let (base_impl, _, base_where) = base_generics.split_for_impl();

    // A poor man approach to know if we need to add the Deserialize<'de> trait
    // bounds to the generic type. Those that are `DeserializedOwned` shouldn't
    // have the constraint. However, if we have borrowed fields, we need lifetime bounds.
    let is_deserialized_owned =
        !generics.params.is_empty() && base_where.is_none() && !has_borrowed_fields;
    if is_deserialized_owned {
        for param in &mut generics.params {
            if let GenericParam::Type(ref mut type_param) = *param {
                type_param
                    .bounds
                    .push(parse_quote!(::serde::Deserialize<'de>));
            }
        }
    }

    let (_, ty_generics, where_clause) = generics.split_for_impl();

    // Create separate generics for the impl with lifetime bounds for borrowed fields
    let mut impl_generics = generics.clone();
    if has_borrowed_fields {
        let lifetime_params: Vec<_> = generics
            .params
            .iter()
            .filter_map(|param| {
                if let GenericParam::Lifetime(lifetime_param) = param {
                    Some(lifetime_param.lifetime.clone())
                } else {
                    None
                }
            })
            .collect();

        for lifetime in lifetime_params {
            let bound: syn::WherePredicate = parse_quote!('de: #lifetime);
            impl_generics.make_where_clause().predicates.push(bound);
        }
    }

    let (_, _, impl_where_clause) = impl_generics.split_for_impl();

    // Create impl generics with 'de first, then struct lifetimes
    let mut de_generics = generics.clone();
    de_generics.params = std::iter::once(syn::GenericParam::Lifetime(LifetimeParam::new(
        Lifetime::new("'de", Span::call_site().into()),
    )))
    .chain(de_generics.params)
    .collect();
    let (de_impl, _, _) = de_generics.split_for_impl();

    let visitor_generics = if generics.params.is_empty() {
        quote! { <#struct_ident> }
    } else {
        quote! { <#struct_ident #ty_generics> }
    };

    // The visitor implementation should have a 'de lifetime
    let visitor_impl = if has_borrowed_fields {
        // For borrowed fields, we need both 'de and the struct's lifetimes
        let mut visitor_params = vec![syn::GenericParam::Lifetime(LifetimeParam::new(
            Lifetime::new("'de", Span::call_site().into()),
        ))];

        for param in &generics.params {
            visitor_params.push(param.clone());
        }

        let visitor_generics_list = syn::Generics {
            lt_token: Some(syn::token::Lt::default()),
            params: visitor_params.into_iter().collect(),
            gt_token: Some(syn::token::Gt::default()),
            where_clause: None,
        };

        let (visitor_impl_generics, _, _) = visitor_generics_list.split_for_impl();
        quote! { #visitor_impl_generics }
    } else if is_deserialized_owned {
        quote! { #de_impl }
    } else {
        quote! { #base_impl }
    };

    let builder_init = field_attrs.iter().map(|f| {
        let name = &f.ident;
        let x = &f.typ;
        if f.collect || f.duplicated {
            match &f.default {
                DefaultFallback::Path(lit) => quote! { let mut #name : #x = (#lit)() },
                _ => quote! { let mut #name : #x = Default::default() },
            }
        } else {
            let field_name_opt = format_ident!("{}_opt", name);
            quote! { let mut #field_name_opt : ::std::option::Option<#x> = None }
        }
    });

    let builder_fields = field_attrs.iter().filter(|f| !f.collect).map(|f| {
        let name = &f.ident;
        let x = &f.typ;
        let name_str = &f.display;
        let match_arm = quote! { __Field::#name };

        if !f.duplicated {
            let field_name_opt = format_ident!("{}_opt", name);

            let des = if let Some(ident) = f.deserialize_with.as_ref() {
                let fncall = quote! { #ident(__deserializer) };
                quote! {{
                    struct __DeserializeWith #ty_generics #where_clause {
                        value: #x,
                    }
                    impl #base_impl ::serde::Deserialize<'de> for __DeserializeWith #ty_generics #where_clause {
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
                        __DeserializeWith #ty_generics,
                    >(&mut __map).map(|x| x.value)
                }}
            } else {
                quote! { serde::de::MapAccess::next_value::<#x>(&mut __map) }
            };

            if !f.take_last {
                Ok(quote! {
                    #match_arm => match #field_name_opt {
                        None => #field_name_opt = Some(#des?),
                        _ => { return Err(<__A::Error as ::serde::de::Error>::duplicate_field(#name_str)); }
                    }
                })
            } else {
                Ok(quote! {
                    #match_arm => #field_name_opt = Some(#des?)
                })
            }
        } else {
            let path = match ungroup(x) {
                syn::Type::Path(ty) => &ty.path,
                _ => return Err(Error::new(x.span(), "duplicated attribute can only be used with path types like Vec<T>")),
            };

            let seg = match path.segments.last() {
                Some(seg) => seg,
                None => return Err(Error::new(path.segments.span(), "expected path segment")),
            };
            let args = match &seg.arguments {
                syn::PathArguments::AngleBracketed(bracketed) => &bracketed.args,
                _ => return Err(Error::new(seg.arguments.span(), "expected angle bracketed arguments")),
            };

            if args.is_empty() {
                return Err(Error::new(seg.arguments.span(), "expected generic argument for duplicated field"));
            }

            let farg = &args[0];

            Ok(match farg {
                syn::GenericArgument::Type(Type::Array(type_array)) => {
                    let elem = &type_array.elem;
                    quote! {
                        #match_arm => { (#name).push(serde::de::MapAccess::next_value::<#elem>(&mut __map)?); }
                    }
                },
                _ => quote! {
                    #match_arm => { (#name).push(serde::de::MapAccess::next_value::<#farg>(&mut __map)?); }
                }
            })
        }
    }).collect::<Result<Vec<_>>>()?;

    let field_extract = field_attrs
        .iter()
        .filter(|x| !x.duplicated && !x.collect)
        .map(|f| {
        let name = &f.ident;
        let field_name_opt = format_ident!("{}_opt", name);
        let name_str = &f.display;

        match &f.default {
            DefaultFallback::Yes => quote! {
                let #name = (#field_name_opt).unwrap_or_default();
            },
            DefaultFallback::Path(lit) => quote! {
                let #name = (#field_name_opt).unwrap_or_else(#lit);
            },
            DefaultFallback::No => quote! {
                let #name = (#field_name_opt)
                    .ok_or_else(|| <__A::Error as ::serde::de::Error>::missing_field(#name_str))?;
            },
        }
    });

    let field_constructor = named_fields.named.iter().map(|f| {
        let name = &f.ident;
        quote! { #name }
    });

    let non_collect_fields: Vec<_> = field_attrs.iter().filter(|f| !f.collect).collect();
    let collect_field = field_attrs.iter().find(|f| f.collect);

    let mut field_enums: Vec<_> = non_collect_fields
        .iter()
        .map(|f| {
            let name = &f.ident;
            quote! { #name }
        })
        .collect();
    if collect_field.is_some() {
        field_enums.push(quote! { __collect(::std::string::String) });
    }

    let field_enum_match: Vec<_> = non_collect_fields
        .iter()
        .map(|f| {
            let name = &f.ident;
            let match_arm = f.alias.clone().unwrap_or_else(|| f.display.clone());
            let field_ident = quote! { __Field::#name };
            quote! {
                #match_arm => Ok(#field_ident)
            }
        })
        .collect();

    let field_enum_token_match = non_collect_fields.iter().filter_map(|f| {
        f.token.map(|token| {
            let name = &f.ident;
            let field_ident = quote! { __Field::#name };
            quote! {
                #token => Ok(#field_ident),
            }
        })
    });

    let token_count = non_collect_fields.iter().filter_map(|x| x.token).count();
    if token_count > 0 && token_count < non_collect_fields.len() {
        return Err(Error::new(
            span,
            format!(
                "{} does not have #[jomini(token = x)] defined for all fields",
                struct_ident
            ),
        ));
    }
    if token_count > 0 {
        return Err(Error::new(
            span,
            format!(
                "{} cannot combine #[jomini(collect)] with #[jomini(token = x)]",
                struct_ident
            ),
        ));
    }

    let deser_request = if token_count > 0 {
        quote! {
            ::serde::Deserializer::deserialize_u16(__deserializer, __FieldVisitor)
        }
    } else {
        quote! {
            ::serde::Deserializer::deserialize_identifier(__deserializer, __FieldVisitor)
        }
    };

    let expecting = format!("struct {}", struct_ident);
    let struct_ident_str = struct_ident.to_string();

    let field_names: Vec<_> = non_collect_fields
        .iter()
        .map(|f| f.display.clone())
        .collect();

    let unknown_str = if collect_field.is_some() {
        quote! { Ok(__Field::__collect(__value.to_string())) }
    } else {
        quote! { Ok(__Field::__ignore) }
    };
    let unknown_string = if collect_field.is_some() {
        quote! { Ok(__Field::__collect(__value)) }
    } else {
        quote! { Ok(__Field::__ignore) }
    };

    let collect_helpers = if let Some(f) = collect_field {
        let collect_ty = &f.typ;
        let collect_value = if let Some(ident) = f.deserialize_with.as_ref() {
            quote! { ::std::convert::Into::into(#ident(__pair)?) }
        } else {
            quote! { ::std::option::Option::Some(::serde::Deserialize::deserialize(__pair)?) }
        };
        quote! {
            struct __CollectPairDeserializer<__D> {
                key: ::std::string::String,
                value: __D,
            }

            struct __CollectSeqAccess<__D> {
                key: ::std::option::Option<::std::string::String>,
                value: ::std::option::Option<__D>,
            }

            impl<'de, __D> ::serde::de::SeqAccess<'de> for __CollectSeqAccess<__D>
            where
                __D: ::serde::de::Deserializer<'de>,
            {
                type Error = __D::Error;

                fn next_element_seed<T>(
                    &mut self,
                    seed: T,
                ) -> ::std::result::Result<::std::option::Option<T::Value>, Self::Error>
                where
                    T: ::serde::de::DeserializeSeed<'de>,
                {
                    if let Some(key) = self.key.take() {
                        let __deserializer =
                            ::serde::de::value::StringDeserializer::<Self::Error>::new(key);
                        return seed.deserialize(__deserializer).map(Some);
                    }

                    if let Some(value) = self.value.take() {
                        return seed.deserialize(value).map(Some);
                    }

                    Ok(None)
                }
            }

            impl<'de, __D> ::serde::de::Deserializer<'de> for __CollectPairDeserializer<__D>
            where
                __D: ::serde::de::Deserializer<'de>,
            {
                type Error = __D::Error;

                fn deserialize_any<V>(
                    self,
                    visitor: V,
                ) -> ::std::result::Result<V::Value, Self::Error>
                where
                    V: ::serde::de::Visitor<'de>,
                {
                    visitor.visit_seq(__CollectSeqAccess {
                        key: Some(self.key),
                        value: Some(self.value),
                    })
                }

                fn deserialize_seq<V>(
                    self,
                    visitor: V,
                ) -> ::std::result::Result<V::Value, Self::Error>
                where
                    V: ::serde::de::Visitor<'de>,
                {
                    self.deserialize_any(visitor)
                }

                fn deserialize_tuple<V>(
                    self,
                    _len: usize,
                    visitor: V,
                ) -> ::std::result::Result<V::Value, Self::Error>
                where
                    V: ::serde::de::Visitor<'de>,
                {
                    self.deserialize_any(visitor)
                }

                fn deserialize_tuple_struct<V>(
                    self,
                    _name: &'static str,
                    _len: usize,
                    visitor: V,
                ) -> ::std::result::Result<V::Value, Self::Error>
                where
                    V: ::serde::de::Visitor<'de>,
                {
                    self.deserialize_any(visitor)
                }

                fn deserialize_newtype_struct<V>(
                    self,
                    _name: &'static str,
                    visitor: V,
                ) -> ::std::result::Result<V::Value, Self::Error>
                where
                    V: ::serde::de::Visitor<'de>,
                {
                    visitor.visit_newtype_struct(self)
                }

                ::serde::forward_to_deserialize_any! {
                    bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
                    bytes byte_buf option unit unit_struct map struct enum identifier ignored_any
                }
            }

            struct __CollectSeed<'a> {
                key: ::std::string::String,
                target: &'a mut #collect_ty,
            }

            impl<'de, 'a> ::serde::de::DeserializeSeed<'de> for __CollectSeed<'a> {
                type Value = ();

                fn deserialize<__D>(
                    self,
                    __deserializer: __D,
                ) -> ::std::result::Result<Self::Value, __D::Error>
                where
                    __D: ::serde::Deserializer<'de>,
                {
                    let __pair = __CollectPairDeserializer {
                        key: self.key,
                        value: __deserializer,
                    };
                    let __value: ::std::option::Option<
                        <#collect_ty as ::jomini::Collect>::Item,
                    > = #collect_value;
                    if let Some(__value) = __value {
                        ::jomini::Collect::collect(self.target, __value);
                    }
                    Ok(())
                }
            }
        }
    } else {
        quote! {}
    };

    let collect_match = if let Some(f) = collect_field {
        let name = &f.ident;
        Some(quote! {
            __Field::__collect(__key) => {
                ::serde::de::MapAccess::next_value_seed(
                    &mut __map,
                    __CollectSeed {
                        key: __key,
                        target: &mut #name,
                    },
                )?;
            }
        })
    } else {
        None
    };

    let mut visitor_match_arms = builder_fields.clone();
    if let Some(collect_match) = collect_match {
        visitor_match_arms.push(collect_match);
    }

    // The visitor struct needs the same generics as the struct being deserialized
    let visitor_struct_generics = quote! { #ty_generics };
    let visitor_struct_where = quote! { #where_clause };

    let output = quote! {
        impl #de_impl ::serde::Deserialize<'de> for #struct_ident #ty_generics #impl_where_clause {
            fn deserialize<__D>(__deserializer: __D) -> ::std::result::Result<Self, __D::Error>
            where __D: ::serde::Deserializer<'de> {
                #[allow(non_camel_case_types)]
                enum __Field {
                    #(#field_enums,)*
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
                            #(#field_enum_match,)*
                            _ => #unknown_str,
                        }
                    }
                    fn visit_borrowed_str<__E>(
                        self,
                        __value: &'de str,
                    ) -> ::std::result::Result<Self::Value, __E>
                    where
                        __E: ::serde::de::Error,
                    {
                        self.visit_str(__value)
                    }
                    fn visit_string<__E>(
                        self,
                        __value: ::std::string::String,
                    ) -> ::std::result::Result<Self::Value, __E>
                    where
                        __E: ::serde::de::Error,
                    {
                        match __value.as_str() {
                            #(#field_enum_match,)*
                            _ => #unknown_string,
                        }
                    }
                    fn visit_u16<__E>(
                        self,
                        __value: u16,
                    ) -> ::std::result::Result<Self::Value, __E>
                    where
                        __E: ::serde::de::Error,
                    {
                        match __value {
                            #(#field_enum_token_match)*
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
                        #deser_request
                    }
                }

                #collect_helpers

                struct __Visitor #visitor_struct_generics #visitor_struct_where {
                    marker: ::core::marker::PhantomData #visitor_generics,
                };

                impl #visitor_impl ::serde::de::Visitor<'de> for __Visitor #visitor_struct_generics #impl_where_clause {
                    type Value = #struct_ident #ty_generics;

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
                                #(#visitor_match_arms,)*
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
                    __Visitor {
                        marker: ::core::marker::PhantomData,
                    }
                )
            }
        }
    };
    Ok(output.into())
}
