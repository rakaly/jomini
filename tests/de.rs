#![cfg(feature = "derive")]

use jomini::{BinaryDeserializer, TextDeserializer};
use serde::{
    de::{self, Visitor},
    Deserialize, Deserializer,
};
use std::collections::HashMap;
use std::fmt;

#[test]
fn same_deserializer_for_header_token() {
    #[derive(Deserialize, Debug, PartialEq)]
    struct MyStruct {
        color: Color,
    }

    #[derive(Debug, PartialEq)]
    struct Color {
        red: u8,
        blue: u8,
        green: u8,
    }

    let bin_data = [
        0x3a, 0x05, 0x01, 0x00, 0x43, 0x02, 0x03, 0x00, 0x14, 0x00, 0x6e, 0x00, 0x00, 0x00, 0x14,
        0x00, 0x1b, 0x00, 0x00, 0x00, 0x14, 0x00, 0x1b, 0x00, 0x00, 0x00, 0x04, 0x00,
    ];

    let mut map = HashMap::new();
    map.insert(0x053a, "color");

    let txt_data = b"color = rgb { 110 27 27 }";

    let bin_out: MyStruct = BinaryDeserializer::from_eu4(&bin_data[..], &map).unwrap();
    let txt_out: MyStruct = TextDeserializer::from_windows1252_slice(&txt_data[..]).unwrap();
    assert_eq!(bin_out, txt_out);
    assert_eq!(
        bin_out,
        MyStruct {
            color: Color {
                red: 110,
                blue: 27,
                green: 27,
            }
        }
    );

    impl<'de> Deserialize<'de> for Color {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
        {
            struct ColorVisitor;

            impl<'de> Visitor<'de> for ColorVisitor {
                type Value = Color;

                fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                    formatter.write_str("a color")
                }

                fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
                where
                    A: de::SeqAccess<'de>,
                {
                    let ty = seq.next_element::<&str>()?.expect("value type");
                    match ty {
                        "rgb" => {
                            let (red, green, blue) =
                                seq.next_element::<(u8, u8, u8)>()?.expect("rgb channels");
                            Ok(Color { red, green, blue })
                        }
                        _ => panic!("unexpected color type"),
                    }
                }
            }

            deserializer.deserialize_seq(ColorVisitor)
        }
    }
}
