use jomini::binary;
use serde::Deserialize;
use std::collections::HashMap;

#[test]
fn test_deserialize_meta() {
    let data = &include_bytes!("../../../assets/fixtures/meta.bin")["EU4bin".len()..];

    #[derive(Deserialize, PartialEq, Eq, Debug)]
    struct Meta {
        campaign_id: String,
    }

    let mut map = HashMap::new();
    map.insert(0x337f, "campaign_id");

    let actual: Meta = binary::de::from_slice(&data[..], &map).unwrap();
    assert_eq!(
        actual,
        Meta {
            campaign_id: String::from("72ce90e3-eff3-4be4-9395-f1c3d33fd1c7"),
        }
    );
}
