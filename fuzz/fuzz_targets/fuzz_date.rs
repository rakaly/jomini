#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if data.len() < 4 {
        return;
    }

    let num = i32::from_le_bytes([data[0], data[1], data[2], data[3]]);
    let txt = jomini::Windows1252Encoding::decode(&data[4..]);
    let _ = jomini::common::Date::from_binary(num);
    if let Some(d) = jomini::common::Date::parse_from_str(txt) {
        assert_eq!(d.days_until(&d.add_days(1)), 1);
        assert_eq!(d.days_until(&d.add_days(-1)), -1);
    }
});
