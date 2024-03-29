#![no_main]
use jomini::common::PdsDate;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if data.len() < 4 {
        return;
    }

    let num = i32::from_le_bytes([data[0], data[1], data[2], data[3]]);
    let _ = jomini::common::Date::from_binary(num);
    let _ = jomini::common::DateHour::from_binary(num);
    if let Ok(d) = jomini::common::Date::parse(&data[4..]) {
        match d.game_fmt().to_string().as_str() {
            // I'm not sure how math works when we go across the border
            // as I'm not familiar that EU4 recognizes the zeroth year
            "1.1.1" | "-1.1.1" | "0.1.1" | "-0.1.1" => {}
            _ => {
                assert_eq!(d.days_until(&d.add_days(1)), 1);
                assert_eq!(d.days_until(&d.add_days(-1)), -1);
            }
        }
    }
});
