use crate::util::le_u64;

/// Determines if the given data is ascii. Significantly faster than the standard library for any
/// strings longer than 5 characters. While the standard library processes each individual byte, we
/// process the the data in larger chunks. This allows the function to be up to 8x faster than the
/// standard library
pub fn is_ascii(data: &[u8]) -> bool {
    let mut iter = data.chunks_exact(8 * 4);
    let mut res = iter.all(|data| {
        (le_u64(data) | le_u64(&data[8..]) | le_u64(&data[16..]) | le_u64(&data[24..]))
            & 0x80808080_80808080
            == 0
    });

    let mut iter2 = iter.remainder().chunks_exact(8);
    res &= iter2.all(|data| le_u64(data) & 0x80808080_80808080 == 0);
    res & iter2.remainder().is_ascii()
}

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck_macros::quickcheck;

    #[test]
    fn test_is_ascii() {
        let data = vec![
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
            0, 0, 0,
        ];
        assert!(is_ascii(&data));
    }

    #[quickcheck]
    fn chunks_by_byte_equality(data: Vec<u8>) -> bool {
        data.is_ascii() == is_ascii(&data)
    }
}
