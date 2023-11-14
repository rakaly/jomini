/// A simplified and const generic version of arrayref
#[inline]
fn take<const N: usize>(data: &[u8]) -> [u8; N] {
    debug_assert!(data.len() >= N);
    unsafe { *(data.as_ptr() as *const [u8; N]) }
}

#[inline]
pub(crate) fn get_split<const N: usize>(data: &[u8]) -> Option<([u8; N], &[u8])> {
    data.get(N..).map(|d| (take::<N>(data), d))
}

/// https://youtu.be/wlvKAT7SZIQ?si=EndNPTY6f8oEBS--&t=2426
#[inline]
pub(crate) const fn fast_digit_parse(val: u64) -> Option<u64> {
    let is_digits = ((val & 0xF0F0_F0F0_F0F0_F0F0)
        | ((val.wrapping_add(0x0606_0606_0606_0606) & 0xF0F0_F0F0_F0F0_F0F0) >> 4))
        == 0x3333_3333_3333_3333;

    if !is_digits {
        None
    } else {
        let val = (val & 0x0F0F_0F0F_0F0F_0F0F).wrapping_mul(2561) >> 8;
        let val = (val & 0x00FF_00FF_00FF_00FF).wrapping_mul(6553601) >> 16;
        let val = (val & 0x0000_FFFF_0000_FFFF).wrapping_mul(42949672960001) >> 32;
        Some(val)
    }
}

#[inline]
pub(crate) fn le_u32(data: &[u8]) -> u32 {
    u32::from_le_bytes(take::<4>(data))
}

#[inline]
pub(crate) fn le_u64(data: &[u8]) -> u64 {
    u64::from_le_bytes(take::<8>(data))
}

#[inline(always)]
pub(crate) const fn repeat_byte(b: u8) -> u64 {
    (b as u64) * (u64::MAX / 255)
}

/// From the memchr crate which bases its implementation on several others
#[inline(always)]
pub(crate) fn contains_zero_byte(x: u64) -> bool {
    const LO_U64: u64 = 0x0101010101010101;
    const HI_U64: u64 = 0x8080808080808080;
    x.wrapping_sub(LO_U64) & !x & HI_U64 != 0
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

    #[rstest]
    #[case(*b"14441111", Some(14441111))]
    #[case(*b"14440101", Some(14440101))]
    #[case(*b"1a440101", None)]
    #[case(*b"144a0101", None)]
    #[case(*b"14440a01", None)]
    fn test_fast_digit_parse(#[case] input: [u8; 8], #[case] expected: Option<u64>) {
        assert_eq!(fast_digit_parse(u64::from_le_bytes(input)), expected);
    }
}
