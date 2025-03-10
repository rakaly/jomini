/// A simplified and const generic version of arrayref
#[inline]
fn take<const N: usize>(data: &[u8]) -> [u8; N] {
    debug_assert!(data.len() >= N);
    unsafe { *(data.as_ptr() as *const [u8; N]) }
}

#[inline]
pub(crate) fn get_split<const N: usize>(data: &[u8]) -> Option<(&[u8; N], &[u8])> {
    data.split_first_chunk::<N>()
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

/// https://github.com/llogiq/bytecount/blob/934ea0ef4338f00c797500b10c39f03b3cfc1692/src/integer_simd.rs#L21-L27
#[inline]
const fn bytewise_equal(lhs: u64, rhs: u64) -> u64 {
    let lo = u64::MAX / 0xFF;
    let hi = lo << 7;

    let x = lhs ^ rhs;
    !((((x & !hi) + !hi) | x) >> 7) & lo
}

#[inline]
const fn sum_usize(values: u64) -> u64 {
    let every_other_byte_lo = u64::MAX / 0xFFFF;
    let every_other_byte = every_other_byte_lo * 0xFF;

    // Pairwise reduction to avoid overflow on next step.
    let pair_sum: u64 = (values & every_other_byte) + ((values >> 8) & every_other_byte);

    // Multiplication results in top two bytes holding sum.
    pair_sum.wrapping_mul(every_other_byte_lo) >> ((core::mem::size_of::<u64>() - 2) * 8)
}

#[inline]
pub(crate) const fn count_chunk(value: u64, byte: u8) -> u64 {
    sum_usize(bytewise_equal(value, repeat_byte(byte)))
}

#[inline]
pub(crate) fn leading_whitespace(value: u64) -> u32 {
    let mask1 = repeat_byte(b'\t');
    let mask2 = repeat_byte(b'\n');
    let res1 = value ^ mask1;
    let res2 = value ^ mask2;
    (res1 & res2).trailing_zeros() >> 3
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

    #[rstest]
    #[case(*b"\t\t\t\t\t\t\t\t", 8)]
    #[case(*b"a\t\t\t\t\t\t\t", 0)]
    #[case(*b"\t       ", 1)]
    #[case(*b"\n\na     ", 2)]
    #[case(*b"\n\ta     ", 2)]
    fn test_leading_whitespace(#[case] input: [u8; 8], #[case] expected: u32) {
        let lhs = u64::from_le_bytes(input);
        assert_eq!(leading_whitespace(lhs), expected);
    }

    #[rstest]
    #[case(*b"        ", 0)]
    #[case(*b"   {    ", 1)]
    #[case(*b"   {   {", 2)]
    #[case(*b"{  {   {", 3)]
    #[case(*b"{{{{{{{{", 8)]
    fn test_count_chunk(#[case] input: [u8; 8], #[case] expected: u64) {
        let lhs = u64::from_le_bytes(input);
        let rhs = repeat_byte(b'{');
        assert_eq!(sum_usize(bytewise_equal(lhs, rhs)), expected);
    }

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
