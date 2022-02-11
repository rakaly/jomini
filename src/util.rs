/// A simplified and const generic version of arrayref
#[inline]
fn take<const N: usize>(data: &[u8]) -> [u8; N] {
    debug_assert!(data.len() >= N);
    unsafe { *(data.as_ptr() as *const [u8; N]) }
}

#[inline]
pub(crate) fn get_split<const N: usize>(data: &[u8]) -> Option<([u8; N], &[u8])> {
    if N <= data.len() {
        let (head, tail) = data.split_at(N);
        Some((take::<N>(head), tail))
    } else {
        None
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
