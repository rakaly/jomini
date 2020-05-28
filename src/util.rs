#[inline]
pub(crate) fn le_u16(data: &[u8]) -> u16 {
    let ptr = data.as_ptr() as *const u8 as *const u16;
    unsafe { ::std::ptr::read_unaligned(ptr).to_le() }
}

#[inline]
pub(crate) fn le_u32(data: &[u8]) -> u32 {
    let ptr = data.as_ptr() as *const u8 as *const u32;
    unsafe { ::std::ptr::read_unaligned(ptr).to_le() }
}

#[inline]
pub(crate) fn le_u64(data: &[u8]) -> u64 {
    let ptr = data.as_ptr() as *const u8 as *const u64;
    unsafe { ::std::ptr::read_unaligned(ptr).to_le() }
}

#[inline]
pub(crate) fn le_i32(data: &[u8]) -> i32 {
    let ptr = data.as_ptr() as *const u8 as *const i32;
    unsafe { ::std::ptr::read_unaligned(ptr).to_le() }
}
