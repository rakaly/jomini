const fn create_windows_1252_table() -> [char; 256] {
    let mut table = [0 as char; 256];
    let mut i = 0usize;
    while i < 256 {
        let c = match i {
            128 => '\u{20ac}',
            129 => '\u{81}',
            130 => '\u{201a}',
            131 => '\u{0192}',
            132 => '\u{201e}',
            133 => '\u{2026}',
            134 => '\u{2020}',
            135 => '\u{2021}',
            136 => '\u{02c6}',
            137 => '\u{2030}',
            138 => '\u{0160}',
            139 => '\u{2039}',
            140 => '\u{0152}',
            141 => '\u{8d}',
            142 => '\u{017d}',
            143 => '\u{8f}',
            144 => '\u{90}',
            145 => '\u{2018}',
            146 => '\u{2019}',
            147 => '\u{201c}',
            148 => '\u{201d}',
            149 => '\u{2022}',
            150 => '\u{2013}',
            151 => '\u{2014}',
            152 => '\u{02dc}',
            153 => '\u{2122}',
            154 => '\u{0161}',
            155 => '\u{203a}',
            156 => '\u{0153}',
            157 => '\u{9d}',
            158 => '\u{017e}',
            159 => '\u{0178}',
            i => i as u8 as char,
        };
        table[i] = c;
        i += 1;
    }
    table
}

pub(crate) static WINDOWS_1252: [char; 256] = create_windows_1252_table();

#[inline]
pub(crate) fn is_boundary(b: u8) -> bool {
    boundary(b) != 0
}

#[inline]
pub(crate) fn boundary(b: u8) -> u8 {
    CHARACTER_CLASS[usize::from(b)]
}

const fn create_character_class_table() -> [u8; 256] {
    let mut table = [0u8; 256];
    table[b'\t' as usize] = 1;
    table[b'\n' as usize] = 1;
    table[b'\x0b' as usize] = 1; // \v
    table[b'\x0c' as usize] = 1; // \f
    table[b'\r' as usize] = 1;
    table[b' ' as usize] = 1;
    table[b'!' as usize] = 1;
    table[b'#' as usize] = 1;
    table[b';' as usize] = 1;
    table[b'<' as usize] = 1;
    table[b'=' as usize] = 1;
    table[b'>' as usize] = 1;
    table[b'[' as usize] = 1;
    table[b']' as usize] = 1;
    table[b'}' as usize] = 1;
    table[b'{' as usize] = 1;
    table
}

/// This table serves as a way to encode multiple attributes of a character in a
/// single place. This way we increase the likelihood that the table is in the
/// cache as it is used in multiple call sites.
pub(crate) static CHARACTER_CLASS: [u8; 256] = create_character_class_table();
