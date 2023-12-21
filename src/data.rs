pub(crate) static WINDOWS_1252: [char; 256] = [
    0 as char,
    1 as char,
    2 as char,
    3 as char,
    4 as char,
    5 as char,
    6 as char,
    7 as char,
    8 as char,
    9 as char,
    10 as char,
    11 as char,
    12 as char,
    13 as char,
    14 as char,
    15 as char,
    16 as char,
    17 as char,
    18 as char,
    19 as char,
    20 as char,
    21 as char,
    22 as char,
    23 as char,
    24 as char,
    25 as char,
    26 as char,
    27 as char,
    28 as char,
    29 as char,
    30 as char,
    31 as char,
    32 as char,
    33 as char,
    34 as char,
    35 as char,
    36 as char,
    37 as char,
    38 as char,
    39 as char,
    40 as char,
    41 as char,
    42 as char,
    43 as char,
    44 as char,
    45 as char,
    46 as char,
    47 as char,
    48 as char,
    49 as char,
    50 as char,
    51 as char,
    52 as char,
    53 as char,
    54 as char,
    55 as char,
    56 as char,
    57 as char,
    58 as char,
    59 as char,
    60 as char,
    61 as char,
    62 as char,
    63 as char,
    64 as char,
    65 as char,
    66 as char,
    67 as char,
    68 as char,
    69 as char,
    70 as char,
    71 as char,
    72 as char,
    73 as char,
    74 as char,
    75 as char,
    76 as char,
    77 as char,
    78 as char,
    79 as char,
    80 as char,
    81 as char,
    82 as char,
    83 as char,
    84 as char,
    85 as char,
    86 as char,
    87 as char,
    88 as char,
    89 as char,
    90 as char,
    91 as char,
    92 as char,
    93 as char,
    94 as char,
    95 as char,
    96 as char,
    97 as char,
    98 as char,
    99 as char,
    100 as char,
    101 as char,
    102 as char,
    103 as char,
    104 as char,
    105 as char,
    106 as char,
    107 as char,
    108 as char,
    109 as char,
    110 as char,
    111 as char,
    112 as char,
    113 as char,
    114 as char,
    115 as char,
    116 as char,
    117 as char,
    118 as char,
    119 as char,
    120 as char,
    121 as char,
    122 as char,
    123 as char,
    124 as char,
    125 as char,
    126 as char,
    127 as char,
    '\u{20ac}',
    '\u{81}',
    '\u{201a}',
    '\u{0192}',
    '\u{201e}',
    '\u{2026}',
    '\u{2020}',
    '\u{2021}',
    '\u{02c6}',
    '\u{2030}',
    '\u{0160}',
    '\u{2039}',
    '\u{0152}',
    '\u{8d}',
    '\u{017d}',
    '\u{8f}',
    '\u{90}',
    '\u{2018}',
    '\u{2019}',
    '\u{201c}',
    '\u{201d}',
    '\u{2022}',
    '\u{2013}',
    '\u{2014}',
    '\u{02dc}',
    '\u{2122}',
    '\u{0161}',
    '\u{203a}',
    '\u{0153}',
    '\u{9d}',
    '\u{017e}',
    '\u{0178}',
    160 as char,
    161 as char,
    162 as char,
    163 as char,
    164 as char,
    165 as char,
    166 as char,
    167 as char,
    168 as char,
    169 as char,
    170 as char,
    171 as char,
    172 as char,
    173 as char,
    174 as char,
    175 as char,
    176 as char,
    177 as char,
    178 as char,
    179 as char,
    180 as char,
    181 as char,
    182 as char,
    183 as char,
    184 as char,
    185 as char,
    186 as char,
    187 as char,
    188 as char,
    189 as char,
    190 as char,
    191 as char,
    192 as char,
    193 as char,
    194 as char,
    195 as char,
    196 as char,
    197 as char,
    198 as char,
    199 as char,
    200 as char,
    201 as char,
    202 as char,
    203 as char,
    204 as char,
    205 as char,
    206 as char,
    207 as char,
    208 as char,
    209 as char,
    210 as char,
    211 as char,
    212 as char,
    213 as char,
    214 as char,
    215 as char,
    216 as char,
    217 as char,
    218 as char,
    219 as char,
    220 as char,
    221 as char,
    222 as char,
    223 as char,
    224 as char,
    225 as char,
    226 as char,
    227 as char,
    228 as char,
    229 as char,
    230 as char,
    231 as char,
    232 as char,
    233 as char,
    234 as char,
    235 as char,
    236 as char,
    237 as char,
    238 as char,
    239 as char,
    240 as char,
    241 as char,
    242 as char,
    243 as char,
    244 as char,
    245 as char,
    246 as char,
    247 as char,
    248 as char,
    249 as char,
    250 as char,
    251 as char,
    252 as char,
    253 as char,
    254 as char,
    255 as char,
];

pub(crate) const BOUNDARY: u8 = 1;
pub(crate) const WHITESPACE: u8 = 2;
pub(crate) const OPERATOR: u8 = 4;
pub(crate) const COMMENT: u8 = 8;

#[inline]
pub(crate) fn is_boundary(b: u8) -> bool {
    CHARACTER_CLASS[usize::from(b)] != 0
}

/// This table probably looks pretty weird but it serves as a way to encode multiple attributes of
/// a character in a single place. This way we increase the likelihood that the table is in the
/// cache as it is used in multiple call sites. The reason why the table has (0 - 0) is so that I
/// can remember what index is being modified -- much better than calculating by hand where an edit
/// needs to go. Unfortunately, clippy hates this and so several lints have to be turned off for
/// it.
#[allow(clippy::eq_op, clippy::identity_op)]
pub(crate) static CHARACTER_CLASS: [u8; 256] = [
    (0 - 0),
    (1 - 1),
    (2 - 2),
    (3 - 3),
    (4 - 4),
    (5 - 5),
    (6 - 6),
    (7 - 7),
    (8 - 8),
    (9 - 9) + WHITESPACE,   // \t
    (10 - 10) + WHITESPACE, // \n
    (11 - 11) + WHITESPACE, // \v
    (12 - 12) + WHITESPACE, // \f
    (13 - 13) + WHITESPACE, // \r
    (14 - 14),
    (15 - 15),
    (16 - 16),
    (17 - 17),
    (18 - 18),
    (19 - 19),
    (20 - 20),
    (21 - 21),
    (22 - 22),
    (23 - 23),
    (24 - 24),
    (25 - 25),
    (26 - 26),
    (27 - 27),
    (28 - 28),
    (29 - 29),
    (30 - 30),
    (31 - 31),
    (32 - 32) + WHITESPACE, // SPACE
    (33 - 33) + OPERATOR,   // !
    (34 - 34),
    (35 - 35) + COMMENT, // #
    (36 - 36),
    (37 - 37),
    (38 - 38),
    (39 - 39),
    (40 - 40),
    (41 - 41),
    (42 - 42),
    (43 - 43),
    (44 - 44),
    (45 - 45),
    (46 - 46),
    (47 - 47),
    (48 - 48),
    (49 - 49),
    (50 - 50),
    (51 - 51),
    (52 - 52),
    (53 - 53),
    (54 - 54),
    (55 - 55),
    (56 - 56),
    (57 - 57),
    (58 - 58),
    (59 - 59),
    (60 - 60) + OPERATOR, // <
    (61 - 61) + OPERATOR, // =
    (62 - 62) + OPERATOR, // >
    (63 - 63),
    (64 - 64),
    (65 - 65),
    (66 - 66),
    (67 - 67),
    (68 - 68),
    (69 - 69),
    (70 - 70),
    (71 - 71),
    (72 - 72),
    (73 - 73),
    (74 - 74),
    (75 - 75),
    (76 - 76),
    (77 - 77),
    (78 - 78),
    (79 - 79),
    (80 - 80),
    (81 - 81),
    (82 - 82),
    (83 - 83),
    (84 - 84),
    (85 - 85),
    (86 - 86),
    (87 - 87),
    (88 - 88),
    (89 - 89),
    (90 - 90),
    (91 - 91) + BOUNDARY, // [
    (92 - 92),
    (93 - 93) + BOUNDARY, // ]
    (94 - 94),
    (95 - 95),
    (96 - 96),
    (97 - 97),
    (98 - 98),
    (99 - 99),
    (100 - 100),
    (101 - 101),
    (102 - 102),
    (103 - 103),
    (104 - 104),
    (105 - 105),
    (106 - 106),
    (107 - 107),
    (108 - 108),
    (109 - 109),
    (110 - 110),
    (111 - 111),
    (112 - 112),
    (113 - 113),
    (114 - 114),
    (115 - 115),
    (116 - 116),
    (117 - 117),
    (118 - 118),
    (119 - 119),
    (120 - 120),
    (121 - 121),
    (122 - 122),
    (123 - 123) + BOUNDARY, // {
    (124 - 124),
    (125 - 125) + BOUNDARY, // }
    (126 - 126),
    (127 - 127),
    (128 - 128),
    (129 - 129),
    (130 - 130),
    (131 - 131),
    (132 - 132),
    (133 - 133),
    (134 - 134),
    (135 - 135),
    (136 - 136),
    (137 - 137),
    (138 - 138),
    (139 - 139),
    (140 - 140),
    (141 - 141),
    (142 - 142),
    (143 - 143),
    (144 - 144),
    (145 - 145),
    (146 - 146),
    (147 - 147),
    (148 - 148),
    (149 - 149),
    (150 - 150),
    (151 - 151),
    (152 - 152),
    (153 - 153),
    (154 - 154),
    (155 - 155),
    (156 - 156),
    (157 - 157),
    (158 - 158),
    (159 - 159),
    (160 - 160),
    (161 - 161),
    (162 - 162),
    (163 - 163),
    (164 - 164),
    (165 - 165),
    (166 - 166),
    (167 - 167),
    (168 - 168),
    (169 - 169),
    (170 - 170),
    (171 - 171),
    (172 - 172),
    (173 - 173),
    (174 - 174),
    (175 - 175),
    (176 - 176),
    (177 - 177),
    (178 - 178),
    (179 - 179),
    (180 - 180),
    (181 - 181),
    (182 - 182),
    (183 - 183),
    (184 - 184),
    (185 - 185),
    (186 - 186),
    (187 - 187),
    (188 - 188),
    (189 - 189),
    (190 - 190),
    (191 - 191),
    (192 - 192),
    (193 - 193),
    (194 - 194),
    (195 - 195),
    (196 - 196),
    (197 - 197),
    (198 - 198),
    (199 - 199),
    (200 - 200),
    (201 - 201),
    (202 - 202),
    (203 - 203),
    (204 - 204),
    (205 - 205),
    (206 - 206),
    (207 - 207),
    (208 - 208),
    (209 - 209),
    (210 - 210),
    (211 - 211),
    (212 - 212),
    (213 - 213),
    (214 - 214),
    (215 - 215),
    (216 - 216),
    (217 - 217),
    (218 - 218),
    (219 - 219),
    (220 - 220),
    (221 - 221),
    (222 - 222),
    (223 - 223),
    (224 - 224),
    (225 - 225),
    (226 - 226),
    (227 - 227),
    (228 - 228),
    (229 - 229),
    (230 - 230),
    (231 - 231),
    (232 - 232),
    (233 - 233),
    (234 - 234),
    (235 - 235),
    (236 - 236),
    (237 - 237),
    (238 - 238),
    (239 - 239),
    (240 - 240),
    (241 - 241),
    (242 - 242),
    (243 - 243),
    (244 - 244),
    (245 - 245),
    (246 - 246),
    (247 - 247),
    (248 - 248),
    (249 - 249),
    (250 - 250),
    (251 - 251),
    (252 - 252),
    (253 - 253),
    (254 - 254),
    (255 - 255),
];
