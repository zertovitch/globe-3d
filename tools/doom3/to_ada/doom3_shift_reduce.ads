package Doom3_Shift_Reduce is

    type Small_Integer is range -32_000 .. 32_000;

    type Shift_Reduce_Entry is record
        T   : Small_Integer;
        Act : Small_Integer;
    end record;
    pragma Pack(Shift_Reduce_Entry);

    subtype Row is Integer range -1 .. Integer'Last;

  --pragma suppress(index_check);

    type Shift_Reduce_Array is array (Row  range <>) of Shift_Reduce_Entry;

    Shift_Reduce_Matrix : constant Shift_Reduce_Array :=
        ( (-1,-1) -- Dummy Entry

-- State  0
,( 1, 2),( 5, 4),( 6, 6),(-1,-3000)
-- State  1
,(-1,-1)
-- State  2
,(-1,-2)
-- State  3
,( 7, 13),( 8, 14),( 9, 15),( 10, 16),(-1,-8)
-- State  4
,(-1,-4)
-- State  5
,(-1,-5)
-- State  6
,( 4, 18),(-1,-3000)
-- State  7
,( 0,-3001),(-1,-3000)
-- State  8
,( 7, 13),( 8, 14),( 9, 15),( 10, 16),(-1,-8)
-- State  9
,(-1,-9)
-- State  10
,(-1,-10)
-- State  11
,(-1,-11)
-- State  12
,(-1,-12)
-- State  13
,( 11, 21),(-1,-3000)
-- State  14
,( 11, 22),(-1,-3000)
-- State  15
,(-1,-50)
-- State  16
,(-1,-66)
-- State  17
,(-1,-3)
-- State  18
,( 2, 25),(-1,-3000)
-- State  19
,(-1,-3000)
-- State  20
,(-1,-7)
-- State  21
,( 4, 26),(-1,-3000)
-- State  22
,( 4, 27),(-1,-3000)
-- State  23
,( 11, 28),(-1,-3000)
-- State  24
,( 11, 29),(-1,-3000)
-- State  25
,(-1,-6)
-- State  26
,(-1,-13)
-- State  27
,( 2, 31),(-1,-3000)
-- State  28
,( 2, 32),(-1,-3000)
-- State  29
,( 2, 33),(-1,-3000)
-- State  30
,( 2, 34),(-1,-3000)
-- State  31
,( 2, 35),(-1,-3000)
-- State  32
,( 2, 36),(-1,-3000)
-- State  33
,(-1,-67)
-- State  34
,(-1,-14)
-- State  35
,( 2, 39),(-1,-3000)
-- State  36
,(-1,-51)
-- State  37
,(-1,-73)
-- State  38
,( 2, 44),(-1,-18)
-- State  39
,( 2, 46),(-1,-3000)
-- State  40
,( 2, 48),(-1,-3000)
-- State  41
,( 13,-70),(-1,-72)
-- State  42
,( 13, 51),(-1,-3000)
-- State  43
,(-1,-68)
-- State  44
,(-1,-17)
-- State  45
,( 11, 55),(-1,-20)
-- State  46
,( 2, 57),(-1,-3000)
-- State  47
,( 12,-56),(-1,-54)
-- State  48
,(-1,-57)
-- State  49
,(-1,-52)
-- State  50
,(-1,-73)
-- State  51
,( 2, 64),( 3, 63),(-1,-3000)
-- State  52
,( 2, 66),(-1,-3000)
-- State  53
,( 12, 67),(-1,-3000)
-- State  54
,( 11, 55),(-1,-20)
-- State  55
,( 4, 69),(-1,-3000)
-- State  56
,(-1,-15)
-- State  57
,( 13, 72),(-1,-44)
-- State  58
,( 2, 48),(-1,-3000)
-- State  59
,( 2, 75),(-1,-3000)
-- State  60
,( 12, 76),(-1,-3000)
-- State  61
,(-1,-71)
-- State  62
,(-1,-78)
-- State  63
,(-1,-81)
-- State  64
,(-1,-82)
-- State  65
,( 2, 64),( 3, 63),(-1,-3000)
-- State  66
,(-1,-74)
-- State  67
,(-1,-69)
-- State  68
,(-1,-19)
-- State  69
,(-1,-21)
-- State  70
,( 12, 81),(-1,-3000)
-- State  71
,( 13, 72),(-1,-44)
-- State  72
,( 2, 64),( 3, 63),(-1,-3000)
-- State  73
,( 2, 85),(-1,-48)
-- State  74
,(-1,-55)
-- State  75
,(-1,-58)
-- State  76
,(-1,-53)
-- State  77
,( 2, 64),( 3, 63),(-1,-3000)
-- State  78
,(-1,-76)
-- State  79
,( 2, 90),(-1,-3000)
-- State  80
,( 2, 91),(-1,-3000)
-- State  81
,(-1,-16)
-- State  82
,(-1,-43)
-- State  83
,(-1,-45)
-- State  84
,( 2, 85),(-1,-48)
-- State  85
,(-1,-49)
-- State  86
,( 12, 94),(-1,-3000)
-- State  87
,( 2, 95),(-1,-3000)
-- State  88
,(-1,-79)
-- State  89
,( 14, 97),(-1,-3000)
-- State  90
,(-1,-75)
-- State  91
,(-1,-22)
-- State  92
,( 14, 99),(-1,-3000)
-- State  93
,(-1,-47)
-- State  94
,(-1,-42)
-- State  95
,(-1,-59)
-- State  96
,( 2, 64),( 3, 63),(-1,-3000)
-- State  97
,(-1,-77)
-- State  98
,( 2, 102),(-1,-3000)
-- State  99
,(-1,-46)
-- State  100
,( 13, 104),(-1,-3000)
-- State  101
,(-1,-80)
-- State  102
,(-1,-23)
-- State  103
,( 13,-61),(-1,-63)
-- State  104
,( 2, 64),( 3, 63),(-1,-3000)
-- State  105
,(-1,-60)
-- State  106
,( 13, 110),(-1,-3000)
-- State  107
,( 13, 104),(-1,-3000)
-- State  108
,(-1,-64)
-- State  109
,( 13,-26),(-1,-28)
-- State  110
,(-1,-29)
-- State  111
,(-1,-24)
-- State  112
,(-1,-62)
-- State  113
,( 14, 117),(-1,-3000)
-- State  114
,( 13, 110),(-1,-3000)
-- State  115
,( 2, 64),( 3, 63),(-1,-3000)
-- State  116
,( 2, 121),(-1,-3000)
-- State  117
,(-1,-65)
-- State  118
,(-1,-27)
-- State  119
,( 2, 64),( 3, 63),(-1,-3000)
-- State  120
,( 12,-38),(-1,-36)
-- State  121
,(-1,-39)
-- State  122
,( 12, 126),(-1,-3000)
-- State  123
,(-1,-30)
-- State  124
,( 2, 121),(-1,-3000)
-- State  125
,( 2, 129),(-1,-3000)
-- State  126
,(-1,-25)
-- State  127
,( 2, 64),( 3, 63),(-1,-3000)
-- State  128
,(-1,-37)
-- State  129
,(-1,-40)
-- State  130
,(-1,-31)
-- State  131
,( 2, 133),(-1,-3000)
-- State  132
,( 2, 64),( 3, 63),(-1,-3000)
-- State  133
,(-1,-41)
-- State  134
,( 2, 64),( 3, 63),(-1,-3000)
-- State  135
,( 2, 64),( 3, 63),(-1,-3000)
-- State  136
,(-1,-32)
-- State  137
,( 2, 138),(-1,-35)
-- State  138
,( 2, 140),(-1,-3000)
-- State  139
,( 14, 141),(-1,-3000)
-- State  140
,( 2, 142),(-1,-3000)
-- State  141
,(-1,-33)
-- State  142
,( 2, 143),(-1,-3000)
-- State  143
,(-1,-34)
);
--  The offset vector
SHIFT_REDUCE_OFFSET : array (0.. 143) of Integer :=
( 0,
 4, 5, 6, 11, 12, 13, 15, 17, 22, 23, 24, 25, 26, 28, 30, 31,
 32, 33, 35, 36, 37, 39, 41, 43, 45, 46, 47, 49, 51, 53, 55, 57,
 59, 60, 61, 63, 64, 65, 67, 69, 71, 73, 75, 76, 77, 79, 81, 83,
 84, 85, 86, 89, 91, 93, 95, 97, 98, 100, 102, 104, 106, 107, 108, 109,
 110, 113, 114, 115, 116, 117, 119, 121, 124, 126, 127, 128, 129, 132, 133, 135,
 137, 138, 139, 140, 142, 143, 145, 147, 148, 150, 151, 152, 154, 155, 156, 157,
 160, 161, 163, 164, 166, 167, 168, 170, 173, 174, 176, 178, 179, 181, 182, 183,
 184, 186, 188, 191, 193, 194, 195, 198, 200, 201, 203, 204, 206, 208, 209, 212,
 213, 214, 215, 217, 220, 221, 224, 227, 228, 230, 232, 234, 236, 237, 239);
end Doom3_Shift_Reduce;