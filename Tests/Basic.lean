import BetterNumLits

-- Numerals
#check (0)
#check (1)
#check (2)
#check (3)
#check (4)
#check (5)
#check (6)
#check (7)
#check (8)
#check (9)
#check (9)
#check (10)
#check (11)
#check (12)
#check (13)
#check (14)
#check (15)
#check (16)

-- Digits
#check 0
#check 1
#check 2
#check 3
#check 4
#check 5
#check 6
#check 7
#check 8
#check 9

-- Binary
#check 0b0
#check 0B1

-- Octal
#check 0o0
#check 0o1
#check 0o2
#check 0o3
#check 0o4
#check 0o5
#check 0o6
#check 0o7

-- Decimal
#check 00
#check 01
#check 02
#check 03
#check 04
#check 05
#check 06
#check 07
#check 08
#check 09

-- Hexadecimal
#check 0x0
#check 0x1
#check 0x2
#check 0x3
#check 0x4
#check 0x5
#check 0x6
#check 0x7
#check 0x8
#check 0x9
#check 0xa
#check 0xA
#check 0xB
#check 0xb
#check 0xB
#check 0xc
#check 0xC
#check 0xd
#check 0xD
#check 0xe
#check 0xE
#check 0xf
#check 0xF

-- Numbers
#check 1239
#check 067845
#check 0b1011
#check 0B1011
#check 0xAf04
#check 0XAf04
#check 0o2041
#check 0O2041

-- Reductions
#reduce 1239
#reduce 067845
#reduce 0b1011 -- 11
#reduce 0xAf04 -- 44804
#reduce 0o2041 -- 1057

-- Heterogenous Unexpanders
#check ofRadix _ #[0, 0xF, 1]
#check ofRadix _ #[0b1, 0, 1]
#check ofRadix _ #[0xA, 0, 1]
#check ofRadix _ #[0o4, 0, 1]

-- Homogenous Unexpanders
#check ofRadix _ #[0b1, 0b0, 0b1]
#check ofRadix _ #[0xA, 0x2, 0xF]
#check ofRadix _ #[0o1, 0o4, 0o7]

-- Numeral Unexpanders
#check ofRadix (2)  #[1, 0, 1]
#check ofRadix (16) #[(10), 2, (15)]
#check ofRadix (8)  #[1, 4, 7]
#check ofRadix (10) #[0, 1, 2]
