# BetterNumLits

This package provides an alternative macro expansion of Lean 4's numerical literals (i.e., `numLit`). 

**Features**
- Classes for each type of digit
- Multi-digit literals are expanded to an array of digits
- Radix is preserved during pretty printing
- No need for `nat_lit` (sort of)
- Entirely syntactic

## Numerals

The `BetterNumLits` package notation for common numerals separate from `numLit`.
Each numeral is expanded to a different type class function.

```
(0)  => Zero.zero
(1)  => One.one
(2)  => Two.two
(3)  => Three.three
(4)  => Four.four
(5)  => Five.five
(6)  => Six.six
(7)  => Seven.seven
(8)  => Eight.eight
(9)  => Nine.nine
(10) => Ten.ten
(11) => Eleven.eleven
(12) => Twelve.twelve
(13) => Thirteen.thirteen
(14) => Fourteen.fourteen
(15) => Fifteen.fifteen
(16) => Sixteen.sixteen
```

## Digit Expansion

Each unique single digit literal is also expanded to a different type class function.

```
0 => Zero.zero
1 => One.one
2 => Two.two
3 => Three.three
4 => Four.four
5 => Five.five
6 => Six.six
7 => Seven.seven
8 => Eight.eight
9 => Nine.nine

0b0 => Bin0.bin0
0b1 => Bin1.bin1

0o0 => Oct0.oct0
0o1 => Oct1.oct1
0o2 => Oct2.oct2
0o3 => Oct3.oct3
0o4 => Oct4.oct4
0o5 => Oct5.oct5
0o6 => Oct6.oct6
0o7 => Oct7.oct7

0x0 => Hex0.hex0
0x1 => Hex1.hex1
0x2 => Hex2.hex2
0x3 => Hex3.hex3
0x4 => Hex4.hex4
0x5 => Hex5.hex5
0x6 => Hex6.hex6
0x7 => Hex7.hex7
0x8 => Hex8.hex8
0x9 => Hex9.hex9
0xA => HexA.hexA
0xB => HexB.hexB
0xC => HexC.hexC
0xD => HexD.hexD
0xE => HexE.hexE
0xF => HexF.hexF
```

Note that letters in numerals are case-insensitive. However, the canonical form used by the unexpanders is lower case for radix markers (i.e., `b`, `o`, `x`) and upper case for hexadecimal digits (i.e. `A`-`F`).

The current implementation defaults radix-specific digits (ex. `0b0`, `0o0`, `0x0`) to `Fin 2`, `Fin 8`,  and `Fin 16` instead of `Nat`, as that seemed more appropriate to the creator. This could easily be changing by marking the `Nat` instances with a higher priority `@[defaultInstance]`.

## Number Expansion

Multi-digit numbers of radix `r` are expanded into an `Array` of `Fin r` that is passed along with the radix to the function `ofRadix` that relies on the type class `OfRadix`, which are defined as follows:

```lean
class OfRadix (A : Type u) (radix : Nat) (digits : Array (Fin radix)) where
  ofRadix : A

abbrev ofRadix {A : Type u} (radix : Nat) (digits : Array (Fin radix))
  [inst : OfRadix A radix digits] : A := inst.ofRadix
```

Some examples of how literals expand to `ofRadix` are provided below:

```
1239   => ofRadix (10) #[one, two, three, nine]
067845 => ofRadix (10) #[zero, six, seven, eight, four, five]
0b1011 => ofRadix (2)  #[bin1, bin0, bin1, bin1]
0xAf04 => ofRadix (16) #[hexA, hexF, hex0, hex4]
0o2041 => ofRadix (8)  #[oct2, oct0, oct4, oct1]
```

This makes it easier for the notation to support custom types that may be better expressed in positional form than `Nat`'s successor form.

Due to the way Lean instance selection works, it is important to define radix-specific instances for `OfRadix` by using numerals for the radix rather than a numeric literal. For example, a hexadecimal `OfRadix` instance for some type `Foo` would look similar to the following:

```lean
instance {digits : Array (Fin (16))} : 
  OfRadix Foo (16) digits := {ofRadix := ...}
```

This essentially supplants how `nat_lit` would be used for defining instances of the original `OfNat` class.

## Pretty Printing

As a consequence of the new expansion, more intelligent unexpansion was also possible. The unexpanders provided in `BetterNumLit` preserve the base of the original literal for pretty printing. However, the specific letter casing of the radix marker and hexadecimal digits is lost and instead converted to their canonical form (i.e., lower case for the marker, upper case for the digits). 

Here are some examples of how literals unexpand:

```
1239   => 1239
067845 => 067845
0b1011 => 0b1011
0XAf04 => 0xAF04
0O2041 => 0o2041
```