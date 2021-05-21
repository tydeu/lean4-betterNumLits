# BetterNumLits

This package provides an alternative macro expansion of Lean 4's numerical literals (i.e., `numLit`). 

**Features**
- Classes for each type of digit
- Multi-digit literals are expanded to an array of digits
- Radix is preserved during pretty printing
- No need for `nat_lit` (sort of)
- Entirely syntactic

## Numerals

The `BetterNumLits` package notation for common numerals that is separate from `numLit`. Each numeral is expanded to a different type class function.

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

Each single decimal digit literal is expanded to its numeral.

```
0 => (0)
1 => (1)
2 => (2)
3 => (3)
4 => (4)
5 => (5)
6 => (6)
7 => (7)
8 => (8)
9 => (9)
```

Radix-specific digits (ex. `0b0`, `0o0`, `0x0`) are expanded in the same way multi-digit numbers are (see below). However, instead of defaulting to `Nat`, they default to `Fin 2`, `Fin 8`,  and `Fin 16` (for binary, octal, and hexadecimal digits, respectively), because this seemed more appropriate to the creator. This can easily be changing by marking the `Nat` `OfRadix` instance with a higher priority `@[defaultInstance]` than the `Fin` instance.

## Number Expansion

Numbers of radix `r` are expanded into an `Array` of `Fin r` that is passed along with the radix to the function `ofRadix` that relies on the type class `OfRadix`, which are defined as follows:

```lean
class OfRadix (A : Type u) (radix : Nat) (digits : Array (Fin radix)) where
  ofRadix : A

abbrev ofRadix {A : Type u} (radix : Nat) (digits : Array (Fin radix))
  [inst : OfRadix A radix digits] : A := inst.ofRadix
```

Some examples of how literals expand to `ofRadix` are provided below:

```
1239   => ofRadix (10) #[(1),  (2),  (3), (9)]
067845 => ofRadix (10) #[(0),  (6),  (7), (8), (4), (5)]
0b1011 => ofRadix (2)  #[(1),  (0),  (1), (1)]
0xAf04 => ofRadix (16) #[(10), (16), (0), (4)]
0o2041 => ofRadix (8)  #[(2),  (0),  (4), (1)]
```

This makes it easier for the notation to support custom types that may be better expressed in positional form than `Nat`'s successor form.

Due to the way Lean instance selection works, it is important to define radix-specific instances for `OfRadix` by using a numeral for the radix rather than a numeric literal. For example, a hexadecimal `OfRadix` instance for some type `Foo` would look similar to the following:

```lean
instance {digits : Array (Fin (16))} : 
  OfRadix Foo (16) digits := {ofRadix := ...}
```

This essentially supplants how `nat_lit` would be used for defining instances of the original `OfNat` class.

## Pretty Printing

As a consequence of the new expansion, more intelligent unexpansion was also possible. The unexpanders provided in `BetterNumLit` preserve the base of the original literal for pretty printing. However, the specific letter casing of the radix marker and hexadecimal digits is lost. The unexpander thus chooses a canonical form to represent them -- lower case for the marker (i.e., `b`, `o`, `x`) and upper case for the hexadecimal digits (i.e. `A`-`F`). 

Here are some examples of how literals unexpand:

```
1239   => 1239
067845 => 067845
0b1011 => 0b1011
0XAf04 => 0xAF04
0O2041 => 0o2041
```