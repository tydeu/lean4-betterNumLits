open Lean

--------------------------------------------------------------------------------
-- Digit Classes
--------------------------------------------------------------------------------

section 
universe u

class Zero  (A : Type u) := zero  : A
class One   (A : Type u) := one   : A
class Two   (A : Type u) := two   : A
class Three (A : Type u) := three : A
class Four  (A : Type u) := four  : A
class Five  (A : Type u) := five  : A
class Six   (A : Type u) := six   : A
class Seven (A : Type u) := seven : A
class Eight (A : Type u) := eight : A
class Nine  (A : Type u) := nine  : A

export Zero  (zero)
export One   (one)
export Two   (two)
export Three (three)
export Four  (four)
export Five  (five)
export Six   (six)
export Seven (seven)
export Eight (eight)
export Nine  (nine)

end

--------------------------------------------------------------------------------
-- Nat Digits & Special Cases
--------------------------------------------------------------------------------

namespace Nat

abbrev one      := 1
abbrev two      := 2
abbrev three    := 3
abbrev four     := 4
abbrev five     := 5
abbrev six      := 6
abbrev seven    := 7
abbrev eight    := 8
abbrev nine     := 9
abbrev ten      := 10
abbrev eleven   := 11
abbrev twelve   := 12
abbrev thirteen := 13
abbrev fourteen := 14
abbrev fifteen  := 15
abbrev sixteen  := 16

@[defaultInstance low] instance : Zero  Nat := ⟨zero⟩ 
@[defaultInstance low] instance : One   Nat := ⟨one⟩
@[defaultInstance low] instance : Two   Nat := ⟨two⟩
@[defaultInstance low] instance : Three Nat := ⟨three⟩
@[defaultInstance low] instance : Four  Nat := ⟨four⟩
@[defaultInstance low] instance : Five  Nat := ⟨five⟩
@[defaultInstance low] instance : Six   Nat := ⟨six⟩
@[defaultInstance low] instance : Seven Nat := ⟨seven⟩
@[defaultInstance low] instance : Eight Nat := ⟨eight⟩
@[defaultInstance low] instance : Nine  Nat := ⟨nine⟩

end Nat

--------------------------------------------------------------------------------
-- Radix Class
--------------------------------------------------------------------------------

section
universe u

class OfRadix (A : Type u) (radix : Nat) (digits : Array (Fin radix)) where
  ofRadix : A

export OfRadix (ofRadix)

@[defaultInstance low]
instance {r : Nat} {ds : Array (Fin r)} 
  : OfRadix Nat r ds := ⟨ds.foldl (fun n d => n * r + d) Nat.zero⟩

instance {A : Type u} {r : Nat} {ds : Array (Fin r)} 
  [Zero A] [Mul A] [Add A] [CoeHTCT Nat A] [CoeHTCT (Fin r) A] 
  : OfRadix A r ds := ⟨ds.foldl (fun n d => n * (coe r) + (coe d)) zero⟩

end

--------------------------------------------------------------------------------
-- Base Class
--------------------------------------------------------------------------------

abbrev Bit := Fin 2

namespace Bit
abbrev zero : Bit := Fin.mk 0 (by decide)
abbrev one  : Bit := Fin.mk 1 (by decide)

instance : Zero Bit := ⟨zero⟩ 
instance : One  Bit := ⟨one⟩ 
end Bit

abbrev Octit := Fin 8

namespace Octit
abbrev zero  : Octit := Fin.mk 0 (by decide)
abbrev one   : Octit := Fin.mk 1 (by decide)
abbrev two   : Octit := Fin.mk 2 (by decide)
abbrev three : Octit := Fin.mk 3 (by decide)
abbrev four  : Octit := Fin.mk 4 (by decide)
abbrev five  : Octit := Fin.mk 5 (by decide)
abbrev six   : Octit := Fin.mk 6 (by decide)
abbrev seven : Octit := Fin.mk 7 (by decide)

instance : Zero   Octit  := ⟨zero⟩ 
instance : One    Octit  := ⟨one⟩ 
instance : Two    Octit  := ⟨two⟩ 
instance : Three  Octit  := ⟨three⟩ 
instance : Four   Octit  := ⟨four⟩ 
instance : Five   Octit  := ⟨five⟩ 
instance : Six    Octit  := ⟨six⟩ 
instance : Seven  Octit  := ⟨seven⟩ 
end Octit

abbrev Digit := Fin 10

namespace Digit
abbrev zero   : Digit := Fin.mk 0 (by decide)
abbrev one    : Digit := Fin.mk 1 (by decide)
abbrev two    : Digit := Fin.mk 2 (by decide)
abbrev three  : Digit := Fin.mk 3 (by decide)
abbrev four   : Digit := Fin.mk 4 (by decide)
abbrev five   : Digit := Fin.mk 5 (by decide)
abbrev six    : Digit := Fin.mk 6 (by decide)
abbrev seven  : Digit := Fin.mk 7 (by decide)
abbrev eight  : Digit := Fin.mk 8 (by decide)
abbrev nine   : Digit := Fin.mk 9 (by decide)

instance : Zero   Digit := ⟨zero⟩ 
instance : One    Digit := ⟨one⟩ 
instance : Two    Digit := ⟨two⟩ 
instance : Three  Digit := ⟨three⟩ 
instance : Four   Digit := ⟨four⟩ 
instance : Five   Digit := ⟨five⟩ 
instance : Six    Digit := ⟨six⟩ 
instance : Seven  Digit := ⟨seven⟩ 
instance : Eight  Digit := ⟨eight⟩ 
instance : Nine   Digit := ⟨nine⟩ 
end Digit

abbrev Hexit := Fin 16

namespace Hexit
abbrev zero     : Hexit := Fin.mk 0  (by decide)
abbrev one      : Hexit := Fin.mk 1  (by decide)
abbrev two      : Hexit := Fin.mk 2  (by decide)
abbrev three    : Hexit := Fin.mk 3  (by decide)
abbrev four     : Hexit := Fin.mk 4  (by decide)
abbrev five     : Hexit := Fin.mk 5  (by decide)
abbrev six      : Hexit := Fin.mk 6  (by decide)
abbrev seven    : Hexit := Fin.mk 7  (by decide)
abbrev eight    : Hexit := Fin.mk 8  (by decide)
abbrev nine     : Hexit := Fin.mk 9  (by decide)
abbrev ten      : Hexit := Fin.mk 10 (by decide)
abbrev eleven   : Hexit := Fin.mk 11 (by decide)
abbrev twelve   : Hexit := Fin.mk 12 (by decide)
abbrev thirteen : Hexit := Fin.mk 13 (by decide)
abbrev fourteen : Hexit := Fin.mk 14 (by decide)
abbrev fifteen  : Hexit := Fin.mk 15 (by decide)


instance : Zero   Hexit := ⟨zero⟩ 
instance : One    Hexit := ⟨one⟩ 
instance : Two    Hexit := ⟨two⟩ 
instance : Three  Hexit := ⟨three⟩ 
instance : Four   Hexit := ⟨four⟩ 
instance : Five   Hexit := ⟨five⟩ 
instance : Six    Hexit := ⟨six⟩ 
instance : Seven  Hexit := ⟨seven⟩ 
instance : Eight  Hexit := ⟨eight⟩ 
instance : Nine   Hexit := ⟨nine⟩ 
end Hexit

--------------------------------------------------------------------------------
-- Num Lit Syntax
--------------------------------------------------------------------------------

section
open Lean Syntax

def digitToStx : Char -> Syntax
| '0' => mkCIdent ``Zero.zero
| '1' => mkCIdent ``One.one
| '2' => mkCIdent ``Two.two
| '3' => mkCIdent ``Three.three
| '4' => mkCIdent ``Four.four
| '5' => mkCIdent ``Five.five
| '6' => mkCIdent ``Six.six
| '7' => mkCIdent ``Seven.seven
| '8' => mkCIdent ``Eight.eight
| '9' => mkCIdent ``Nine.nine
| _ => Syntax.missing

def bitToStx : Char -> Syntax
| '0' => mkCIdent ``Bit.zero
| '1' => mkCIdent ``Bit.one
| _ => Syntax.missing

def octToStx : Char -> Syntax
| '0' => mkCIdent ``Octit.zero
| '1' => mkCIdent ``Octit.one
| '2' => mkCIdent ``Octit.two
| '3' => mkCIdent ``Octit.three
| '4' => mkCIdent ``Octit.four
| '5' => mkCIdent ``Octit.five
| '6' => mkCIdent ``Octit.six
| '7' => mkCIdent ``Octit.seven
| _ => Syntax.missing

def decToStx : Char -> Syntax
| '0' => mkCIdent ``Digit.zero
| '1' => mkCIdent ``Digit.one
| '2' => mkCIdent ``Digit.two
| '3' => mkCIdent ``Digit.three
| '4' => mkCIdent ``Digit.four
| '5' => mkCIdent ``Digit.five
| '6' => mkCIdent ``Digit.six
| '7' => mkCIdent ``Digit.seven
| '8' => mkCIdent ``Digit.eight
| '9' => mkCIdent ``Digit.nine
| _ => Syntax.missing

def hexToStx : Char -> Syntax
| '0' => mkCIdent ``Hexit.zero
| '1' => mkCIdent ``Hexit.one
| '2' => mkCIdent ``Hexit.two
| '3' => mkCIdent ``Hexit.three
| '4' => mkCIdent ``Hexit.four
| '5' => mkCIdent ``Hexit.five
| '6' => mkCIdent ``Hexit.six
| '7' => mkCIdent ``Hexit.seven
| '8' => mkCIdent ``Hexit.eight
| '9' => mkCIdent ``Hexit.nine
| 'a' => mkCIdent ``Hexit.ten
| 'A' => mkCIdent ``Hexit.ten
| 'b' => mkCIdent ``Hexit.eleven
| 'B' => mkCIdent ``Hexit.eleven
| 'c' => mkCIdent ``Hexit.twelve
| 'C' => mkCIdent ``Hexit.twelve
| 'd' => mkCIdent ``Hexit.thirteen
| 'D' => mkCIdent ``Hexit.thirteen
| 'e' => mkCIdent ``Hexit.fourteen
| 'E' => mkCIdent ``Hexit.fourteen
| 'f' => mkCIdent ``Hexit.fifteen
| 'F' => mkCIdent ``Hexit.fifteen
| _ => Syntax.missing

partial def digitsToStxList
  (digitFn : Char -> Syntax) (str : String) (off : String.Pos) 
: List Syntax :=
  if str.atEnd off then 
    []
  else 
    digitFn (str.get off) :: digitsToStxList digitFn str (str.next off)

def digitsToStx 
  (radix : Syntax) (digitFn : Char -> Syntax) (str : String) (off : String.Pos)
: MacroM Syntax :=
  let digits := quote $ List.toArray $ digitsToStxList digitFn str off
  `(ofRadix (radix := $radix) (digits := $digits))

def expandRadixLit (stx : Syntax) (str : String) : MacroM Syntax :=
  let len := str.length
  if len == 0 then 
    Macro.throwErrorAt stx "empty numLit"
  else 
    let c := str.get 0
    if len == 1 then 
      digitToStx c
    else
      if c == '0' then 
        let c := str.get 1
        if c == 'x' || c == 'X' then 
          if len == 3 then 
            hexToStx (str.get 2)
          else
            digitsToStx (mkCIdent ``Nat.sixteen) hexToStx str 2
        else if c == 'b' || c == 'B' then 
          if len == 3 then 
            bitToStx (str.get 2)
          else
            digitsToStx (mkCIdent ``Nat.two) bitToStx str 2
        else if c == 'o' || c == 'O' then 
          if len == 3 then 
            octToStx (str.get 2)
          else
            digitsToStx (mkCIdent ``Nat.eight) octToStx str 2
        else if c.isDigit then 
          digitsToStx (mkCIdent ``Nat.ten) digitToStx str 0
        else 
          Macro.throwErrorAt stx "invalid num lit prefix"
      else if c.isDigit then 
        digitsToStx (mkCIdent ``Nat.ten) digitToStx str 0
      else 
        Macro.throwErrorAt stx "invalid num lit"

@[macro numLit]
def expandNumLit : Macro
  | stx => 
    match isLit? numLitKind stx with
      | some str => expandRadixLit stx str
      | _        => Macro.throwErrorAt stx "invalid num lit"

end

--------------------------------------------------------------------------------
-- Unexpanders
--------------------------------------------------------------------------------

section 
open Lean Syntax PrettyPrinter

@[inline] abbrev unexpandToNum (s : String) : Unexpander := fun _ => mkNumLit s

@[appUnexpander Zero.zero]      def unexpandZero          := unexpandToNum "0"
@[appUnexpander One.one]        def unexpandOne           := unexpandToNum "1"
@[appUnexpander Two.two]        def unexpandTwo           := unexpandToNum "2"
@[appUnexpander Three.three]    def unexpandThree         := unexpandToNum "3"
@[appUnexpander Four.four]      def unexpandFour          := unexpandToNum "4"
@[appUnexpander Five.five]      def unexpandFive          := unexpandToNum "5"
@[appUnexpander Six.six]        def unexpandSix           := unexpandToNum "6"
@[appUnexpander Seven.seven]    def unexpandSeven         := unexpandToNum "7"
@[appUnexpander Eight.eight]    def unexpandEight         := unexpandToNum "8"
@[appUnexpander Nine.nine]      def unexpandNine          := unexpandToNum "9"

@[appUnexpander Bit.zero]       def unexpandBitZero       := unexpandToNum "0b0"
@[appUnexpander Bit.one]        def unexpandBitOne        := unexpandToNum "0b1"

@[appUnexpander Octit.zero]     def unexpandOctitZero     := unexpandToNum "0o0"
@[appUnexpander Octit.one]      def unexpandOctitOne      := unexpandToNum "0o1"
@[appUnexpander Octit.two]      def unexpandOctitTwo      := unexpandToNum "0o2"
@[appUnexpander Octit.three]    def unexpandOctitThree    := unexpandToNum "0o3"
@[appUnexpander Octit.four]     def unexpandOctitFour     := unexpandToNum "0o4"
@[appUnexpander Octit.five]     def unexpandOctitFive     := unexpandToNum "0o5"
@[appUnexpander Octit.six]      def unexpandOctitSix      := unexpandToNum "0o6"
@[appUnexpander Octit.seven]    def unexpandOctitSeven    := unexpandToNum "0o7"

@[appUnexpander Digit.zero]     def unexpandDigitZero     := unexpandToNum "00"
@[appUnexpander Digit.one]      def unexpandDigitOne      := unexpandToNum "01"
@[appUnexpander Digit.two]      def unexpandDigitTwo      := unexpandToNum "02"
@[appUnexpander Digit.three]    def unexpandDigitThree    := unexpandToNum "03"
@[appUnexpander Digit.four]     def unexpandDigitFour     := unexpandToNum "04"
@[appUnexpander Digit.five]     def unexpandDigitFive     := unexpandToNum "05"
@[appUnexpander Digit.six]      def unexpandDigitSix      := unexpandToNum "06"
@[appUnexpander Digit.seven]    def unexpandDigitSeven    := unexpandToNum "07"
@[appUnexpander Digit.eight]    def unexpandDigitEight    := unexpandToNum "08"
@[appUnexpander Digit.nine]     def unexpandDigitNine     := unexpandToNum "09"

@[appUnexpander Hexit.zero]     def unexpandHexitZero     := unexpandToNum "0x0"
@[appUnexpander Hexit.one]      def unexpandHexitOne      := unexpandToNum "0x1"
@[appUnexpander Hexit.two]      def unexpandHexitTwo      := unexpandToNum "0x2"
@[appUnexpander Hexit.three]    def unexpandHexitThree    := unexpandToNum "0x3"
@[appUnexpander Hexit.four]     def unexpandHexitFour     := unexpandToNum "0x4"
@[appUnexpander Hexit.five]     def unexpandHexitFive     := unexpandToNum "0x5"
@[appUnexpander Hexit.six]      def unexpandHexitSix      := unexpandToNum "0x6"
@[appUnexpander Hexit.seven]    def unexpandHexitSeven    := unexpandToNum "0x7"
@[appUnexpander Hexit.eight]    def unexpandHexitEight    := unexpandToNum "0x8"
@[appUnexpander Hexit.nine]     def unexpandHexitNine     := unexpandToNum "0x9"
@[appUnexpander Hexit.ten]      def unexpandHexitTen      := unexpandToNum "0xA"
@[appUnexpander Hexit.eleven]   def unexpandHexitEleven   := unexpandToNum "0xB"
@[appUnexpander Hexit.twelve]   def unexpandHexitTwelve   := unexpandToNum "0xC"
@[appUnexpander Hexit.thirteen] def unexpandHexitThirteen := unexpandToNum "0xD"
@[appUnexpander Hexit.fourteen] def unexpandHexitFourteen := unexpandToNum "0xE"
@[appUnexpander Hexit.fifteen]  def unexpandHexitFifteen  := unexpandToNum "0xF"

private def decodeDigitLit 
  (radixChar : Char) (dstx : Syntax) 
: Option Char := OptionM.run do
  let dstr <- dstx.isLit? numLitKind
  if dstr.length == 3 && dstr[0] == '0' && dstr[1] == radixChar then
    dstr[2]
  else
    none

@[appUnexpander OfRadix.ofRadix]
def unexpandOfRadix : Unexpander
| `($_f:ident #[$[$ds:numLit],*]) => 
  let res := OptionM.run do
    let d <- isLit? numLitKind ds[0]
    let len := d.length
    if len == 1 then
      let num <- ds.mapM fun d => 
        d.isLit? numLitKind >>= fun s => ite (s.length == 1) s[0] none
      mkNumLit (String.mk num.toList)
    else if len == 3 && d[0] == '0' then
      let d1 := d[1]
      if d1 == 'x' then
        let num <- ds.mapM (decodeDigitLit 'x')
        mkNumLit ("0x" ++ String.mk num.toList)
      else if d1 == 'b' then
        let num <- ds.mapM (decodeDigitLit 'b')
        mkNumLit ("0b" ++ String.mk num.toList)
      else if d1 == 'o' then
        let num <- ds.mapM (decodeDigitLit 'o')
        mkNumLit ("0o" ++ String.mk num.toList)
      else
        none
    else
      none
  match res with | some v => v | none => throw ()
| _ => throw ()

end

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

-- Numerals
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

-- Bits
#check 0b0
#check 0B1

-- Octits
#check 0o0
#check 0o1
#check 0o2
#check 0o3
#check 0o4
#check 0o5
#check 0o6
#check 0o7

-- Digits
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

-- Hexits
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

-- Unexpanders
#check ofRadix #[0, 0xF, 1]
#check ofRadix #[0b1, 0, 1]
#check ofRadix #[0xA, 0, 1]
#check ofRadix #[0o4, 0, 1]
#check ofRadix #[0b1, 0b0, 0b1]
#check ofRadix #[0xA, 0x2, 0xF]
#check ofRadix #[0o1, 0o4, 0o7]
#check ofRadix (#[0, 1, 2] : Array Digit)
