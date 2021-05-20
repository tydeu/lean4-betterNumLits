open Lean

--------------------------------------------------------------------------------
-- Generic Digit Classes
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
-- Radix-Specific Digit Classes
--------------------------------------------------------------------------------

section 
universe u

class Bin0 (A : Type u) := bin0 : A
class Bin1 (A : Type u) := bin1 : A

export Bin0 (bin0)
export Bin1 (bin1)

class Oct0 (A : Type u) := oct0 : A
class Oct1 (A : Type u) := oct1 : A
class Oct2 (A : Type u) := oct2 : A
class Oct3 (A : Type u) := oct3 : A
class Oct4 (A : Type u) := oct4 : A
class Oct5 (A : Type u) := oct5 : A
class Oct6 (A : Type u) := oct6 : A
class Oct7 (A : Type u) := oct7 : A

export Oct0 (oct0)
export Oct1 (oct1)
export Oct2 (oct2)
export Oct3 (oct3)
export Oct4 (oct4)
export Oct5 (oct5)
export Oct6 (oct6)
export Oct7 (oct7)

class Hex0 (A : Type u) := hex0 : A
class Hex1 (A : Type u) := hex1 : A
class Hex2 (A : Type u) := hex2 : A
class Hex3 (A : Type u) := hex3 : A
class Hex4 (A : Type u) := hex4 : A
class Hex5 (A : Type u) := hex5 : A
class Hex6 (A : Type u) := hex6 : A
class Hex7 (A : Type u) := hex7 : A
class Hex8 (A : Type u) := hex8 : A
class Hex9 (A : Type u) := hex9 : A
class HexA (A : Type u) := hexA : A
class HexB (A : Type u) := hexB : A
class HexC (A : Type u) := hexC : A
class HexD (A : Type u) := hexD : A
class HexE (A : Type u) := hexE : A
class HexF (A : Type u) := hexF : A

export Hex0 (hex0)
export Hex1 (hex1)
export Hex2 (hex2)
export Hex3 (hex3)
export Hex4 (hex4)
export Hex5 (hex5)
export Hex6 (hex6)
export Hex7 (hex7)
export Hex8 (hex8)
export Hex9 (hex9)
export HexA (hexA)
export HexB (hexB)
export HexC (hexC)
export HexD (hexD)
export HexE (hexE)
export HexF (hexF)

end

--------------------------------------------------------------------------------
-- Nat Instances
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

instance : Bin0 Nat := ⟨zero⟩
instance : Bin1 Nat := ⟨one⟩

instance : Oct0 Nat := ⟨zero⟩
instance : Oct1 Nat := ⟨one⟩
instance : Oct2 Nat := ⟨two⟩
instance : Oct3 Nat := ⟨three⟩
instance : Oct4 Nat := ⟨four⟩
instance : Oct5 Nat := ⟨five⟩
instance : Oct6 Nat := ⟨six⟩
instance : Oct7 Nat := ⟨seven⟩

instance : Hex0 Nat := ⟨zero⟩
instance : Hex1 Nat := ⟨one⟩
instance : Hex2 Nat := ⟨two⟩
instance : Hex3 Nat := ⟨three⟩
instance : Hex4 Nat := ⟨four⟩
instance : Hex5 Nat := ⟨five⟩
instance : Hex6 Nat := ⟨six⟩
instance : Hex7 Nat := ⟨seven⟩
instance : Hex8 Nat := ⟨eight⟩
instance : Hex9 Nat := ⟨nine⟩
instance : HexA Nat := ⟨ten⟩
instance : HexB Nat := ⟨eleven⟩
instance : HexC Nat := ⟨twelve⟩
instance : HexD Nat := ⟨thirteen⟩
instance : HexE Nat := ⟨fourteen⟩
instance : HexF Nat := ⟨fifteen⟩
end Nat

--------------------------------------------------------------------------------
-- Radix Lit Class
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
-- Radix-Specific Types
--------------------------------------------------------------------------------

abbrev Fin2 := Fin Nat.two

namespace Fin2
abbrev zero : Fin2 := Fin.mk 0 (by decide)
abbrev one  : Fin2 := Fin.mk 1 (by decide)

instance : Zero Fin2 := ⟨zero⟩
instance : One  Fin2 := ⟨one⟩

@[defaultInstance low] instance : Bin0 Fin2 := ⟨zero⟩
@[defaultInstance low] instance : Bin1 Fin2 := ⟨one⟩
end Fin2

abbrev Fin8 := Fin Nat.eight

namespace Fin8
abbrev zero  : Fin8 := Fin.mk 0 (by decide)
abbrev one   : Fin8 := Fin.mk 1 (by decide)
abbrev two   : Fin8 := Fin.mk 2 (by decide)
abbrev three : Fin8 := Fin.mk 3 (by decide)
abbrev four  : Fin8 := Fin.mk 4 (by decide)
abbrev five  : Fin8 := Fin.mk 5 (by decide)
abbrev six   : Fin8 := Fin.mk 6 (by decide)
abbrev seven : Fin8 := Fin.mk 7 (by decide)

instance : Zero   Fin8 := ⟨zero⟩
instance : One    Fin8 := ⟨one⟩
instance : Two    Fin8 := ⟨two⟩
instance : Three  Fin8 := ⟨three⟩
instance : Four   Fin8 := ⟨four⟩
instance : Five   Fin8 := ⟨five⟩
instance : Six    Fin8 := ⟨six⟩
instance : Seven  Fin8 := ⟨seven⟩

@[defaultInstance low] instance : Oct0 Fin8 := ⟨zero⟩
@[defaultInstance low] instance : Oct1 Fin8 := ⟨one⟩
@[defaultInstance low] instance : Oct2 Fin8 := ⟨two⟩
@[defaultInstance low] instance : Oct3 Fin8 := ⟨three⟩
@[defaultInstance low] instance : Oct4 Fin8 := ⟨four⟩
@[defaultInstance low] instance : Oct5 Fin8 := ⟨five⟩
@[defaultInstance low] instance : Oct6 Fin8 := ⟨six⟩
@[defaultInstance low] instance : Oct7 Fin8 := ⟨seven⟩
end Fin8

abbrev Fin10 := Fin Nat.ten

namespace Fin10
abbrev zero   : Fin10 := Fin.mk 0 (by decide)
abbrev one    : Fin10 := Fin.mk 1 (by decide)
abbrev two    : Fin10 := Fin.mk 2 (by decide)
abbrev three  : Fin10 := Fin.mk 3 (by decide)
abbrev four   : Fin10 := Fin.mk 4 (by decide)
abbrev five   : Fin10 := Fin.mk 5 (by decide)
abbrev six    : Fin10 := Fin.mk 6 (by decide)
abbrev seven  : Fin10 := Fin.mk 7 (by decide)
abbrev eight  : Fin10 := Fin.mk 8 (by decide)
abbrev nine   : Fin10 := Fin.mk 9 (by decide)

instance : Zero   Fin10 := ⟨zero⟩
instance : One    Fin10 := ⟨one⟩
instance : Two    Fin10 := ⟨two⟩
instance : Three  Fin10 := ⟨three⟩
instance : Four   Fin10 := ⟨four⟩
instance : Five   Fin10 := ⟨five⟩
instance : Six    Fin10 := ⟨six⟩
instance : Seven  Fin10 := ⟨seven⟩
instance : Eight  Fin10 := ⟨eight⟩
instance : Nine   Fin10 := ⟨nine⟩
end Fin10

abbrev Fin16 := Fin Nat.sixteen

namespace Fin16
abbrev zero     : Fin16 := Fin.mk 0  (by decide)
abbrev one      : Fin16 := Fin.mk 1  (by decide)
abbrev two      : Fin16 := Fin.mk 2  (by decide)
abbrev three    : Fin16 := Fin.mk 3  (by decide)
abbrev four     : Fin16 := Fin.mk 4  (by decide)
abbrev five     : Fin16 := Fin.mk 5  (by decide)
abbrev six      : Fin16 := Fin.mk 6  (by decide)
abbrev seven    : Fin16 := Fin.mk 7  (by decide)
abbrev eight    : Fin16 := Fin.mk 8  (by decide)
abbrev nine     : Fin16 := Fin.mk 9  (by decide)
abbrev ten      : Fin16 := Fin.mk 10 (by decide)
abbrev eleven   : Fin16 := Fin.mk 11 (by decide)
abbrev twelve   : Fin16 := Fin.mk 12 (by decide)
abbrev thirteen : Fin16 := Fin.mk 13 (by decide)
abbrev fourteen : Fin16 := Fin.mk 14 (by decide)
abbrev fifteen  : Fin16 := Fin.mk 15 (by decide)

instance : Zero   Fin16 := ⟨zero⟩
instance : One    Fin16 := ⟨one⟩
instance : Two    Fin16 := ⟨two⟩
instance : Three  Fin16 := ⟨three⟩
instance : Four   Fin16 := ⟨four⟩
instance : Five   Fin16 := ⟨five⟩
instance : Six    Fin16 := ⟨six⟩
instance : Seven  Fin16 := ⟨seven⟩
instance : Eight  Fin16 := ⟨eight⟩
instance : Nine   Fin16 := ⟨nine⟩

@[defaultInstance low] instance : Hex0 Fin16 := ⟨zero⟩
@[defaultInstance low] instance : Hex1 Fin16 := ⟨one⟩
@[defaultInstance low] instance : Hex2 Fin16 := ⟨two⟩
@[defaultInstance low] instance : Hex3 Fin16 := ⟨three⟩
@[defaultInstance low] instance : Hex4 Fin16 := ⟨four⟩
@[defaultInstance low] instance : Hex5 Fin16 := ⟨five⟩
@[defaultInstance low] instance : Hex6 Fin16 := ⟨six⟩
@[defaultInstance low] instance : Hex7 Fin16 := ⟨seven⟩
@[defaultInstance low] instance : Hex8 Fin16 := ⟨eight⟩
@[defaultInstance low] instance : Hex9 Fin16 := ⟨nine⟩
@[defaultInstance low] instance : HexA Fin16 := ⟨ten⟩
@[defaultInstance low] instance : HexB Fin16 := ⟨eleven⟩
@[defaultInstance low] instance : HexC Fin16 := ⟨twelve⟩
@[defaultInstance low] instance : HexD Fin16 := ⟨thirteen⟩
@[defaultInstance low] instance : HexE Fin16 := ⟨fourteen⟩
@[defaultInstance low] instance : HexF Fin16 := ⟨fifteen⟩
end Fin16

--------------------------------------------------------------------------------
-- Num Lit Syntax
--------------------------------------------------------------------------------

section
open Lean Syntax

def digitToStx : Char -> Syntax
| '0' => mkCIdent ``zero
| '1' => mkCIdent ``one
| '2' => mkCIdent ``two
| '3' => mkCIdent ``three
| '4' => mkCIdent ``four
| '5' => mkCIdent ``five
| '6' => mkCIdent ``six
| '7' => mkCIdent ``seven
| '8' => mkCIdent ``eight
| '9' => mkCIdent ``nine
| _ => Syntax.missing

def bitToStx : Char -> Syntax
| '0' => mkCIdent ``bin0
| '1' => mkCIdent ``bin1
| _ => Syntax.missing

def octToStx : Char -> Syntax
| '0' => mkCIdent ``oct0
| '1' => mkCIdent ``oct1
| '2' => mkCIdent ``oct2
| '3' => mkCIdent ``oct3
| '4' => mkCIdent ``oct4
| '5' => mkCIdent ``oct5
| '6' => mkCIdent ``oct6
| '7' => mkCIdent ``oct7
| _ => Syntax.missing

def hexToStx : Char -> Syntax
| '0' => mkCIdent ``hex0
| '1' => mkCIdent ``hex1
| '2' => mkCIdent ``hex2
| '3' => mkCIdent ``hex3
| '4' => mkCIdent ``hex4
| '5' => mkCIdent ``hex5
| '6' => mkCIdent ``hex6
| '7' => mkCIdent ``hex7
| '8' => mkCIdent ``hex8
| '9' => mkCIdent ``hex9
| 'a' => mkCIdent ``hexA
| 'A' => mkCIdent ``hexA
| 'b' => mkCIdent ``hexB
| 'B' => mkCIdent ``hexB
| 'c' => mkCIdent ``hexC
| 'C' => mkCIdent ``hexC
| 'd' => mkCIdent ``hexD
| 'D' => mkCIdent ``hexD
| 'e' => mkCIdent ``hexE
| 'E' => mkCIdent ``hexE
| 'f' => mkCIdent ``hexF
| 'F' => mkCIdent ``hexF
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

@[appUnexpander Zero.zero]    def unexpandZero  := unexpandToNum "0"
@[appUnexpander One.one]      def unexpandOne   := unexpandToNum "1"
@[appUnexpander Two.two]      def unexpandTwo   := unexpandToNum "2"
@[appUnexpander Three.three]  def unexpandThree := unexpandToNum "3"
@[appUnexpander Four.four]    def unexpandFour  := unexpandToNum "4"
@[appUnexpander Five.five]    def unexpandFive  := unexpandToNum "5"
@[appUnexpander Six.six]      def unexpandSix   := unexpandToNum "6"
@[appUnexpander Seven.seven]  def unexpandSeven := unexpandToNum "7"
@[appUnexpander Eight.eight]  def unexpandEight := unexpandToNum "8"
@[appUnexpander Nine.nine]    def unexpandNine  := unexpandToNum "9"

@[appUnexpander Bin0.bin0] def unexpandBin0 := unexpandToNum "0b0"
@[appUnexpander Bin1.bin1] def unexpandBin1 := unexpandToNum "0b1"

@[appUnexpander Oct0.oct0] def unexpandOct0 := unexpandToNum "0o0"
@[appUnexpander Oct1.oct1] def unexpandOct1 := unexpandToNum "0o1"
@[appUnexpander Oct2.oct2] def unexpandOct2 := unexpandToNum "0o2"
@[appUnexpander Oct3.oct3] def unexpandOct3 := unexpandToNum "0o3"
@[appUnexpander Oct4.oct4] def unexpandOct4 := unexpandToNum "0o4"
@[appUnexpander Oct5.oct5] def unexpandOct5 := unexpandToNum "0o5"
@[appUnexpander Oct6.oct6] def unexpandOct6 := unexpandToNum "0o6"
@[appUnexpander Oct7.oct7] def unexpandOct7 := unexpandToNum "0o7"

@[appUnexpander Hex0.hex0] def unexpandHex0 := unexpandToNum "0x0"
@[appUnexpander Hex1.hex1] def unexpandHex1 := unexpandToNum "0x1"
@[appUnexpander Hex2.hex2] def unexpandHex2 := unexpandToNum "0x2"
@[appUnexpander Hex3.hex3] def unexpandHex3 := unexpandToNum "0x3"
@[appUnexpander Hex4.hex4] def unexpandHex4 := unexpandToNum "0x4"
@[appUnexpander Hex5.hex5] def unexpandHex5 := unexpandToNum "0x5"
@[appUnexpander Hex6.hex6] def unexpandHex6 := unexpandToNum "0x6"
@[appUnexpander Hex7.hex7] def unexpandHex7 := unexpandToNum "0x7"
@[appUnexpander Hex8.hex8] def unexpandHex8 := unexpandToNum "0x8"
@[appUnexpander Hex9.hex9] def unexpandHex9 := unexpandToNum "0x9"
@[appUnexpander HexA.hexA] def unexpandHexA := unexpandToNum "0xA"
@[appUnexpander HexB.hexB] def unexpandHexB := unexpandToNum "0xB"
@[appUnexpander HexC.hexC] def unexpandHexC := unexpandToNum "0xC"
@[appUnexpander HexD.hexD] def unexpandHexD := unexpandToNum "0xD"
@[appUnexpander HexE.hexE] def unexpandHexE := unexpandToNum "0xE"
@[appUnexpander HexF.hexF] def unexpandHexF := unexpandToNum "0xF"

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
