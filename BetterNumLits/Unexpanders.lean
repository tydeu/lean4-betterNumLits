import BetterNumLits.Numerals
import BetterNumLits.Digits
import BetterNumLits.Nat
import BetterNumLits.OfRadix 
import BetterNumLits.Fin

open Lean Syntax PrettyPrinter

@[inline] abbrev unexpandToNum (s : String) : Unexpander := fun _ => mkNumLit s

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

def decodeRadixDigitLit 
  (radixChar : Char) (dstx : Syntax) 
: Option Char := OptionM.run do
  let dstr <- dstx.isLit? numLitKind
  if dstr.length == 3 && dstr[0] == '0' && dstr[1] == radixChar then
    dstr[2]
  else
    none

def decodeDecNum : Syntax -> Option Char
| `((0)) => '0'
| `((1)) => '1'
| `((2)) => '2'
| `((3)) => '3'
| `((4)) => '4'
| `((5)) => '5'
| `((6)) => '6'
| `((7)) => '7'
| `((8)) => '8'
| `((9)) => '9'
| _ => none

@[appUnexpander ofRadix]
def unexpandOfRadix : Unexpander
| `($_f:ident $r:term #[$[$ds:term],*]) => 
  let res := OptionM.run do
    match r with
    | `((10)) => 
      let num <- ds.mapM decodeDecNum
      mkNumLit (String.mk num.data)
    | `((16)) => 
      let num <- ds.mapM (decodeRadixDigitLit 'x')
      mkNumLit ("0x" ++ String.mk num.data)
    | `((2)) => 
      let num <- ds.mapM (decodeRadixDigitLit 'b')
      mkNumLit ("0b" ++ String.mk num.data)
    | `((8)) => 
      let num <- ds.mapM (decodeRadixDigitLit 'o')
      mkNumLit ("0o" ++ String.mk num.data)
    | _ => none
  match res with | some v => v | none => throw ()
| _ => throw ()
