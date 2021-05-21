import BetterNumLits.Numerals
import BetterNumLits.Nat
import BetterNumLits.OfRadix 
import BetterNumLits.Fin

open Lean Syntax PrettyPrinter

def decodeRadixDigitLit 
  (radixChar : Char) (dstx : Syntax) 
: Option Char := OptionM.run do
  let dstr <- dstx.isLit? numLitKind
  if dstr.length == 3 && dstr[0] == '0' && dstr[1] == radixChar then
    dstr[2]
  else
    none

def decodeBinNum : Syntax -> Option Char
| `((0)) => '0'
| `((1)) => '1'
| _ => none

def decodeOctNum : Syntax -> Option Char
| `((0)) => '0'
| `((1)) => '1'
| `((2)) => '2'
| `((3)) => '3'
| `((4)) => '4'
| `((5)) => '5'
| `((6)) => '6'
| `((7)) => '7'
| _ => none

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

def decodeHexNum : Syntax -> Option Char
| `((0))  => '0'
| `((1))  => '1'
| `((2))  => '2'
| `((3))  => '3'
| `((4))  => '4'
| `((5))  => '5'
| `((6))  => '6'
| `((7))  => '7'
| `((8))  => '8'
| `((9))  => '9'
| `((10)) => 'A'
| `((11)) => 'B'
| `((12)) => 'C'
| `((13)) => 'D'
| `((14)) => 'E'
| `((15)) => 'F'
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
      let num <- ds.mapM decodeHexNum
      mkNumLit ("0x" ++ String.mk num.data)
    | `((2)) => 
      let num <- ds.mapM decodeBinNum
      mkNumLit ("0b" ++ String.mk num.data)
    | `((8)) => 
      let num <- ds.mapM decodeOctNum
      mkNumLit ("0o" ++ String.mk num.data)
    | _ => none
  match res with | some v => v | none => throw ()
| _ => throw ()
