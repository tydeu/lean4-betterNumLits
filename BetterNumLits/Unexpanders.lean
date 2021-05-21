import BetterNumLits.Numerals
import BetterNumLits.Nat
import BetterNumLits.OfRadix 
import BetterNumLits.Fin

open Lean Syntax PrettyPrinter

--------------------------------------------------------------------------------
-- Digit Unexpanders
--------------------------------------------------------------------------------

@[inline] def unexpandToNum (s : String) : Unexpander := fun _ => mkNumLit s

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

--------------------------------------------------------------------------------
-- Radix Unexpander (& Helpers)
--------------------------------------------------------------------------------

def decodeDigitIf 
  (pred : Char -> Bool) (dstx : Syntax) 
: Option Char := OptionM.run do 
  let dstr <- dstx.isLit? numLitKind
  ite (dstr.length == 1 && pred dstr[0]) dstr[0] none

@[inline]
def decodeBinNum : Syntax -> Option Char
:= decodeDigitIf fun c => c == '0' || c == '1'

@[inline]
def decodeOctNum : Syntax -> Option Char
:= decodeDigitIf fun c => '0' <= c && c <= '7'

@[inline]
def decodeDecNum : Syntax -> Option Char
:= decodeDigitIf fun c => '0' <= c && c <= '9'

def decodeHexNum : Syntax -> Option Char
| `((10)) => 'A'
| `((11)) => 'B'
| `((12)) => 'C'
| `((13)) => 'D'
| `((14)) => 'E'
| `((15)) => 'F'
| stx => decodeDecNum stx

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
    | stx =>
      let str <- stx.isLit? numLitKind
      match str with
      | "2" =>
        let num <- ds.mapM decodeBinNum
        mkNumLit ("0b" ++ String.mk num.data)
      | "8" => 
        let num <- ds.mapM decodeOctNum
        mkNumLit ("0o" ++ String.mk num.data)
      | _ => none
  match res with | some v => v | none => throw ()
| _ => throw ()
