import BetterNumLits.Digits
import BetterNumLits.Numerals
import BetterNumLits.Notation
import BetterNumLits.Nat
import BetterNumLits.OfRadix 
import BetterNumLits.Fin

open Lean Syntax

def digitToStx : Char -> Syntax
| '0' => mkLit ``num0 "(0)"
| '1' => mkLit ``num1 "(1)"
| '2' => mkLit ``num2 "(2)"
| '3' => mkLit ``num3 "(3)"
| '4' => mkLit ``num4 "(4)"
| '5' => mkLit ``num5 "(5)"
| '6' => mkLit ``num6 "(6)"
| '7' => mkLit ``num7 "(7)"
| '8' => mkLit ``num8 "(8)"
| '9' => mkLit ``num9 "(9)"
| 'a' => mkLit ``num10 "(10)"
| 'A' => mkLit ``num10 "(10)"
| 'b' => mkLit ``num11 "(11)"
| 'B' => mkLit ``num11 "(11)"
| 'c' => mkLit ``num12 "(12)"
| 'C' => mkLit ``num12 "(12)"
| 'd' => mkLit ``num13 "(13)"
| 'D' => mkLit ``num13 "(13)"
| 'e' => mkLit ``num14 "(14)"
| 'E' => mkLit ``num14 "(14)"
| 'f' => mkLit ``num15 "(15)"
| 'F' => mkLit ``num15 "(15)"
| _ => Syntax.missing

def binDigitToStx : Char -> Syntax
| '0' => mkCIdent ``bin0
| '1' => mkCIdent ``bin1
| _ => Syntax.missing

def octDigitToStx : Char -> Syntax
| '0' => mkCIdent ``oct0
| '1' => mkCIdent ``oct1
| '2' => mkCIdent ``oct2
| '3' => mkCIdent ``oct3
| '4' => mkCIdent ``oct4
| '5' => mkCIdent ``oct5
| '6' => mkCIdent ``oct6
| '7' => mkCIdent ``oct7
| _ => Syntax.missing

def hexDigitToStx : Char -> Syntax
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
  `(ofRadix $radix $digits)

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
            hexDigitToStx (str.get 2)
          else do
            digitsToStx (<- `((16))) digitToStx str 2
        else if c == 'b' || c == 'B' then 
          if len == 3 then 
            binDigitToStx (str.get 2)
          else do
            digitsToStx (<- `((2))) digitToStx str 2
        else if c == 'o' || c == 'O' then 
          if len == 3 then 
            octDigitToStx (str.get 2)
          else do
            digitsToStx (<- `((8))) digitToStx str 2
        else if c.isDigit then do
          digitsToStx (<- `((10))) digitToStx str 0
        else 
          Macro.throwErrorAt stx "invalid num lit prefix"
      else if c.isDigit then do
        digitsToStx (<- `((10))) digitToStx str 0
      else 
        Macro.throwErrorAt stx "invalid num lit"

@[macro numLit]
def expandNumLit : Macro
  | stx => 
    match isLit? numLitKind stx with
      | some str => expandRadixLit stx str
      | _        => Macro.throwErrorAt stx "invalid num lit"
