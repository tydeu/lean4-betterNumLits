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
        if c == 'x' || c == 'X' then do
          digitsToStx (<- `((16))) digitToStx str 2
        else if c == 'b' || c == 'B' then do
          digitsToStx (<- `((2))) digitToStx str 2
        else if c == 'o' || c == 'O' then do
          digitsToStx (<- `((8))) digitToStx str 2
        else if c.isDigit then do
          digitsToStx (<- `((10))) digitToStx str 0
        else 
          Macro.throwErrorAt stx "invalid numLit prefix"
      else if c.isDigit then do
        digitsToStx (<- `((10))) digitToStx str 0
      else 
        Macro.throwErrorAt stx "invalid numLit"

@[macro numLit]
def expandNumLit : Macro
  | stx => 
    match isLit? numLitKind stx with
      | some str => expandRadixLit stx str
      | _        => Macro.throwErrorAt stx "invalid numLit"
