import BetterNumLits.Numerals
import BetterNumLits.Notation
import BetterNumLits.Nat
import BetterNumLits.OfRadix 
import BetterNumLits.Fin

open Lean Syntax

def digitToStx : Char -> MacroM Syntax
| '0' => `((0))
| '1' => `((1))
| '2' => `((2))
| '3' => `((3))
| '4' => `((4))
| '5' => `((5))
| '6' => `((6))
| '7' => `((7))
| '8' => `((8))
| '9' => `((9))
| 'a' => `((10))
| 'A' => `((10))
| 'b' => `((11))
| 'B' => `((11))
| 'c' => `((12))
| 'C' => `((12))
| 'd' => `((13))
| 'D' => `((13))
| 'e' => `((14))
| 'E' => `((14))
| 'f' => `((15))
| 'F' => `((15))
| _ => Syntax.missing

partial def digitsToStxList
  (str : String) (off : String.Pos) 
: MacroM (List Syntax) := do
  if str.atEnd off then 
    []
  else 
    let d <- digitToStx (str.get off) 
    let ds <- digitsToStxList str (str.next off)
    d :: ds

def digitsToStx 
  (radix : Syntax) (str : String) (off : String.Pos)
: MacroM Syntax := do
  let digits := quote $ List.toArray (<- digitsToStxList str off)
  `(ofRadix $radix $digits)

def expandRadixNumLitFrom (src : Syntax) (str : String) : MacroM Syntax := do
  let len := str.length
  if len == 0 then 
    Macro.throwErrorAt src "empty numLit"
  else 
    let c := str.get 0
    if len == 1 then 
      digitToStx c
    else
      if c == '0' then 
        let c := str.get 1
        if c == 'x' || c == 'X' then 
          digitsToStx (<- `((16))) str 2
        else if c == 'b' || c == 'B' then
          digitsToStx (<- `((2))) str 2
        else if c == 'o' || c == 'O' then
          digitsToStx (<- `((8))) str 2
        else if c.isDigit then
          digitsToStx (<- `((10))) str 0
        else 
          Macro.throwErrorAt src "invalid numLit prefix"
      else if c.isDigit then
        digitsToStx (<- `((10))) str 0
      else 
        Macro.throwErrorAt src "invalid numLit"

syntax:max (name := radixNum) (priority := default + default) num : term

@[macro radixNum] 
def expandRadixNum : Macro
| `( $n:numLit ) =>
  match isLit? numLitKind n with
  | some str => expandRadixNumLitFrom n str
  | _        => Macro.throwErrorAt n "invalid radixLit"
| stx => Macro.throwErrorAt stx "invalid radixLit"
