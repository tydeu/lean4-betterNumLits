import BetterNumLits.Digits
import BetterNumLits.Nat
import BetterNumLits.OfRadix 
import BetterNumLits.Fin

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
