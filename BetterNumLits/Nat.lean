import BetterNumLits.Numerals
import BetterNumLits.Digits

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

@[defaultInstance low] instance : Zero      Nat := ⟨zero⟩ 
@[defaultInstance low] instance : One       Nat := ⟨one⟩
@[defaultInstance low] instance : Two       Nat := ⟨two⟩
@[defaultInstance low] instance : Three     Nat := ⟨three⟩
@[defaultInstance low] instance : Four      Nat := ⟨four⟩
@[defaultInstance low] instance : Five      Nat := ⟨five⟩
@[defaultInstance low] instance : Six       Nat := ⟨six⟩
@[defaultInstance low] instance : Seven     Nat := ⟨seven⟩
@[defaultInstance low] instance : Eight     Nat := ⟨eight⟩
@[defaultInstance low] instance : Nine      Nat := ⟨nine⟩
@[defaultInstance low] instance : Ten       Nat := ⟨ten⟩
@[defaultInstance low] instance : Eleven    Nat := ⟨eleven⟩
@[defaultInstance low] instance : Twelve    Nat := ⟨twelve⟩
@[defaultInstance low] instance : Thirteen  Nat := ⟨thirteen⟩
@[defaultInstance low] instance : Fourteen  Nat := ⟨fourteen⟩
@[defaultInstance low] instance : Fifteen   Nat := ⟨fifteen⟩
@[defaultInstance low] instance : Sixteen   Nat := ⟨sixteen⟩

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
