import BetterNumLits.Numerals

namespace Nat

abbrev one      := nat_lit 1
abbrev two      := nat_lit 2
abbrev three    := nat_lit 3
abbrev four     := nat_lit 4
abbrev five     := nat_lit 5
abbrev six      := nat_lit 6
abbrev seven    := nat_lit 7
abbrev eight    := nat_lit 8
abbrev nine     := nat_lit 9
abbrev ten      := nat_lit 10
abbrev eleven   := nat_lit 11
abbrev twelve   := nat_lit 12
abbrev thirteen := nat_lit 13
abbrev fourteen := nat_lit 14
abbrev fifteen  := nat_lit 15
abbrev sixteen  := nat_lit 16

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
