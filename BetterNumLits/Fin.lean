import BetterNumLits.Numerals
import BetterNumLits.Notation
import BetterNumLits.Nat

abbrev Fin2 := Fin (2)

namespace Fin2
abbrev zero : Fin2 := Fin.mk 0 (by decide)
abbrev one  : Fin2 := Fin.mk 1 (by decide)

instance : Zero Fin2 := ⟨zero⟩
instance : One  Fin2 := ⟨one⟩
end Fin2

abbrev Fin8 := Fin (8)

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
end Fin8

abbrev Fin10 := Fin (10)

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

abbrev Fin16 := Fin (16)

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

instance : Zero     Fin16 := ⟨zero⟩
instance : One      Fin16 := ⟨one⟩
instance : Two      Fin16 := ⟨two⟩
instance : Three    Fin16 := ⟨three⟩
instance : Four     Fin16 := ⟨four⟩
instance : Five     Fin16 := ⟨five⟩
instance : Six      Fin16 := ⟨six⟩
instance : Seven    Fin16 := ⟨seven⟩
instance : Eight    Fin16 := ⟨eight⟩
instance : Nine     Fin16 := ⟨nine⟩
instance : Ten      Fin16 := ⟨ten⟩
instance : Eleven   Fin16 := ⟨eleven⟩
instance : Twelve   Fin16 := ⟨twelve⟩
instance : Thirteen Fin16 := ⟨thirteen⟩
instance : Fourteen Fin16 := ⟨fourteen⟩
instance : Fifteen  Fin16 := ⟨fifteen⟩
end Fin16
