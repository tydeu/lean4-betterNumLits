import BetterNumLits.Numerals

universe u

class OfRadix (A : Type u) (radix : Nat) (digits : Array (Fin radix)) where
  ofRadix : A

abbrev ofRadix {A : Type u} (radix : Nat) (digits : Array (Fin radix))
  [inst : OfRadix A radix digits] : A := inst.ofRadix

@[defaultInstance low]
instance {r : Nat} {ds : Array (Fin r)} 
  : OfRadix Nat r ds := ⟨ds.foldl (fun n d => n * r + d) Nat.zero⟩

instance {A : Type u} {r : Nat} {ds : Array (Fin r)} 
  [Zero A] [Mul A] [Add A] [CoeHTCT Nat A] [CoeHTCT (Fin r) A] 
  : OfRadix A r ds := ⟨ds.foldl (fun n d => n * (coe r) + (coe d)) zero⟩
