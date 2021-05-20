import BetterNumLits.Digits

universe u

class OfRadix (A : Type u) (radix : Nat) (digits : Array (Fin radix)) where
  ofRadix : A

export OfRadix (ofRadix)

@[defaultInstance low]
instance {r : Nat} {ds : Array (Fin r)} 
  : OfRadix Nat r ds := ⟨ds.foldl (fun n d => n * r + d) Nat.zero⟩

instance {A : Type u} {r : Nat} {ds : Array (Fin r)} 
  [Zero A] [Mul A] [Add A] [CoeHTCT Nat A] [CoeHTCT (Fin r) A] 
  : OfRadix A r ds := ⟨ds.foldl (fun n d => n * (coe r) + (coe d)) zero⟩
