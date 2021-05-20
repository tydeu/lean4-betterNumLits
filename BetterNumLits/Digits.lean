universe u

--------------------------------------------------------------------------------
-- Generic Digit Classes
--------------------------------------------------------------------------------

class Zero  (A : Type u) := zero  : A
class One   (A : Type u) := one   : A
class Two   (A : Type u) := two   : A
class Three (A : Type u) := three : A
class Four  (A : Type u) := four  : A
class Five  (A : Type u) := five  : A
class Six   (A : Type u) := six   : A
class Seven (A : Type u) := seven : A
class Eight (A : Type u) := eight : A
class Nine  (A : Type u) := nine  : A

export Zero  (zero)
export One   (one)
export Two   (two)
export Three (three)
export Four  (four)
export Five  (five)
export Six   (six)
export Seven (seven)
export Eight (eight)
export Nine  (nine)

--------------------------------------------------------------------------------
-- Radix-Specific Digit Classes
--------------------------------------------------------------------------------

class Bin0 (A : Type u) := bin0 : A
class Bin1 (A : Type u) := bin1 : A

export Bin0 (bin0)
export Bin1 (bin1)

class Oct0 (A : Type u) := oct0 : A
class Oct1 (A : Type u) := oct1 : A
class Oct2 (A : Type u) := oct2 : A
class Oct3 (A : Type u) := oct3 : A
class Oct4 (A : Type u) := oct4 : A
class Oct5 (A : Type u) := oct5 : A
class Oct6 (A : Type u) := oct6 : A
class Oct7 (A : Type u) := oct7 : A

export Oct0 (oct0)
export Oct1 (oct1)
export Oct2 (oct2)
export Oct3 (oct3)
export Oct4 (oct4)
export Oct5 (oct5)
export Oct6 (oct6)
export Oct7 (oct7)

class Hex0 (A : Type u) := hex0 : A
class Hex1 (A : Type u) := hex1 : A
class Hex2 (A : Type u) := hex2 : A
class Hex3 (A : Type u) := hex3 : A
class Hex4 (A : Type u) := hex4 : A
class Hex5 (A : Type u) := hex5 : A
class Hex6 (A : Type u) := hex6 : A
class Hex7 (A : Type u) := hex7 : A
class Hex8 (A : Type u) := hex8 : A
class Hex9 (A : Type u) := hex9 : A
class HexA (A : Type u) := hexA : A
class HexB (A : Type u) := hexB : A
class HexC (A : Type u) := hexC : A
class HexD (A : Type u) := hexD : A
class HexE (A : Type u) := hexE : A
class HexF (A : Type u) := hexF : A

export Hex0 (hex0)
export Hex1 (hex1)
export Hex2 (hex2)
export Hex3 (hex3)
export Hex4 (hex4)
export Hex5 (hex5)
export Hex6 (hex6)
export Hex7 (hex7)
export Hex8 (hex8)
export Hex9 (hex9)
export HexA (hexA)
export HexB (hexB)
export HexC (hexC)
export HexD (hexD)
export HexE (hexE)
export HexF (hexF)
