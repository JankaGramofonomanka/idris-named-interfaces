module Semigroup


implementation [add] Semigroup Integer where
  (<+>) = (+)

implementation [mul] Semigroup Integer where
  (<+>) = (*)


threePlusThree  = (3 <+> 3) @{add}
threeTimesThree = (3 <+> 3) @{mul}
