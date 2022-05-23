module NamedInterfaces

-- Semigroup ------------------------------------------------------------------
implementation [add] Semigroup Integer where
  (<+>) = (+)

implementation [mul] Semigroup Integer where
  (<+>) = (*)


threePlusThree  = (3 <+> 3) @{add}
threeTimesThree = (3 <+> 3) @{mul}

implementation [addm] Monoid Integer using add where
  neutral = 0

implementation [mulm] Monoid Integer using mul where
  neutral = 1

threeAdd = (3 <+> neutral @{addm}) @{add}
threeMul = (3 <+> neutral @{mulm}) @{mul}



-------------------------------------------------------------------------------
data MyList a = Nil | (::) a (MyList a)

myMap : (a -> b) -> MyList a -> MyList b
myMap f Nil = Nil
myMap f (x :: xs) = f x :: myMap f xs

myConcat : Monoid a => MyList a -> a
myConcat Nil = neutral
myConcat (x :: xs) = x <+> myConcat xs

interface ShowElems a where
  showElems : MyList a -> String

implementation [comma] Show a => ShowElems a where
  showElems Nil = ""
  showElems [x] = show x
  showElems (x :: xs) = show x ++ myConcat (myMap ((", " ++) . show) xs)


implementation [space] Show a => ShowElems a where
  showElems Nil = ""
  showElems [x] = show x
  showElems (x :: xs) = show x ++ myConcat (myMap ((" " ++) . show) xs)

--implementation ShowElems a => Show (MyList a) where
--  show l = "[ " ++ showElems l ++ " ]"

implementation [showMyList] ShowElems a => Show (MyList a) where
  show l = "[ " ++ showElems l ++ " ]"


l : MyList Int
l = 1 :: (2 :: Nil)

--example1 = show l @{comma}
--example2 = show l @{space}

example1 = show l @{showMyList @{comma}}
example2 = show l @{showMyList @{space}}



