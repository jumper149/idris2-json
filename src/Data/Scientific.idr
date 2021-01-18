module Data.Scientific

import Data.Precision

import Data.Fin
import Data.List
import Data.Vect

public export
data Scientific : Type where
  SciZ : Scientific
  Sci : Precision 10 n -> Scientific

export
Eq Scientific where
  SciZ == SciZ = True
  Sci x == Sci y = eqPrecs x y
  _ == _ = False

-- TODO: don't use Show?
export
Show Scientific where
  show SciZ = "0"
  show (Sci x) = show x
