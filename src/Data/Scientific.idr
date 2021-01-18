module Data.Scientific

import Data.Precision

import Data.Fin
import Data.List
import Data.Vect

data Scientific : Type where
  SciZ : Scientific
  Sci : Precision 10 n -> Scientific

prettyShow : Scientific -> String
