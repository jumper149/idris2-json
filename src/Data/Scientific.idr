module Data.Scientific

import Data.Precision

import Data.Fin
import Data.List
import Data.Vect

public export
data Sign = Positive
          | Negative

export
Eq Sign where
  Positive == Positive = True
  Negative == Negative = True
  _ == _ = False

export
Ord Sign where
  compare Positive Positive = EQ
  compare Positive Negative = GT
  compare Negative Positive = LT
  compare Negative Negative = EQ

public export
data Scientific : Type where
  SciZ : Scientific
  Sci : Sign -> Precision 10 n -> Scientific

export
Eq Scientific where
  SciZ == SciZ = True
  Sci sign prec == Sci sign' prec' = sign == sign' && eqPrecs prec prec'
  _ == _ = False

export
Ord Scientific where
  compare SciZ SciZ = EQ
  compare SciZ (Sci sign prec) =
    case sign of
         Positive => LT
         Negative => GT
  compare (Sci sign prec) SciZ =
    case sign of
         Positive => GT
         Negative => LT
  compare (Sci sign prec) (Sci sign' prec') =
    case compare sign sign' of
         LT => LT
         EQ => case sign of
                    Positive => comparePrecs prec prec'
                    Negative => comparePrecs prec' prec
         GT => GT

-- TODO: don't use Show?
export
Show Scientific where
  show SciZ = "0"
  show (Sci sign prec) = (++ show prec) $ case sign of
                                               Positive => ""
                                               Negative => "-"
