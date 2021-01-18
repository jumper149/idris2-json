module Data.Precision

import Data.Fin
import Data.List
import Data.Vect

-- TODO: this needs to be accessible (maybe from Data.Nat?)
private
greaterNat : Nat -> Nat -> Nat
greaterNat k j with (compare k j)
  greaterNat k j | LT = j
  greaterNat k j | EQ = k
  greaterNat k j | GT = k

-- Can construct all x with:
--   b > x >= 1
export
data Coefficient : (b : Nat) -> (n : Nat) -> Type where
  Coeff : (Fin (S b)) ->
          Vect n (Fin (S (S b))) ->
          Coefficient (S (S b)) n

export
Show (Coefficient 10 n) where
  show (Coeff x xs) =
    show (finToNat (weaken x)) ++ "." ++ concat (show . finToNat <$> xs)

private
eqDigits : Vect n (Fin (S (S b))) -> Vect m (Fin (S (S b))) -> Bool
eqDigits [] ys = all (== 0) ys
eqDigits xs [] = all (== 0) xs
eqDigits (x :: xs) (y :: ys) = x == y && eqDigits xs ys

private
eqCoeffs : Coefficient b n -> Coefficient b m -> Bool
eqCoeffs (Coeff x xs) (Coeff y ys) = x == y && eqDigits xs ys

private
plusCoeffs : Coefficient b n -> Coefficient b m -> (Coefficient b (greaterNat n m), Bool)

||| Can construct all x with:
|||   x /= 0
public export
data Precision : (b : Nat) -> (n : Nat) -> Type where
  Prec : Coefficient b n -> Integer -> Precision b n

export
Show (Precision 10 n) where
  show (Prec coeff expon) = show coeff ++ "e" ++ show expon

-- TODO: export?
export
eqPrecs : Precision b n -> Precision b m -> Bool
eqPrecs (Prec coeff expon) (Prec coeff' expon') = expon == expon' && eqCoeffs coeff coeff'

export
plusPrecs : Precision b n -> Precision b m -> Precision b (greaterNat n m)
plusPrecs (Prec x expon) (Prec y expon') with (compare expon expon')
  plusPrecs (Prec x expon) (Prec y expon') | LT = ?plusPrecs_rhs_1
  plusPrecs (Prec x expon) (Prec y expon') | EQ with (plusCoeffs x y)
    plusPrecs (Prec x expon) (Prec y expon') | EQ | (coeff, overhead) =
      Prec coeff $ if overhead
                      then expon + 1
                      else expon
  plusPrecs (Prec x expon) (Prec y expon') | GT = ?plusPrecs_rhs_3

export
multPrecs : Precision b n -> Precision b m -> Precision b (greaterNat n m)
