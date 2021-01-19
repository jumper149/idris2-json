module Data.Scientific
-- TODO: don't export everything publicly?

import Data.Fin
import Data.List

public export
data Coefficient : (b : Nat) -> Type where
  CoeffInt : Fin (S b) ->
             Coefficient (S (S b))
  CoeffFloat : Fin (S b) ->
               List (Fin (S (S b))) ->
               Fin (S b) ->
               Coefficient (S (S b))

public export
Eq (Coefficient b) where
  (CoeffInt x) == (CoeffInt y) = x == y
  (CoeffFloat x xs x') == (CoeffFloat y ys y') = x == y && xs == ys && x' == y'
  _ == _ = False

public export
Ord (Coefficient b) where
  compare (CoeffInt x) (CoeffInt y) = compare x y
  compare (CoeffInt x) (CoeffFloat y ys y') = case compare x y of
                                               EQ => LT
                                               comp => comp
  compare (CoeffFloat x xs x') (CoeffInt y) = case compare x y of
                                               EQ => GT
                                               comp => comp
  compare (CoeffFloat x xs x') (CoeffFloat y ys y') =
    compare (FS x :: xs `snoc` FS x') (FS y :: ys `snoc` FS y')

private
prettyShowDigit : Fin 10 -> Char
prettyShowDigit x =
  case x of
       0 => '0'
       1 => '1'
       2 => '2'
       3 => '3'
       4 => '4'
       5 => '5'
       6 => '6'
       7 => '7'
       8 => '8'
       9 => '9'

private
prettyShowCoefficient : Coefficient 10 -> String
prettyShowCoefficient (CoeffInt x) = pack [ prettyShowDigit (FS x) ]
prettyShowCoefficient (CoeffFloat x xs x') =
  pack $ [ prettyShowDigit (FS x), '.' ] ++ map prettyShowDigit xs ++ [ prettyShowDigit (FS x') ]

public export
data Sign = Positive
          | Negative

public export
Eq Sign where
  Positive == Positive = True
  Negative == Negative = True
  _ == _ = False

public export
Ord Sign where
  compare Positive Positive = EQ
  compare Positive Negative = GT
  compare Negative Positive = LT
  compare Negative Negative = EQ

private
prettyShowSign : Sign -> String
prettyShowSign s = case s of
                        Positive => ""
                        Negative => "-"

public export
data Scientific : (b : Nat) -> Type where
  SciZ : Scientific b
  Sci : Sign ->
        Coefficient b ->
        Integer ->
        Scientific b

public export
Eq (Scientific b) where
  SciZ == SciZ = True
  (Sci s c e) == (Sci s' c' e') = s == s' && c == c' && e == e'
  _ == _ = False

public export
Ord (Scientific b) where
  compare SciZ SciZ = EQ
  compare SciZ (Sci s _ _) = case s of
                                  Positive => LT
                                  Negative => GT
  compare (Sci s _ _) SciZ = case s of
                                  Positive => GT
                                  Negative => LT
  compare (Sci s c e) (Sci s' c' e') =
    case (s, s') of
         (Positive, Positive) => case compare e e' of
                                      EQ => compare c c'
                                      comp => comp
         (Positive, Negative) => GT
         (Negative, Positive) => LT
         (Negative, Negative) => case compare e' e of
                                      EQ => compare c' c
                                      comp => comp

export
negate : Scientific b -> Scientific b
negate SciZ = SciZ
negate (Sci s c e) = let s' = case s of
                                   Positive => Negative
                                   Negative => Positive
                     in Sci s' c e

export
abs : Scientific b -> Scientific b
abs SciZ = SciZ
abs (Sci _ c e) = Sci Positive c e

private
||| The digits of a Nat, least significant first.
natDigits : {b : _} -> Nat -> List (Fin (S (S b)))
natDigits 0 = []
natDigits x = d :: natDigits r where
  d : Fin (S (S b))
  d = restrict (S b) $ natToInteger x
  r : Nat
  r = integerToNat $ natToInteger x `div` natToInteger (S (S b))

private
removeLeadingZeros : List (Fin (S (S b))) -> Maybe (Fin (S b), List (Fin (S (S b))))
removeLeadingZeros [] = Nothing
removeLeadingZeros (FZ :: xs) = removeLeadingZeros xs
removeLeadingZeros (FS x :: xs) = Just (x, xs)

private
fromDigits : List (Fin (S (S b))) -> Scientific (S (S b))
fromDigits ys =
  case removeLeadingZeros $ reverse ys of
       Nothing => SciZ
       Just (x, ys') => case removeLeadingZeros $ reverse ys' of
                            Nothing => Sci Positive (CoeffInt x) (cast $ length ys')
                            Just (x', xs) => Sci Positive (CoeffFloat x (reverse xs) x') (cast $ length ys')

private
fromScientificDigits : List (Fin (S (S b)), Nat) -> Scientific (S (S b))

export
fromFin : Fin (S (S b)) -> Scientific (S (S b))
fromFin FZ = SciZ
fromFin (FS x) = Sci Positive (CoeffInt x) 0

export
fromNat : {b : _} -> Nat -> Scientific (S (S b))
fromNat = fromDigits . natDigits

export
fromInteger : {b : _} -> Integer -> Scientific (S (S b))
fromInteger x = if x < 0
                   then negate $ fromIntegerPositive x
                   else fromIntegerPositive x
where
  fromIntegerPositive : Integer -> Scientific (S (S b))
  fromIntegerPositive = fromNat . integerToNat . abs

-- -- TODO: consider other implementations:
-- -- - Fractional might not terminate, because of infinite representation
-- -- - Integral doesn't sound like it would fit, but mod and div make still make sense
-- public export
-- Num (Scientific (S (S b))) where
--   SciZ + y = y
--   x + SciZ = x
--   -- TODO: plus
--   (Sci s c e) + (Sci s' c' e') = ?plus_2
--   -- TODO: mult
--   x * y = ?mult
--   -- fromInteger = fromInteger

--public export
--Neg (Scientific (S (S b))) where
--  -- negate = negate

--public export
--Abs (Scientific (S (S b))) where
--  -- abs = abs

export
prettyShowScientific : Scientific 10 -> String
prettyShowScientific SciZ = "0"
prettyShowScientific (Sci s c e) = prettyShowSign s ++ prettyShowCoefficient c ++ prettyExponent where
  prettyExponent : String
  prettyExponent = case e of
                        0 => ""
                        x => "e" ++ show x
