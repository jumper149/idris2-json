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

-- TODO: What happens, when Integer is negative? Low priority, since this is private.
private
||| The digits of an Integer, least significant first.
integerDigits : {b : _} -> Integer -> List (Fin (S (S b)))
integerDigits 0 = []
integerDigits x = d :: integerDigits r where
  d : Fin (S (S b))
  d = restrict (S b) x
  r : Integer
  r = x `div` natToInteger (S (S b))

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

export
fromFin : Fin (S (S b)) -> Scientific (S (S b))
fromFin FZ = SciZ
fromFin (FS x) = Sci Positive (CoeffInt x) 0

export
fromInteger : {b : _} -> Integer -> Scientific (S (S b))
fromInteger x = if x < 0
                   then negate $ fromIntegerPositive x
                   else fromIntegerPositive x
where
  fromIntegerPositive : Integer -> Scientific (S (S b))
  fromIntegerPositive = fromDigits . integerDigits . abs

export
fromNat : {b : _} -> Nat -> Scientific (S (S b))
fromNat = fromInteger . natToInteger

||| All bits of a Coefficient, least significant first.
coefficientBits : Coefficient (S (S b)) -> List (Fin (S (S b)))
coefficientBits (CoeffInt x) = [FS x]
coefficientBits (CoeffFloat x xs x') = reverse $ FS x :: xs ++ [FS x']

-- TODO: currently add and subtract require equal lengths for the coefficients
-- adds 1 bit (might be zero)
addBits : {b : _} ->
          (carry : Bool) ->
          List (Fin (S (S b))) ->
          List (Fin (S (S b))) ->
          List (Fin (S (S b)))
addBits False [] [] = [FZ]
addBits True [] [] = [FS FZ]
addBits carry p@[] q@(y :: ys) = addBits carry q p -- TODO: case can be avoided when one is larger than the other
addBits False (x :: xs) [] = x :: addBits False xs []
addBits True (x :: xs) [] = case strengthen $ FS x of
                                 Left _ => FZ :: addBits True xs []
                                 Right x' => x' :: addBits False xs []
addBits carry (x :: xs) (y :: ys) =
  restrict (S b) addition :: addBits (addition >= natToInteger (S (S b))) xs ys where
    addition : Integer
    addition = if carry
                  then finToInteger x + finToInteger y + 1
                  else finToInteger x + finToInteger y

subtractBits : {b : _} ->
               (borrow : Bool) ->
               List (Fin (S (S b))) ->
               List (Fin (S (S b))) ->
               List (Fin (S (S b)))
subtractBits False [] [] = [FZ]
subtractBits True [] [] = ?thatShouldntHappen1
subtractBits borrow [] (x :: xs) = ?thatShouldntHappen2
subtractBits False (x :: xs) [] = x :: subtractBits False xs []
subtractBits True (FZ :: xs) [] = last :: subtractBits True xs []
subtractBits True ((FS x) :: xs) [] = weaken x :: subtractBits False xs []
subtractBits borrow (x :: xs) (y :: ys) = ?subtractBits_rhs_4

export
plus : {b : _} -> Scientific (S (S b)) -> Scientific (S (S b)) -> Scientific (S (S b))
plus SciZ y = y
plus x SciZ = x
-- TODO: finish plus
plus x@(Sci s c e) y@(Sci s' c' e') =
  case compare e e' of
       GT => plus y x
       _ => ?asdasd where
         exponentDifference : Nat
         exponentDifference = integerToNat $ e' - e
         bits : List (Fin (S (S b)))
         bits = addBits False (coefficientBits c ++ replicate exponentDifference FZ) (coefficientBits c')
         s'' : Sign
         c'' : Coefficient (S (S b))
         e'' : Integer
         e'' = e' + ?additionalExponent bits

||| Multiply two Coefficients and return True in the Bool, when the product is greater than the base.
multCoefficents : {b : _} -> Coefficient (S (S b)) -> Coefficient (S (S b)) -> (Coefficient (S (S b)), Bool)
multCoefficents (CoeffInt x) (CoeffInt y) =
  case integerToFin res (S b) of
       Nothing => (CoeffInt $ restrict b res, True)
       Just fin => (CoeffInt fin, False)
where
  res : Integer
  res = (finToInteger x + 1) * (finToInteger y + 1) - 1
multCoefficents a@(CoeffInt x) b@(CoeffFloat y ys y') = multCoefficents b a
-- TODO: finish multCoefficents
multCoefficents (CoeffFloat x xs x') (CoeffInt y) = ?multCoefficents_rhs_2
multCoefficents (CoeffFloat x xs x') (CoeffFloat y ys y') = ?multCoefficents_rhs_3

export
mult : {b : _} -> Scientific (S (S b)) -> Scientific (S (S b)) -> Scientific (S (S b))
mult SciZ y = SciZ
mult x SciZ = SciZ
mult (Sci s c e) (Sci s' c' e') = Sci s'' c'' e'' where
  coefficientPair : (Coefficient (S (S b)), Bool)
  coefficientPair = multCoefficents c c'
  s'' : Sign
  s'' = if s == s'
           then Positive
           else Negative
  c'' : Coefficient (S (S b))
  c'' = fst coefficientPair
  e'' : Integer
  e'' = if snd coefficientPair
           then e + e' + 1
           else e + e'

-- -- TODO: consider other implementations:
-- -- - Fractional might not terminate, because of infinite representation
-- -- - Integral doesn't sound like it would fit, but mod and div make still make sense
-- public export
-- Num (Scientific (S (S b))) where
--   -- (+) = plus
--   -- (*) = mult
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
