module Acov.Frontend.VInt
  ( VInt,
    makeVInt,
    basicVInt,
    vIntWidth,
    VISchema (..),
    baseVISchema,
    checkVInt,
    applyUnOp,
    applyBinOp,
    applyCond,
    printVInt,
  )
where

import Acov.Frontend.Hashable (Hashable (..), hashCombine)
import Acov.Frontend.Operators (BinOp (..), UnOp (..))
import Control.Exception.Base (assert)
import Data.Bits
  ( Bits (complement, popCount, shift, shiftR, xor, (.&.), (.|.)),
  )
import Data.Maybe (fromJust, isJust, isNothing)

data VInt = VInt !(Maybe Int) !Bool !Integer
  deriving (Eq)

instance Hashable VInt where
  hash (VInt w s n) =
    hashCombine (hash w) (hashCombine (hash s) (hash n))

basicVInt :: Integer -> VInt
basicVInt = VInt Nothing False

vIntValue :: VInt -> Integer
vIntValue (VInt _ _ n) = n

vIntWidth :: VInt -> Maybe Int
vIntWidth (VInt w _ _) = w

makeVInt :: Maybe Integer -> Bool -> Integer -> Either String VInt
makeVInt Nothing sgn n = Right $ VInt Nothing sgn n
makeVInt (Just w) sgn n
  | w < 1 = Left "Integer width must be positive."
  | sgn && w == 1 = Left "Cannot form a 1-bit signed integer."
  | w > 1000 = Left "Implausibly large width."
  | shiftR (abs n) bitsAvailable /= 0 = Left "Integer literal doesn't fit in the given width."
  | otherwise = Right $ VInt (Just intw) sgn n
  where
    intw = fromInteger w
    bitsAvailable = if sgn then intw - 1 else intw

data VISchema = VISchema
  { viRequireWidth :: !Bool,
    viAllowWidth :: !Bool,
    viAllowSign :: !Bool,
    viMinValue :: !(Maybe Integer)
  }

baseVISchema :: VISchema
baseVISchema = VISchema False True True Nothing

checkMinValue :: Maybe Integer -> Integer -> Maybe String
checkMinValue Nothing _ = Nothing
checkMinValue (Just m) n =
  if n < m
    then Just $ "must be at least " ++ show m ++ "."
    else Nothing

checkVInt :: VInt -> VISchema -> Maybe String
checkVInt (VInt w sgn n) schema
  | viRequireWidth schema && isNothing w = Just "needs a width."
  | isJust w && not (viAllowWidth schema) = Just "shouldn't have a width."
  | sgn && not (viAllowSign schema) = Just "may not be signed."
  | otherwise = checkMinValue (viMinValue schema) n

{-@ clamp :: {a:VInt (Just w) sgn n | w > 0 && w <= 999} -> VInt @-}
clamp :: VInt -> VInt
clamp (VInt (Just w) sgn n) =
  VInt (Just w) sgn $
    if n < 0
      then
        if shift (- n) (- w) /= 0
          then mod (- n) (2 ^ w)
          else n
      else mod n (2 ^ w)
clamp vi = vi

combineWidth :: Maybe Int -> Maybe Int -> Maybe Int
combineWidth w0 w1
  | isJust w0 && isJust w1 = Just (max (fromJust w0) (fromJust w1))
  | isJust w0 = w0
  | otherwise = w1

makeCombinedVInt :: VInt -> VInt -> Integer -> VInt
makeCombinedVInt (VInt w0 s0 _) (VInt w1 s1 _) =
  clamp . VInt (combineWidth w0 w1) (s0 || s1)

vIntQuotRem :: VInt -> VInt -> (VInt, VInt)
vIntQuotRem v0 v1 = (mkQR q, mkQR r)
  where
    (q, r) = quotRem (vIntValue v0) (vIntValue v1)
    mkQR = makeCombinedVInt v0 v1

liftUnOp :: (Integer -> Integer) -> VInt -> VInt
liftUnOp op (VInt w sgn n) = clamp $ VInt w sgn (op n)

liftBinOp ::
  (Integer -> Integer -> Integer) ->
  VInt ->
  VInt ->
  VInt
liftBinOp op a b =
  makeCombinedVInt a b (vIntValue a `op` vIntValue b)

instance Ord VInt where
  -- Note: This is a bit naughty because it isn't a total order.
  compare (VInt _ _ a) (VInt _ _ b) = compare a b

instance Num VInt where
  (+) = liftBinOp (+)
  (*) = liftBinOp (*)
  abs = liftUnOp abs
  signum = liftUnOp signum
  fromInteger = basicVInt
  negate = liftUnOp negate

instance Real VInt where
  toRational = toRational . vIntValue

instance Enum VInt where
  toEnum = basicVInt . toInteger
  fromEnum = fromInteger . vIntValue

instance Integral VInt where
  quotRem = vIntQuotRem
  toInteger = vIntValue

applyUnOp :: UnOp -> VInt -> Either String VInt
applyUnOp LogNot (VInt Nothing _ _) =
  Left "cannot apply logical negation with no width."
applyUnOp LogNot (VInt (Just w) sgn n) =
  if w /= 1
    then Left "cannot apply logical negation with width != 1."
    else
      assert
        (not sgn)
        assert
        (n == 0 || n == 1)
        Right
        $ VInt (Just 1) False (1 - n)
applyUnOp BitNot (VInt Nothing _ _) =
  Left "cannot apply bitwise negation with no width."
applyUnOp BitNot (VInt (Just w) sgn n) =
  if sgn
    then Left "cannot apply bitwise negation to a signed number."
    else Right $ VInt (Just w) False (2 ^ w - n - 1)
applyUnOp RedAnd (VInt Nothing _ _) =
  Left "cannot apply reduction AND with no width."
applyUnOp RedAnd (VInt (Just w) _ n) =
  Right $ VInt (Just 1) False (if 2 ^ w - n - 1 == 0 then 1 else 0)
applyUnOp RedOr (VInt _ _ n) =
  Right $ VInt (Just 1) False (if n == 0 then 0 else 1)
applyUnOp RedNand (VInt Nothing _ _) = Left "cannot apply reduction NAND with no width."
applyUnOp RedNand (VInt (Just w) _ n) =
  Right $ VInt (Just 1) False (if 2 ^ w - n - 1 == 0 then 0 else 1)
applyUnOp RedNor (VInt _ _ n) =
  Right $ VInt (Just 1) False (if n == 0 then 1 else 0)
applyUnOp RedXor (VInt _ _ n) =
  Right $ VInt (Just 1) False (if even (popCount n) then 0 else 1)
applyUnOp RedXnor (VInt _ _ n) =
  Right $ VInt (Just 1) False (if even (popCount n) then 1 else 0)
applyUnOp UPlus vi = Right vi
applyUnOp UMinus vi = Right $ liftUnOp negate vi

fromBool :: Bool -> VInt
fromBool b = VInt (Just 1) False (if b then 1 else 0)

applyBinOp' :: BinOp -> VInt -> VInt -> Either String VInt
applyBinOp' Times a b = Right $ a * b
applyBinOp' Divide a b = Right $ quot a b
applyBinOp' Modulo a b = Right $ mod a b
applyBinOp' Plus a b = Right $ a + b
applyBinOp' Minus a b = Right $ a - b
applyBinOp' LShift a b
  | toInteger b < 0 = Left "shift amount must be non-negative."
  | toInteger b > 1000 = Left "shift amount is implausibly large."
  | otherwise = Right $ fromInteger (shift (toInteger a) (fromInteger (toInteger b)))
applyBinOp' RShift a b
  | toInteger b < 0 = Left "shift amount must be non-negative."
  | toInteger b > 1000 = Left "shift amount is implausibly large."
  | otherwise = Right $ fromInteger (shift (toInteger a) (- (fromInteger (toInteger b))))
applyBinOp' Greater a b = Right $ fromBool $ a > b
applyBinOp' GreaterEq a b = Right $ fromBool $ a >= b
applyBinOp' Less a b = Right $ fromBool $ a < b
applyBinOp' LessEq a b = Right $ fromBool $ a <= b
applyBinOp' LogEq a b = Right $ fromBool $ toInteger a == toInteger b
applyBinOp' LogNeq a b = Right $ fromBool $ toInteger a /= toInteger b
applyBinOp' CaseEq a b = Right $ fromBool $ toInteger a == toInteger b
applyBinOp' CaseNeq a b = Right $ fromBool $ toInteger a /= toInteger b
applyBinOp' BitAnd a b = Right $ liftBinOp (.&.) a b
applyBinOp' BitXor a b = Right $ liftBinOp xor a b
applyBinOp' BitXnor a b = Right $ liftUnOp complement $ liftBinOp xor a b
applyBinOp' BitOr a b = Right $ liftBinOp (.|.) a b
applyBinOp' LogAnd (VInt (Just w0) sgn0 n0) (VInt (Just w1) sgn1 n1) =
  if w0 /= 1 || w1 /= 1
    then Left "operand for && has a width greater than 1."
    else
      assert
        (not sgn0)
        assert
        (not sgn1)
        Right
        $ VInt (Just 1) False (if n0 + n1 == 2 then 1 else 0)
applyBinOp' LogAnd _ _ =
  Left "operand for && has no width."
applyBinOp' LogOr (VInt (Just w0) sgn0 n0) (VInt (Just w1) sgn1 n1) =
  if w0 /= 1 || w1 /= 1
    then Left "operand for || has a width greater than 1."
    else
      assert
        (not sgn0)
        assert
        (not sgn1)
        Right
        $ VInt (Just 1) False (if n0 + n1 == 0 then 0 else 1)
applyBinOp' LogOr _ _ =
  Left "operand for || has no width."

compatibleWidths :: VInt -> VInt -> Bool
compatibleWidths a b = cw' (vIntWidth a) (vIntWidth b)
  where
    cw' (Just w) (Just w') = w == w'
    cw' _ _ = True

applyBinOp :: BinOp -> VInt -> VInt -> Either String VInt
applyBinOp op a b =
  if not (compatibleWidths a b)
    then Left "operator widths are incompatible."
    else clamp <$> applyBinOp' op a b

applyCond :: VInt -> VInt -> VInt -> Either String VInt
applyCond (VInt Nothing _ _) _ _ =
  Left "test for conditional has no width."
applyCond (VInt (Just x) sgn a) b c =
  if x /= 1
    then Left "test for conditional has a width other than 1."
    else
      assert
        (not sgn)
        assert
        (a == 0 || a == 1)
        Right
        $ makeCombinedVInt b c (vIntValue (if a == 0 then c else b))

printVInt :: VInt -> String
printVInt (VInt mw sgn n) = maybe "" show mw ++ (if sgn then "'sd" else "'d") ++ show n
