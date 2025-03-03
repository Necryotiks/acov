module Acov.Frontend.Expressions
  ( run,
    Expression (..),
    Record (..),
    BitsRecord (..),
    Group (..),
    Module (..),
    Slice (..),
    sliceWidth,
  )
where

import Acov.Frontend.ErrorsOr
  ( ErrorsOr,
    bad1,
    eaToEO,
    eoMaybe,
    eoToEA,
    good,
    mapEO,
  )
import Acov.Frontend.Hashable (Hashable (..), hashCombine)
import Acov.Frontend.Operators (BinOp, UnOp)
import qualified Acov.Frontend.Parser as P
import Acov.Frontend.Ranged
  ( LCRange,
    Ranged (..),
    copyRange,
    rangedData,
    rangedRange,
  )
import Acov.Frontend.SymbolTable
  ( Symbol,
    SymbolTable,
    stTraverseWithSym,
  )
import qualified Acov.Frontend.Symbols as S
import Acov.Frontend.VInt
  ( VISchema (viAllowSign, viAllowWidth, viMinValue),
    VInt,
    baseVISchema,
    checkVInt,
  )
import Control.Applicative (Applicative (liftA2), liftA3)

{-
  The expressions pass tightens up our representation of expressions,
  making sure that stuff that looks like it should be an integer
  really is, that bit selects only apply to symbols and so on.
-}
data Expression
  = ExprSym !Symbol
  | ExprInt !VInt
  | ExprSel
      !(Ranged Symbol)
      !(Ranged Expression)
      !(Maybe (Ranged Expression))
  | ExprConcat !(Ranged Expression) ![Ranged Expression]
  | ExprReplicate !Int !(Ranged Expression)
  | ExprUnOp !(Ranged UnOp) !(Ranged Expression)
  | ExprBinOp
      !(Ranged BinOp)
      !(Ranged Expression)
      !(Ranged Expression)
  | ExprCond
      !(Ranged Expression)
      !(Ranged Expression)
      !(Ranged Expression)

instance Hashable Expression where
  hash (ExprSym s) = hashCombine 0 (hash s)
  hash (ExprInt i) = hashCombine 1 (hash i)
  hash (ExprSel a b c) =
    hashCombine 2 (hashCombine (hash a) (hashCombine (hash b) (hash c)))
  hash (ExprConcat a b) = hashCombine 3 (hashCombine (hash a) (hash b))
  hash (ExprReplicate a b) = hashCombine 4 (hashCombine (hash a) (hash b))
  hash (ExprUnOp a b) = hashCombine 5 (hashCombine (hash a) (hash b))
  hash (ExprBinOp a b c) =
    hashCombine 6 (hashCombine (hash a) (hashCombine (hash b) (hash c)))
  hash (ExprCond a b c) =
    hashCombine 7 (hashCombine (hash a) (hashCombine (hash b) (hash c)))

data Record
  = Record
      !(Ranged Expression)
      !(Ranged P.Symbol)
      !(Maybe [(Ranged Integer, Ranged Integer)])

data BitsRecord = BitsRecord !(Ranged Expression) !(Ranged P.Symbol)

data Group
  = Group
      ![Ranged Expression]
      ![String]
      !(Either [Record] BitsRecord)

data Slice = Slice !Int !Int

instance Hashable Slice where
  hash (Slice a b) = hashCombine (hash a) (hash b)

type PortSyms = SymbolTable (Ranged Slice)

data Module = Module
  { modName :: !(Ranged P.Symbol),
    modSyms :: !PortSyms,
    modGrps :: ![Group]
  }

sliceWidth :: Slice -> Int
sliceWidth (Slice a b) = 1 + (if a < b then b - a else a - b)

tightenAtom :: S.Atom -> Expression
tightenAtom (S.AtomSym sym) = ExprSym sym
tightenAtom (S.AtomInt int) = ExprInt int

tightenSel' :: Ranged S.Expression -> ErrorsOr (Ranged Symbol)
tightenSel' rse =
  case rangedData rse of
    S.ExprAtom (S.AtomSym sym) -> good $ copyRange rse sym
    _other ->
      bad1
        ( copyRange
            rse
            "bit selection applied to something other than a symbol."
        )

tightenSel ::
  Ranged S.Expression ->
  Ranged S.Expression ->
  Maybe (Ranged S.Expression) ->
  ErrorsOr Expression
tightenSel base top bot =
  liftA3 ExprSel (tightenSel' base) (tighten top) (eoMaybe $ tighten <$> bot)

tightenConcat :: LCRange -> [Ranged S.Expression] -> ErrorsOr Expression
tightenConcat rng [] = bad1 $ Ranged rng "Empty concatenation."
tightenConcat _ (e : es) =
  liftA2 ExprConcat (tighten e) (mapEO tighten es)

tightenRepCount :: Ranged S.Expression -> ErrorsOr Int
tightenRepCount rse =
  case rangedData rse of
    S.ExprAtom (S.AtomInt int) ->
      case checkVInt int $
        baseVISchema
          { viAllowWidth = False,
            viAllowSign = False,
            viMinValue = Just 0
          } of
        Nothing ->
          let i = toInteger int
           in if i < toInteger (minBound :: Int)
                || i > toInteger (maxBound :: Int)
                then noGood "is out of bounds for an integer."
                else good $ fromInteger i
        Just req -> noGood req
    _other -> noGood "should be a literal integer."
  where
    noGood req = bad1 $ copyRange rse ("Replication count " ++ req)

tightenReplicate ::
  Ranged S.Expression ->
  Ranged S.Expression ->
  ErrorsOr Expression
tightenReplicate count val =
  liftA2 ExprReplicate (tightenRepCount count) (tighten val)

tightenUnOp :: Ranged UnOp -> Ranged S.Expression -> ErrorsOr Expression
tightenUnOp uo se = ExprUnOp uo <$> tighten se

tightenBinOp ::
  Ranged BinOp ->
  Ranged S.Expression ->
  Ranged S.Expression ->
  ErrorsOr Expression
tightenBinOp bo se0 se1 = liftA2 (ExprBinOp bo) (tighten se0) (tighten se1)

tightenCond ::
  Ranged S.Expression ->
  Ranged S.Expression ->
  Ranged S.Expression ->
  ErrorsOr Expression
tightenCond se0 se1 se2 =
  liftA3 ExprCond (tighten se0) (tighten se1) (tighten se2)

tighten' :: LCRange -> S.Expression -> ErrorsOr Expression
tighten' _ (S.ExprAtom atom) = good $ tightenAtom atom
tighten' _ (S.ExprParens se) = rangedData <$> tighten se
tighten' _ (S.ExprSel base top bot) = tightenSel base top bot
tighten' r (S.ExprConcat exprs) = tightenConcat r exprs
tighten' _ (S.ExprReplicate count val) = tightenReplicate count val
tighten' _ (S.ExprUnOp uo se) = tightenUnOp uo se
tighten' _ (S.ExprBinOp bo se0 se1) = tightenBinOp bo se0 se1
tighten' _ (S.ExprCond se0 se1 se2) = tightenCond se0 se1 se2

tighten :: Ranged S.Expression -> ErrorsOr (Ranged Expression)
tighten rse = copyRange rse <$> tighten' (rangedRange rse) (rangedData rse)

tightenRecord :: S.Record -> ErrorsOr Record
tightenRecord (S.Record expr name cover) =
  (\e -> Record e name cover) <$> tighten expr

tightenBitsRecord :: S.BitsRecord -> ErrorsOr BitsRecord
tightenBitsRecord (S.BitsRecord expr name) =
  (`BitsRecord` name) <$> tighten expr

tightenGroup :: S.Group -> ErrorsOr Group
tightenGroup (S.Group guards scopes (Left recs)) =
  do
    (guards', recs') <-
      liftA2
        (,)
        (mapEO tighten guards)
        (mapEO tightenRecord recs)
    return $ Group guards' scopes (Left recs')
tightenGroup (S.Group guards scopes (Right brec)) =
  do
    (guards', brec') <-
      liftA2
        (,)
        (mapEO tighten guards)
        (tightenBitsRecord brec)
    return $ Group guards' scopes (Right brec')

tightenBitSel :: P.Symbol -> LCRange -> VInt -> ErrorsOr Integer
tightenBitSel psym rng int =
  case checkVInt int $
    baseVISchema
      { viAllowWidth = False,
        viAllowSign = False,
        viMinValue = Just 0
      } of
    Nothing -> good $ toInteger int
    Just req ->
      bad1 $
        Ranged
          rng
          ( "Bit selection for port `"
              ++ P.symName psym
              ++ "' "
              ++ req
          )

tightenSlice :: Ranged P.Symbol -> Maybe (Ranged P.Slice) -> ErrorsOr (Ranged Slice)
tightenSlice rpsym rslice =
  case rslice of
    Nothing -> good $ copyRange rpsym $ Slice 0 0
    Just (Ranged rng (P.Slice va vb)) ->
      do
        (a, b) <-
          liftA2
            (,)
            (tightenBitSel psym rng va)
            (tightenBitSel psym rng vb)
        if abs (a - b) > toInteger (maxBound :: Int)
          then
            bad1
              ( Ranged
                  rng
                  "Difference in bit positions overflows an int."
              )
          else
            if (- (max (abs a) (abs b))) < toInteger (minBound :: Int)
              then
                bad1
                  ( Ranged
                      rng
                      "A bit position in the slice overflows an int."
                  )
              else good $ Ranged rng $ Slice (fromInteger a) (fromInteger b)
  where
    psym = rangedData rpsym

tightenPortSyms ::
  SymbolTable (Maybe (Ranged P.Slice)) ->
  ErrorsOr (SymbolTable (Ranged Slice))
tightenPortSyms st = eaToEO $ stTraverseWithSym f st
  where
    f sym a = eoToEA undefined $ tightenSlice sym a

tightenModule :: S.Module -> ErrorsOr Module
tightenModule mod_ =
  do
    (psyms, grps) <-
      liftA2
        (,)
        (tightenPortSyms (S.modSyms mod_))
        (mapEO tightenGroup (S.modGrps mod_))
    good $ Module (S.modName mod_) psyms grps

run :: [S.Module] -> ErrorsOr [Module]
run = mapEO tightenModule
