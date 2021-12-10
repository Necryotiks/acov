module Acov.Frontend.SymbolTable
  ( Symbol,
    SymbolTable,
    stLookup,
    stAt,
    stNameAt,
    stAssocs,
    stTraverseWithSym,
    STBuilder,
    stbEmpty,
    stbAdd,
    stbToSymbolTable,
  )
where

import Acov.Frontend.CList
  ( CList,
    clArray,
    clCons,
    clEmpty,
    clLen,
  )
import Acov.Frontend.ErrorsOr (ErrorsOr, bad1, good)
import Acov.Frontend.Hashable (Hashable (..))
import qualified Acov.Frontend.Parser as P
import Acov.Frontend.Ranged (LCRange, Ranged (..), rangedData)
import Data.Array.IArray (Array, elems, (!))
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map

newtype Symbol = Symbol Int
  deriving (Eq, Ord)

instance Hashable Symbol where
  hash (Symbol n) = hash n

data SymbolTable a = SymbolTable
  { stMap :: !(Map.Map String Int),
    stData :: !(Array Int (Ranged P.Symbol, a))
  }

instance Hashable a => Hashable (SymbolTable a) where
  hash st =
    hash
      ( Map.toAscList $ stMap st,
        elems $ stData st
      )

instance Functor SymbolTable where
  fmap f st = SymbolTable (stMap st) (f' <$> stData st)
    where
      f' (sym, a) = (sym, f a)

instance Foldable SymbolTable where
  foldMap f st = Foldable.foldMap (f . snd) (stData st)
  foldr f b st = Foldable.foldr f' b (stData st)
    where
      f' (_, a) b' = f a b'

instance Traversable SymbolTable where
  traverse f st = SymbolTable (stMap st) <$> traverse f' (stData st)
    where
      f' (sym, a) = (,) sym <$> f a

stLookup :: P.Symbol -> SymbolTable a -> Maybe Symbol
stLookup sym st = Symbol <$> Map.lookup (P.symName sym) (stMap st)

stAt :: Symbol -> SymbolTable a -> a
stAt (Symbol idx) st = snd $ stData st ! idx

stNameAt :: Symbol -> SymbolTable a -> Ranged P.Symbol
stNameAt (Symbol idx) st = fst $ stData st ! idx

stAssocs :: SymbolTable a -> [(Ranged P.Symbol, a)]
stAssocs = elems . stData

stTraverseWithSym ::
  Applicative t =>
  (Ranged P.Symbol -> a -> t b) ->
  SymbolTable a ->
  t (SymbolTable b)
stTraverseWithSym f st = SymbolTable (stMap st) <$> traverse f' (stData st)
  where
    f' (sym, a) = (,) sym <$> f sym a

data STBuilder a = STBuilder
  { stbMap :: !(Map.Map String Int),
    stbList :: !(CList (Ranged P.Symbol, a))
  }

stbEmpty :: STBuilder a
stbEmpty = STBuilder Map.empty clEmpty

stbAdd ::
  String ->
  STBuilder a ->
  LCRange ->
  Ranged P.Symbol ->
  a ->
  ErrorsOr (STBuilder a)
stbAdd what stb rng rsym a =
  if Map.member name (stbMap stb)
    then bad1 (Ranged rng $ "duplicate " ++ what ++ ": `" ++ name ++ "'.")
    else good $ STBuilder (Map.insert name len (stbMap stb)) (clCons (rsym, a) list)
  where
    name = P.symName (rangedData rsym)
    list = stbList stb
    len = clLen list

stbToSymbolTable :: STBuilder a -> SymbolTable a
stbToSymbolTable stb = SymbolTable (stbMap stb) (clArray (stbList stb))
