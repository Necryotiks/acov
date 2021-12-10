module Acov.Frontend.CList
  ( CList,
    clCons,
    clLen,
    clEmpty,
    clArray,
  )
where

import Data.Array (Array, listArray)

data CList a = CList !Int ![a]

clCons :: a -> CList a -> CList a
clCons a (CList n as) = CList (n + 1) (a : as)

clLen :: CList a -> Int
clLen (CList n _) = n

clEmpty :: CList a
clEmpty = CList 0 []

clArray :: CList a -> Array Int a
clArray (CList n as) = listArray (0, n - 1) (reverse as)
