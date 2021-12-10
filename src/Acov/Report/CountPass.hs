module Acov.Report.CountPass
  ( Coverage (..),
    ModCoverage (..),
    ScopeCoverage (..),
    GroupCoverage (..),
    BitsCov (..),
    covCount,
    run,
  )
where

import Acov.Frontend.Count
  ( Count,
    Counts,
    countMissed,
    incCounts,
    incCounts1,
    mkCount,
    zeroCounts,
  )
import Acov.Frontend.Cross (crossCount, crossMisses, mkCross)
import qualified Acov.Frontend.Width as W
import qualified Acov.Report.Merge as M
import Control.Exception.Base (assert)
import qualified Data.IntSet as IS

{-
  When we have a set of integers but are doing a "cover bits" group,
  we need to iterate through, looking for which bits have been seen
  set and cleared.
-}
cbCount :: Int -> IS.IntSet -> IS.IntSet -> Count
cbCount w ones zeros = mkCount hits total
  where
    hits = IS.size ones + IS.size zeros
    total = toInteger (2 * w)

-- BitsCov w hits partial misses
--
--   where w is the width of the record, hits is the set of valid bits
--   that hit, partial is true if misses is not the whole lot and
--   misses is the first bit indices that miss.
data BitsCov = BitsCov !Int !IS.IntSet !Bool ![Int]

mkBC :: Int -> IS.IntSet -> BitsCov
mkBC w hits = seq misses $ BitsCov w hits partial misses
  where
    --TODO: Check assertion via Liquid Haskell
    partial =
      assert (IS.size hits <= w) $
        (w - IS.size hits) > 10
    misses = take 10 $ filter miss [0 .. (w - 1)]
    miss i = not $ IS.member i hits

data GroupCoverage
  = RecCov !Count ![W.Record] ![[Integer]]
  | -- BRecCov zeros ones
    BRecCov !Count !BitsCov !BitsCov
  | BadScope

covCount :: GroupCoverage -> Maybe Count
covCount (RecCov c _ _) = Just c
covCount (BRecCov c _ _) = Just c
covCount BadScope = Nothing

{-
  This runs through the crossed values, counting up how many of them
  we've managed to hit. We also collect the first 10 values that we
  missed.
-}
countGroup' :: M.GroupCoverage -> GroupCoverage
countGroup' (M.Recs (M.RecsCoverage recs vals)) = RecCov cnt recs misses
  where
    cnt = crossCount vals cross
    cross = mkCross $ map (\r -> (W.recWidth r, W.recClist r)) recs
    nToTake = fromInteger (min (countMissed cnt) 10)
    misses = crossMisses nToTake vals cross

-- (M.BRecCoverage brec (ones, zeros))

countGroup' (M.BRec brc) =
  BRecCov cnt bc0 bc1
  where
    cnt = cbCount w (M.brcOnes brc) (M.brcZeros brc)
    w = W.brWidth $ M.brcBitsRecord brc
    bc1 = mkBC w $ M.brcOnes brc
    bc0 = mkBC w $ M.brcZeros brc
countGroup' M.BadScope = BadScope

countGroup :: (String, M.GroupCoverage) -> (String, GroupCoverage)
countGroup (grpName, gc) = (grpName, countGroup' gc)

data ScopeCoverage = ScopeCoverage
  { scName :: !String,
    scCounts :: !Counts,
    scGroups :: ![(String, GroupCoverage)]
  }

countScope :: M.ScopeCoverage -> ScopeCoverage
countScope (M.ScopeCoverage name gcs) = ScopeCoverage name scnts gcs'
  where
    gcs' = map countGroup gcs
    scnts = foldr (incCounts1 . (covCount . snd)) zeroCounts gcs'

data ModCoverage = ModCoverage
  { mcName :: !String,
    mcCounts :: !Counts,
    mcScopes :: ![ScopeCoverage]
  }

countMod :: M.ModCoverage -> ModCoverage
countMod (M.ModCoverage name scs) = ModCoverage name mcnts scs'
  where
    scs' = map countScope scs
    mcnts = foldr (incCounts . scCounts) zeroCounts scs'

data Coverage = Coverage
  { covTests :: !Int,
    covCounts :: !Counts,
    covMods :: ![ModCoverage]
  }

run :: M.Coverage -> Coverage
run cov = Coverage (M.covTests cov) cnts mcs
  where
    mcs = map countMod (M.covMods cov)
    cnts = foldr (incCounts . mcCounts) zeroCounts mcs
