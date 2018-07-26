module Grouping
  ( run
  , Record(..)
  , BitsRecord(..)
  , Group(..)
  , Module(..)
  ) where

import Data.Functor

import qualified Parser as P

import Ranged
import ErrorsOr

{-
  This pass is in charge of putting when/group blocks into a canonical
  form. A when block that contains multiple groups is split, so the
  end result is a list of groups, each of which may have a guard and
  contains one or more record statements.
-}
data Record = Record
              (Ranged P.Expression)
              (Maybe (Ranged P.Symbol))
              (Maybe [(Ranged Integer, Ranged Integer)])

data BitsRecord = BitsRecord
                  (Ranged P.Expression)
                  (Maybe (Ranged P.Symbol))

{-
  A group either contains a list of records which form an implicit
  cross or it contains a single record as a "BitsRecord". This means
  we want to see each bit in the recorded expression as 1 and 0.
-}
data Group = Group [Ranged P.Expression] (Either [Record] BitsRecord)

data Module = Module (Ranged P.Symbol) [Ranged P.Port] [Group]

tlReadStmt :: Ranged P.Statement -> ErrorsOr [Group]
tlReadStmt rstmt = f (rangedData rstmt)
  where rng = rangedRange rstmt
        erk = bad1 . Ranged rng
        -- CoverBits records turn into special BitsGroups. Other
        -- records make CrossGroups.
        f (P.Record expr as (Just P.CoverBits)) =
          good [Group [] $ Right $ BitsRecord expr as]
        f (P.Record expr as (Just (P.CoverList vals))) =
          good [Group [] $ Left [Record expr as (Just vals)]]
        f (P.Record expr as Nothing) =
          good [Group [] $ Left [Record expr as Nothing]]
        f (P.When guard stmts) = concat <$> mapEO (whenReadStmt [guard]) stmts
        f (P.Group stmts) = do { body <- mapEO groupReadStmt stmts
                               ; good [Group [] $ Left body]
                               }

whenReadStmt :: [Ranged P.Expression] -> Ranged P.Statement -> ErrorsOr [Group]
whenReadStmt guards rstmt = f (rangedData rstmt)
  where rng = rangedRange rstmt
        erk x = bad1 $ Ranged rng x
        f (P.Record expr as (Just P.CoverBits)) =
          good $ [Group guards $ Right $ BitsRecord expr as]
        f (P.Record expr as (Just (P.CoverList vals))) =
          good $ [Group guards $ Left [Record expr as (Just vals)]]
        f (P.Record expr as Nothing) =
          good $ [Group guards $ Left [Record expr as Nothing]]
        f (P.When guard stmts) = concat <$>
                                 mapEO (whenReadStmt (guard : guards)) stmts
        f (P.Group stmts) = do { recs <- mapEO groupReadStmt stmts
                               ; good [Group guards $ Left recs]
                               }

groupReadStmt :: Ranged P.Statement -> ErrorsOr Record
groupReadStmt rstmt = f (rangedData rstmt)
  where rng = rangedRange rstmt
        erk x = bad1 $ Ranged rng x
        f (P.Record expr as (Just P.CoverBits)) =
          erk "record with `cover bits' inside group."
        f (P.Record expr as (Just (P.CoverList vals))) =
          good $ Record expr as (Just vals)
        f (P.Record expr as Nothing) =
          good $ Record expr as Nothing
        f (P.When _ _) = erk "when block nested inside group."
        f (P.Group _) = erk "nested groups."

readModule :: P.Module -> ErrorsOr Module
readModule (P.Module name ports stmts) =
  (Module name ports . concat) <$> mapEO tlReadStmt stmts

run :: [P.Module] -> ErrorsOr [Module]
run = mapEO readModule
