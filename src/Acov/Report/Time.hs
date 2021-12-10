{-# LANGUAGE CPP #-}

module Acov.Report.Time
  ( stringTime,
  )
where

import Data.Time.Format
  ( defaultTimeLocale,
    formatTime,
  )
import Data.Time.LocalTime (getZonedTime)

{-
  This module is needed because we want to support the old locales in
  GHC 8.4, but also the new ones for more modern GHCs.
-}

stringTime :: IO String
stringTime =
  formatTime defaultTimeLocale "%Y/%m/%d %H:%M" <$> getZonedTime
