module Main where

import ParserSpec
import RangeListSpec

main :: IO ()
main = do
  parserSpec
  rangeListSpec
