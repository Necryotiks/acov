module Acov.Core.Verilog
  ( run,
  )
where

import qualified Acov.Core.DPI as DPI
import Acov.Core.Printer (modName)
import qualified Acov.Core.SV as SV
import qualified Acov.Frontend.Expressions as E
import qualified Acov.Frontend.Parser as P
import Acov.Frontend.Ranged (Ranged, rangedData)
import Acov.Frontend.SymbolTable (stAssocs)
import qualified Acov.Frontend.Width as W
import Control.Exception.Base (assert)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (Handle, IOMode (WriteMode), hPutStr, withFile)

fileHeader :: String
fileHeader =
  unlines
    [ "// AUTO-GENERATED FILE: Do not edit.",
      "",
      "`default_nettype none",
      ""
    ]

fileFooter :: String
fileFooter = "\n`default_nettype wire\n"

showBitSel :: E.Slice -> String
showBitSel (E.Slice a b) =
  if a == 0 && b == 0
    then ""
    else "[" ++ show a ++ ":" ++ show b ++ "]"

showBitSels :: [E.Slice] -> [String]
showBitSels slices = map expand raw
  where
    raw = map showBitSel slices
    width = maximum $ map length raw
    expand s =
      assert (length s <= width) $
        s ++ replicate (width - length s) ' '

showPorts :: [(Ranged P.Symbol, Ranged E.Slice)] -> [String]
showPorts entries = zipWith (curry draw) names sels
  where
    names =
      [P.Symbol "clk", P.Symbol "rst_n"]
        ++ map (rangedData . fst) entries
    sels =
      showBitSels
        ( [slice0, slice0]
            ++ map (rangedData . snd) entries
        )
    slice0 = E.Slice 0 0
    draw (sym, sel) = "input wire " ++ sel ++ " " ++ P.symName sym

-- Print the start and end of a module, wrapping a body printing
-- function inside.
printModule :: Int -> Int -> W.Module -> Handle -> IO ()
printModule hash modIdx mod_ h =
  do
    put fileHeader
    put start
    put (head portStrs)
    mapM_ (\str -> put indent >> put str) (tail portStrs)
    put ");\n\n"
    put "`ifndef ACOV_SV\n"
    DPI.printModule h hash modIdx mod_
    put "`else\n"
    SV.printModule h mod_
    put "`endif\n"
    put "endmodule\n\n"
    put fileFooter
  where
    put = hPutStr h
    name = modName mod_
    start = "module " ++ name ++ "_coverage ("
    indent = ",\n" ++ replicate (length start) ' '
    ports = W.modSyms mod_
    portStrs = showPorts $ stAssocs ports

dumpModule :: FilePath -> Int -> (Int, W.Module) -> IO ()
dumpModule dirname hash (modIdx, mod_) =
  withFile
    (dirname </> (modName mod_ ++ "_coverage.sv"))
    WriteMode
    (printModule hash modIdx mod_)

run :: FilePath -> (Int, [W.Module]) -> IO ()
run dirname (hash, mods) =
  createDirectoryIfMissing False dirname
    >> mapM_ (dumpModule dirname hash) (zip [0 ..] mods)
