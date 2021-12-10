module Acov.Core.DPI
  ( printModule,
  )
where

import Acov.Core.Printer (endAlways, showExpression, startAlways)
import qualified Acov.Frontend.Expressions as E
import Acov.Frontend.Ranged (Ranged, rangedData)
import Acov.Frontend.SymbolTable (SymbolTable)
import qualified Acov.Frontend.Width as W
import Control.Exception.Base (assert)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import Numeric (showHex)
import qualified Slist
import qualified Slist as S
import System.IO (Handle, hPutStr)

printModule :: Handle -> Int -> Int -> W.Module -> IO ()
printModule h hash modIdx mod_ =
  do
    hPutStr h (imports hash modIdx)
    grps <- mapM (writeWire h syms) (zip [0 ..] (W.modGroups mod_))
    startAlways h
    mapM_ (writeGroup h syms) (zip [0 ..] grps)
    endAlways h
  where
    syms = W.modSyms mod_

imports :: Int -> Int -> String
imports hash modIdx =
  unlines
    [ "  import \"DPI-C\" context acov_record1 =",
      "    function void acov_record1 (input longint ctxt,",
      "                                input byte cover_bits,",
      "                                input longint grp,",
      "                                input longint val);",
      "  import \"DPI-C\" context acov_record2 =",
      "    function void acov_record2 (input longint ctxt,",
      "                                input byte cover_bits,",
      "                                input longint grp,",
      "                                input longint val1,",
      "                                input longint val0);",
      "  import \"DPI-C\" context acov_record3 =",
      "    function void acov_record3 (input longint ctxt,",
      "                                input byte cover_bits,",
      "                                input longint grp,",
      "                                input longint val2,",
      "                                input longint val1,",
      "                                input longint val0);",
      "  import \"DPI-C\" context acov_record4 =",
      "    function void acov_record4 (input longint ctxt,",
      "                                input byte cover_bits,",
      "                                input longint grp,",
      "                                input longint val3,",
      "                                input longint val2,",
      "                                input longint val1,",
      "                                input longint val0);",
      "",
      "  import \"DPI-C\" context function ",
      "    longint acov_open (input longint hash, input longint modidx);",
      "  import \"DPI-C\" context function void acov_close ();",
      "",
      "  longint ctxt;",
      "  initial begin",
      "    ctxt = acov_open (64'h" ++ showHex hash "" ++ ",",
      "                      64'h" ++ showHex modIdx "" ++ ");",
      "  end",
      "  final acov_close ();",
      ""
    ]

--TODO: Validate assert with Liquid Haskell
{-@ writeWire :: Handle -> SymbolTable (Ranged E.Slice) -> { (a:Int, b:W.Group) | W.grpExprs b > 0 } -> IO ([Ranged E.Expression], Bool, Int) @-}
writeWire ::
  Handle ->
  SymbolTable (Ranged E.Slice) ->
  (Int, W.Group) ->
  IO ([Ranged E.Expression], Bool, Int)
writeWire h syms (idx, grp) =
  assert (width > 0) $
    do
      put "  wire ["
      put (show $ width - 1)
      put ":0] "
      put name
      put ";\n  assign "
      put name
      put " = "
      put (showExpression syms (E.ExprConcat (head exprs) (tail exprs)))
      put ";\n\n"
      return (W.grpGuards grp, covBits, width)
  where
    put = hPutStr h
    name = "acov_recgroup_" ++ show idx
    width = W.grpWidth grp
    exprs = W.grpExprs grp
    covBits = W.grpIsCovBits grp

boolByte :: Bool -> String
boolByte False = "8'b0"
boolByte True = "8'b1"

{-@ writeGroup :: Handle -> SymbolTable (Ranged E.Slice) -> {(a:Int, (b:[Ranged E.Expression], c:Bool, d:Int)) | quot (d + 63) 64 > 0 && quot (d + 63) 64 <= 4} -> IO () @-}
writeGroup ::
  Handle ->
  SymbolTable (Ranged E.Slice) ->
  (Int, ([Ranged E.Expression], Bool, Int)) ->
  IO ()
writeGroup h syms (idx, (guards, isCovBits, width)) =
  do
    guarded <- startGuard h syms guards
    put $ (if guarded then "  " else "") ++ "      acov_record"
    put $ show nwords
    put (" (ctxt, " ++ boolByte isCovBits ++ ", " ++ show idx ++ ", ")
    put $ showRecArgs idx width
    put ");\n"
    endGuard h guarded
  where
    put = hPutStr h
    nwords = quot (width + 63) 64

-- Take a slice of a verilog symbol, starting at bit LO, with width W.
-- We should have W <= 64. If W < 64, pad it out to 64 bits
-- explicitly.
--
--    λ> showFieldSlice "foo" 0 20
--    "{44'b0, foo[19:0]}"
--
showFieldSlice :: String -> Int -> Int -> String
showFieldSlice name lo w = (if w < 64 then pad (64 - w) else id) slice
  where
    pad k str = '{' : show k ++ "'b0, " ++ str ++ "}"
    slice = name ++ "[" ++ show hi ++ ":" ++ show lo ++ "]"
    hi = lo + w - 1

-- Show all the pieces of the field, as the arguments to pass to the
-- acov_recgroup_N function.
--
--   λ> showFieldArgs "foo" 200
--   "{56'b0, foo[199:192]}, foo[191:128], foo[127:64], foo[63:0]"
--
showFieldArgs :: String -> Int -> String
showFieldArgs name w =
  intercalate ", " $
    map slice (reverse [0 .. quot (w + 63) 64 -1])
  where
    slice idx = showFieldSlice name (lo idx) (min (w - lo idx) 64)
    lo idx = 64 * idx

showRecArgs :: Int -> Int -> String
showRecArgs idx width = assert (width > 0) $ showFieldArgs name width
  where
    name = "acov_recgroup_" ++ show idx

startGuard ::
  Handle ->
  SymbolTable (Ranged E.Slice) ->
  [Ranged E.Expression] ->
  IO Bool
startGuard _ _ [] = return False
startGuard h syms guards =
  do
    put "      if (("
    put (intercalate ") && (" (map (showExpression syms . rangedData) guards))
    put ")) begin\n"
    return True
  where
    put = hPutStr h

endGuard :: Handle -> Bool -> IO ()
endGuard _ False = return ()
endGuard h True = hPutStr h "      end\n"
