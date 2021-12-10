module Acov.Frontend.ErrorsOr
  ( ErrorsOr,
    bad1,
    good,
    eaToEO,
    eoToEA,
    foldEO,
    mapEO,
    eoMaybe,
    reportEO,
  )
where

import Control.Exception.Base
import qualified Data.Foldable as F
import Acov.Frontend.ErrorsAnd
import Acov.Frontend.Ranged
import System.Exit
import System.IO

newtype ErrorsOr a = ErrorsOr (Either [Ranged String] a)

bad :: [Ranged String] -> ErrorsOr a
bad errs =
  assert
    (not $ null errs)
    ErrorsOr
    $ Left errs

bad1 :: Ranged String -> ErrorsOr a
bad1 rs = ErrorsOr $ Left [rs]

good :: a -> ErrorsOr a
good = ErrorsOr . Right

instance Functor ErrorsOr where
  fmap _ (ErrorsOr (Left errs)) = bad errs
  fmap f (ErrorsOr (Right a)) = good (f a)

instance Applicative ErrorsOr where
  pure a = good a
  ErrorsOr (Left e0) <*> ErrorsOr (Left e1) = bad $ e0 ++ e1
  ErrorsOr (Left e0) <*> ErrorsOr (Right _) = bad e0
  ErrorsOr (Right _) <*> ErrorsOr (Left e1) = bad e1
  ErrorsOr (Right f) <*> ErrorsOr (Right a) = good $ f a

instance Monad ErrorsOr where
  ErrorsOr (Left errs) >>= _ = bad errs
  ErrorsOr (Right a) >>= f = f a
  return = good

updateEO ::
  (b -> a -> ErrorsOr b) ->
  ([Ranged String], b) ->
  a ->
  ([Ranged String], b)
updateEO f (errs, b) a =
  case f b a of
    ErrorsOr (Left errs') -> (errs' ++ errs, b)
    ErrorsOr (Right b') -> (errs, b')

eaToEO :: ErrorsAnd a -> ErrorsOr a
eaToEO (ErrorsAnd errs a) = if null errs then good a else bad errs

eoToEA :: a -> ErrorsOr a -> ErrorsAnd a
eoToEA a0 (ErrorsOr (Left errs)) = ErrorsAnd errs a0
eoToEA _ (ErrorsOr (Right a)) = ErrorsAnd [] a

foldEO :: F.Foldable t => (b -> a -> ErrorsOr b) -> b -> t a -> ErrorsOr b
foldEO f b0 as =
  if null errs then good b1 else bad (reverse errs)
  where
    (errs, b1) = F.foldl' (updateEO f) ([], b0) as

mapEO :: (a -> ErrorsOr b) -> [a] -> ErrorsOr [b]
mapEO f as =
  case foldr take_ (Right []) as of
    Left errs -> ErrorsOr $ Left errs
    Right bs -> good bs
  where
    take_ a ebs =
      case f a of
        ErrorsOr (Left errs') ->
          case ebs of
            Left errs -> Left (errs' ++ errs)
            Right _ -> Left errs'
        ErrorsOr (Right b) ->
          case ebs of
            Left errs -> Left errs
            Right bs -> Right (b : bs)

eoMaybe :: Maybe (ErrorsOr a) -> ErrorsOr (Maybe a)
eoMaybe Nothing = good Nothing
eoMaybe (Just eoa) = Just <$> eoa

showStart :: LCRange -> String
showStart (LCRange (LCPos l c) _) = show l ++ ":" ++ show c

reportError :: FilePath -> Ranged String -> IO ()
reportError path (Ranged rng str) =
  hPutStr stderr (path ++ ":" ++ showStart rng ++ ": " ++ str ++ "\n")

reportEO :: FilePath -> ErrorsOr a -> IO a
reportEO path (ErrorsOr (Left errs)) = mapM_ (reportError path) errs >> exitFailure
reportEO _ (ErrorsOr (Right a)) = return a
