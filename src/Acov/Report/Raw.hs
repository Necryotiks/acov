{-# LANGUAGE StrictData #-}

module Acov.Report.Raw
  ( Coverage,
    covCount,
    emptyCoverage,
    getModData,
    updateCoverage,
    ModData,
    traverseMD,
    ScopeData,
    sdMaxKey,
    sdGetGroup,
  )
where

import qualified Control.Exception as CE
import Control.Exception.Base (assert)
import Control.Monad (foldM, when)
import Data.Char (digitToInt)
import Data.Foldable (Foldable (foldl'))
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as Set
import Numeric (showHex)
import Text.Parsec
  ( SourceName,
    eof,
    errorPos,
    hexDigit,
    many1,
    option,
    parse,
    sepBy,
    (<?>),
    (<|>),
  )
import Text.Parsec.Error (errorMessages, showErrorMessages)
import qualified Text.Parsec.Language as L
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as T

type IntegerSet = Set.Set Integer

{-
  Here are the fundamental types for the coverage data.

  ScopeData is the coverage data for a single scope. It is a map from
  group index to a set of integer values seen.
-}
newtype ScopeData = ScopeData (Map.Map Int IntegerSet)

sdMaxKey :: ScopeData -> Int
sdMaxKey (ScopeData scdmap) = if Map.null scdmap then (-1) else fst $ Map.findMax scdmap

sdGetGroup :: ScopeData -> Int -> IntegerSet
sdGetGroup (ScopeData scdmap) n = Map.findWithDefault Set.empty n scdmap

{-
  ModData is the coverage data for a module (possibly with multiple
  instantiations). It is a map from scope name to associated data.
-}
newtype ModData = ModData (Map.Map String ScopeData)

emptyMD :: ModData
emptyMD = ModData Map.empty

traverseMD ::
  Applicative t =>
  (String -> ScopeData -> t a) ->
  ModData ->
  t (Map.Map String a)
traverseMD f (ModData mdmap) = Map.traverseWithKey f mdmap

{-
  Coverage is the whole shebang. It is a map from module name to
  associated data, together with a counter that counts how many tests'
  data it contains.
-}
data Coverage = Coverage
  { covCount :: !Int,
    covMap :: !(Map.Map Integer ModData)
  }

emptyCoverage :: Coverage
emptyCoverage = Coverage 0 Map.empty

getModData :: Integer -> Coverage -> ModData
getModData modname cov = Map.findWithDefault emptyMD modname (covMap cov)

{-
  Let's define a parser for coverage logs.

  The first parsing pass just generates statements, which either set
  the module, set the scope or record a group.

  A record is the group number, then a list of values (parsed into
  integers)
-}
data Statement
  = Module !Integer
  | Scope !String
  | Record !Int ![Integer]

language :: L.LanguageDef ()
language = L.emptyDef {T.commentLine = "#"}

lexer :: T.TokenParser ()
lexer = T.makeTokenParser language

hex :: Parser Integer
hex = do
  n <- foldl' acc 0 <$> T.lexeme lexer (many1 hexDigit)
  seq n $ return n
  where
    acc x c = 16 * x + toInteger (digitToInt c)

signedHex :: Parser Integer
signedHex = do
  neg <- option False (T.reservedOp lexer "-" >> return True)
  val <- hex
  return $ if neg then - val else val

parseGrp :: Parser Int
parseGrp = do
  integer <- signedHex
  when
    ( integer < toInteger (minBound :: Int)
        || integer > toInteger (maxBound :: Int)
    )
    $ fail "Group index overflows an Int."
  return $ fromInteger integer

vals :: Parser [Integer]
vals = T.braces lexer $ sepBy hex (T.reservedOp lexer ",")

sym :: Parser String
sym = T.identifier lexer

colon :: Parser ()
colon = T.reservedOp lexer ":"

dot :: Parser ()
dot = T.reservedOp lexer "."

parseMod :: Parser Statement
parseMod = T.reserved lexer "MODULE" >> colon >> (Module <$> hex)

parseScope :: Parser Statement
parseScope =
  T.reserved lexer "SCOPE" >> colon
    >> (Scope . intercalate "." <$> sepBy sym dot)

parseRecord :: Parser Statement
parseRecord = do
  g <- parseGrp
  colon
  Record g <$> vals

statement :: Parser Statement
statement = (parseMod <|> parseScope <|> parseRecord) <?> "statement"

script :: Parser (Integer, [Statement])
script = do
  T.whiteSpace lexer
  hash <- hex
  stmts <- many1 statement
  eof
  return (hash, stmts)

parseScript :: SourceName -> String -> Either String (Integer, [Statement])
parseScript name contents =
  case parse script name contents of
    Left err ->
      Left $
        show (errorPos err)
          ++ showErrorMessages
            "or"
            "unknown parse error"
            "expecting"
            "unexpected"
            "end of input"
            (errorMessages err)
    Right res -> Right res

{-
  Now we know how to parse a script into a list of statements, we can
  use them to update an existing coverage map.

  This might go wrong, because at the start of the list of statements,
  we have no ambient module and at the start of each module, we have
  no ambient scope. We're not putting much work into error reporting
  though - this should only fail if cover.cc generates rubbish for
  some reason.

  To track this, we have local state that we fold with an Either
  monad.
-}
type ScrState = (Maybe Integer, Maybe String)

takeStatement ::
  (ScrState, Coverage) ->
  Statement ->
  Either String (ScrState, Coverage)
takeStatement ((_, _), cov) (Module mod_) = Right ((Just mod_, Nothing), cov)
takeStatement ((Nothing, _), _) (Scope scp) = Left ("Cannot set scope to " ++ scp ++ " without ambient module.")
takeStatement ((Just smod, _), cov) (Scope scp) = Right ((Just smod, Just scp), cov)
takeStatement ((_, Nothing), _) (Record _ _) = Left "Cannot record group without ambient scope."
takeStatement ((smod, Just ssc), cov) (Record grp values) = Right $ seq cov' ((smod, Just ssc), cov')
  where
    cov' = updCoverage (fromJust smod) ssc grp values cov

updCoverage :: Integer -> String -> Int -> [Integer] -> Coverage -> Coverage
updCoverage mod_ scope grp values cov =
  -- Build a new Coverage object with the updated map. Note that we
  -- have to ensure strict evaluation of count as well as map' to
  -- avoid leaking a reference to the old coverage object.
  seq cvcount $ seq map' $ Coverage cvcount map'
  where
    cvcount = covCount cov
    map' = Map.alter (Just . updMD scope grp values) mod_ (covMap cov)

updMD :: String -> Int -> [Integer] -> Maybe ModData -> ModData
updMD scope grp values Nothing = newMD scope grp values
updMD scope grp values (Just (ModData mdmap)) =
  ModData $ Map.alter (Just . updSD grp values) scope mdmap

newMD :: String -> Int -> [Integer] -> ModData
newMD scope grp values = ModData $ Map.singleton scope $ newSD grp values

updSD :: Int -> [Integer] -> Maybe ScopeData -> ScopeData
updSD grp values Nothing = newSD grp values
updSD grp values (Just (ScopeData scdmap)) =
  ScopeData $ Map.alter (Just . updGrp values) grp scdmap

newSD :: Int -> [Integer] -> ScopeData
newSD grp values = ScopeData $ Map.singleton grp $ Set.fromList values

updGrp :: [Integer] -> Maybe IntegerSet -> IntegerSet
updGrp values Nothing = Set.fromList values
updGrp values (Just values') = Set.fromList values <> values'

takeScript ::
  Coverage ->
  Maybe Int ->
  (Integer, [Statement]) ->
  Either String Coverage
takeScript cov hash (hash', stmts) =
  do
    case hash of
      Nothing -> Right ()
      Just h ->
        if toInteger h /= hash'
          then
            Left
              ( "Hash code in file is 0x" ++ showHex hash' ""
                  ++ ", which doesn't match the expected 0x"
                  ++ showHex h ""
                  ++ "."
              )
          else Right ()
    cov' <- snd <$> foldM takeStatement ((Nothing, Nothing), cov) stmts
    return $ cov' {covCount = covCount cov + 1}

takeContents :: Coverage -> Maybe Int -> FilePath -> String -> (Coverage, Maybe String)
takeContents cov hash path contents =
  case parseScript path contents >>= takeScript cov hash of
    Left err -> (cov, Just $ show path ++ ": Ignoring file. " ++ err)
    Right cov' -> (cov', Nothing)

updateCoverage :: Coverage -> Maybe Int -> FilePath -> IO (Coverage, Maybe String)
updateCoverage cov hash path =
  do
    res <- (CE.try $ readFile path) :: IO (Either CE.IOException String)
    return $ case res of
      Left err -> (cov, Just $ show err)
      Right str -> takeContents cov hash path str
