module ParserSpec (parserSpec) where

import Data.Either
import Parser (Symbol (..), sym, symName)
import Test.Hspec
import Test.QuickCheck
import Text.Parsec

-- Check symbol parsing
parseSym :: String -> Either ParseError Symbol
parseSym = parse sym "fname"

prop_Alpha_Parse :: Property
prop_Alpha_Parse = forAll chrs $ \x -> parseSym x == Right (Symbol x)
  where
    chrs = listOf1 $ elements $ ['A' .. 'Z'] <> ['a' .. 'z']

prop_Name_Extract :: String -> Bool
prop_Name_Extract x = symName (Symbol x) == x

parserSpec :: IO ()
parserSpec = hspec $ do
  describe "parseSym" $ do
    it "fails on empty strings" $
      isLeft (parseSym "") `shouldBe` True
    it "fails on integer lead symbols" $
      isLeft (parseSym "3") `shouldBe` True
    it "parses integer containing symbols" $
      parseSym "_x123" `shouldBe` Right (Symbol "_x123")
    it "parses alphabetical characters" $ property prop_Alpha_Parse
  describe "symName" $ do
    it "extracts names from Symbols" $ property prop_Name_Extract
