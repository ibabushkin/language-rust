{-# LANGUAGE OverloadedStrings #-}
module LexerSpec where

import Test.Hspec
-- import Test.QuickCheck

import Language.Rust.Lexer (alexScanTokens, AlexPn(..))
import Language.Rust.Token

-- | main spec
spec :: Spec
spec = describe "Language.Rust.Lexer"
    tokenSpec

tokenSpec :: Spec
tokenSpec = describe "tokenization" $ do
    it "recognizes whitespace" $
        alexScanTokens " \t\n\t" == [Token (AlexPn 0 1 1) SpaceToken " \t\n\t"]
