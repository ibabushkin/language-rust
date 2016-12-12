{-# LANGUAGE OverloadedStrings #-}
module LexerSpec where

import Test.Hspec
-- import Test.QuickCheck

import Language.Rust.Lexer (alexScanTokens, AlexPosn(..))
import Language.Rust.Token

-- | main spec
spec :: Spec
spec = describe "Language.Rust.Lexer"
    tokenSpec

tokenSpec :: Spec
tokenSpec = describe "tokenization" $ do
    it "recognizes whitespace" $
        alexScanTokens " \t\n\t" == [Token (AlexPn 0 1 1) SpaceToken " \t\n\t"]
    it "recognizes different integer literals" $
        alexScanTokens "0xab2_c1 0o_22_3553 0b1101_010 1234u32" ==
            [ Token (AlexPn 0 1 1) HexLiteralToken "0xab2_c1"
            , Token (AlexPn 8 1 9) SpaceToken " "
            , Token (AlexPn 9 1 10) OctalLiteralToken "0o_22_3553"
            , Token (AlexPn 19 1 20) SpaceToken " "
            , Token (AlexPn 20 1 21) BinaryLiteralToken "0b1101_010"
            , Token (AlexPn 30 1 31) SpaceToken " "
            , Token (AlexPn 31 1 32) DecimalLiteralToken "1234u32"
            ]
