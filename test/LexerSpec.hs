{-# LANGUAGE OverloadedStrings #-}
module LexerSpec where

import Data.ByteString.Lazy (fromStrict)
import Data.Text.Encoding (encodeUtf8)

import Language.Rust.Lexer (alexScanTokens, AlexPosn(..))
import Language.Rust.Token

import Test.Hspec
-- import Test.QuickCheck

-- | main spec
spec :: Spec
spec = describe "Language.Rust.Lexer" $ do
    commentSpec
    numberLiteralSpec

commentSpec :: Spec
commentSpec = describe "comment tokenization" $ do
    it "recognizes comment precedence" $
        alexScanTokens "asdf // asdf = 0.0" ==
            [ Token (AlexPn 0 1 1) IdentifierToken "asdf"
            , Token (AlexPn 4 1 5) SpaceToken " "
            , Token (AlexPn 5 1 6) CommentToken "// asdf = 0.0"
            ]
    it "allows inline comments" $
        alexScanTokens "asdf /* some comment */ 1" ==
            [ Token (AlexPn 0 1 1) IdentifierToken "asdf"
            , Token (AlexPn 4 1 5) SpaceToken " "
            , Token (AlexPn 5 1 6) CommentToken "/* some comment */"
            , Token (AlexPn 23 1 24) SpaceToken " "
            , Token (AlexPn 24 1 25) DecimalLiteralToken "1"
            ]
    it "allows multiline comments" $
        alexScanTokens (fromStrict $ encodeUtf8 "bla /* abc\ndef */ b") ==
            [ Token (AlexPn 0 1 1) IdentifierToken "bla"
            , Token (AlexPn 3 1 4) SpaceToken " "
            , Token (AlexPn 4 1 5) CommentToken "/* abc\ndef */"
            , Token (AlexPn 17 2 7) SpaceToken " "
            , Token (AlexPn 18 2 8) IdentifierToken "b"
            ]

numberLiteralSpec :: Spec
numberLiteralSpec = describe "number tokenization" $ do
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
    it "recognizes floating pointer literals" $
        alexScanTokens "1.234 1.34E+3 1.78E-2 1. 1f64" ==
            [ Token (AlexPn 0 1 1) FloatingLiteralToken "1.234"
            , Token (AlexPn 5 1 6) SpaceToken " "
            , Token (AlexPn 6 1 7) FloatingLiteralToken "1.34E+3"
            , Token (AlexPn 13 1 14) SpaceToken " "
            , Token (AlexPn 14 1 15) FloatingLiteralToken "1.78E-2"
            , Token (AlexPn 21 1 22) SpaceToken " "
            , Token (AlexPn 22 1 23) FloatingLiteralToken "1."
            , Token (AlexPn 24 1 25) SpaceToken " "
            , Token (AlexPn 25 1 26) FloatingLiteralToken "1f64"
            ]
