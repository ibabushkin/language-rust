module Language.Rust.Token where

import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With)

-- | Our token type, a chunk of text we pulled from a Rust source, annotated with
-- some basic information on it's role in the input stream. To avoid confusion and
-- annoyance, we parametrize over the type of position information.
data Token pos = Token pos TokenType Text
    deriving (Show, Eq)

-- | Types of tokens we distinguish in the input stream.
data TokenType
    = SpaceToken
    | CommentToken
    | CharLiteralToken
    | ByteLiteralToken
    | StringLiteralToken
    | ByteStringLiteralToken
    | BoolLiteralToken
    | DecimalLiteralToken
    | HexLiteralToken
    | OctalLiteralToken
    | BinaryLiteralToken
    | FloatingLiteralToken
    | IdentifierToken
    deriving (Show, Eq)

-- | Construct a token given a type and a position, seemlessly handling data
-- we can't decode (we simply ignore it right now).
token :: TokenType -> pos -> L.ByteString -> Token pos
token t p = Token p t . decodeUtf8With (\_ _ -> Nothing) . L.toStrict
