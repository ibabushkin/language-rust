{
{-# LANGUAGE OverloadedStrings #-}
module Language.Rust.Lexer where

import Language.Rust.Token (token, TokenType(..))
}

-- we want position info and efficient bytestring input
%wrapper "posn-bytestring"

-- all number literals can contain underscores
$binarydigit = [01_]
$octaldigit = [0-7_]
$decimaldigit = [0-9_]
$hexdigit = [0-9a-fA-F_]
$ascii = [\0 - \127]

-- comments
@comment = "//" .* | "/*" (. | \n)* "*/"

-- different escapes
@byte_escape = \x [0-9a-fA-F]{2} | \n | \r | \t | \\ | \0
@unicode_escape = "\u{" $hexdigit{1,6} "}"

-- character literals (any character or an escaped single quote in single quotes)
@character = ' (. # ' | \' | @byte_escape | @unicode_escape) '

-- byte literals
-- TODO: are we sure? ;)
@byte = b' (\$octaldigit $hexdigit | \\ | $ascii) '

-- string literals (any sequence of characters enclosed in double quotes, possibly
-- including escaped double quotes)
-- TODO: raw literals
@string = \" (. # \" | \\\" | @byte_escape | @unicode_escape | \n)* \"
@bytestring = b\" ($ascii | @byte_escape) \"

-- identifiers
$ident_start = [a-zA-Z]
$ident_continue = [a-zA-Z_0-9]
@identifier = ($ident_start $ident_continue*) | (_ $ident_continue+)

-- suffixes and prefixes
@integer_suffix = ([ui] (8 | 16 | 32 | 64 | size))?
@floating_point_suffix = (f32 | f64)?

-- bool literals
@bool = true | false

-- integer and floating point literals
@decimal_integer = $decimaldigit+
@hex_integer = "0x" $hexdigit+
@octal_integer = "0o" $octaldigit+
@binary_integer = "0b" $binarydigit+
@floating_point = $decimaldigit+ (\. $decimaldigit*)? (E ("-" | "+") $decimaldigit+)?

-- paths
@type_param = "<" .+ ">"
@path = (::)? (@identifier @type_param? ::)+ (@identifier @type_param? | @type_param)

tokens :-
    $white+ { token SpaceToken }
    @comment { token CommentToken }
    @character { token CharLiteralToken }
    @byte { token ByteLiteralToken }
    @string { token StringLiteralToken }
    @bytestring { token ByteStringLiteralToken }
    @bool { token BoolLiteralToken }
    @decimal_integer @integer_suffix { token DecimalLiteralToken }
    @hex_integer @integer_suffix { token HexLiteralToken }
    @octal_integer @integer_suffix { token OctalLiteralToken }
    @binary_integer @integer_suffix { token BinaryLiteralToken }
    @floating_point @floating_point_suffix { token FloatingLiteralToken }
    @path { token PathToken }
    @identifier { token IdentifierToken }
