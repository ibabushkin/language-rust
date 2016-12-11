{
module Language.Rust.Lexer where
}

-- we want position info and efficient bytestring input
%wrapper "posn-bytestring"

-- comments
@comment = "//" .* | "/*" (. | \n)* "*/"

-- all number literals can contain underscores
$binarydigit = [01_]
$octaldigit = [0-7_]
$decimaldigit = [0-9_]
$hexdigit = [0-9a-f_]

-- identifiers
$ident_start = [a-zA-Z]
$ident_continue = [a-zA-Z_0-9]
@identifier = ($ident_start $ident_continue*) | (_ $ident_continue+)

-- suffixes and prefixes
@sign = "-" | "+"
@integer_suffix = ([ui] (8 | 16 | 32 | 64 | size))?
@floating_point_suffix = (f32 | f64)?

-- character literals (any character or an escaped single quote in single quotes)
-- TODO: raw and byte literals
@character = ' (. # ' | \\\') '

-- string literals (any sequence of characters enclosed in double quotes, possibly
-- including escaped double quotes)
-- TODO: what about multiline literals with (or without) escapes?
-- TODO: raw and byte literals
@string = \" (. # \" | \\\")* \"

-- bool literals
@bool = true | false

-- integer and floating point literals
@decimal_integer = $decimaldigit+
@hex_integer = "0x" $hexdigit+
@octal_integer = "0o" $octaldigit+
@binary_integer = "0b" $binarydigit+
@floating_point = $decimaldigit+ (\. $decimaldigit*)? (E @sign $decimaldigit+)?

tokens :-
    $white+ ;
    "//".* ;
    ($ident_start $ident_continue*) | (_ $ident_continue+) ;
