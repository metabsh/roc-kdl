module [
    is_ident_start,
    read_identifier,
    read_multi_line_string,
    read_quoted_string,
    read_raw_string,
]

import Kdl.Stream exposing [first_byte, full_stop, hex_value, hyphen_minus, is_ascii_digit, is_between, is_hex_digit, is_newline, is_whitespace, left_brace, number_sign, plus_sign, quotation_mark, reverse_solidus, right_brace, skip_one]

###############################################################################
# Identifier String
###############################################################################
# Read a KDL identifier string from the input.
# The input must be positioned at the first identifier character
# (whitespace already skipped by the caller/Lexer).
read_identifier : Str -> Result { string_value : Str, next : Str } [InvalidIdentifier, ReservedKeyword, EndOfBuffer]
read_identifier = |input|
    when first_byte input is
        Err _ -> Err EndOfBuffer
        Ok first ->
            when validate_first_char first is
                Err _ -> Err InvalidIdentifier
                Ok _ ->
                    after_first = skip_one input
                    when validate_lookahead first after_first is
                        Err _ -> Err InvalidIdentifier
                        Ok _ -> finish_identifier input ""

finish_identifier : Str, Str -> Result { string_value : Str, next : Str } [InvalidIdentifier, ReservedKeyword, EndOfBuffer]
finish_identifier = |input, acc|
    when first_byte input is
        Err _ ->
            if Str.is_empty acc then Err EndOfBuffer
            else if is_reserved_keyword acc then Err ReservedKeyword
            else Ok { string_value: acc, next: input }
        Ok byte ->
            if is_non_ident_char byte then
                if Str.is_empty acc then
                    Err InvalidIdentifier
                else if is_reserved_keyword acc then
                    Err ReservedKeyword
                else Ok { string_value: acc, next: input }
            else
                char_str = Str.from_utf8 [byte] |> result_or_empty
                finish_identifier (skip_one input) (Str.concat acc char_str)

result_or_empty : Result Str [BadUtf8 _] -> Str
result_or_empty = |r|
    when r is
        Ok s -> s
        Err _ -> ""

# Validate that the first byte of an identifier is allowed by KDL 2.0 rules.
# Digits and non-identifier chars are rejected.
# +, -, and . require lookahead: if followed by a digit, the identifier is invalid
# (to avoid ambiguity with numbers like +1, -1, .5).
validate_first_char : U8 -> Result {} [InvalidIdentifier]
validate_first_char = |byte|
    if is_ascii_digit byte or is_non_ident_char byte then
        Err InvalidIdentifier
    else
        Ok {}

# Validate lookahead rules: +, -, . followed by a digit is an invalid identifier.
# Call this with the first byte and the remaining input after it.
validate_lookahead : U8, Str -> Result {} [InvalidIdentifier]
validate_lookahead = |first, rest|
    if first == plus_sign or first == hyphen_minus then
        # '+' or '-': second char must not be a digit.
        # If second is '.', third must not be a digit.
        when first_byte rest is
            Err _ -> Ok {}
            Ok second ->
                if is_ascii_digit second then
                    Err InvalidIdentifier
                else if second == full_stop then
                    after_dot = skip_one rest
                    when first_byte after_dot is
                        Err _ -> Ok {}
                        Ok third ->
                            if is_ascii_digit third then
                                Err InvalidIdentifier
                            else Ok {}
                else
                    Ok {}
    else if first == full_stop then
        # '.': second char must not be a digit
        when first_byte rest is
            Err _ -> Ok {}
            Ok second ->
                if is_ascii_digit second then
                    Err InvalidIdentifier
                else Ok {}
    else
        Ok {}

# Check if an identifier string is a reserved keyword in KDL.
is_reserved_keyword : Str -> Bool
is_reserved_keyword = |id|
    List.contains ["true", "false", "null", "inf", "-inf", "nan"] id

# Check if a byte is a non-identifier character (stops identifier parsing).
# Spec: (){}[]/\"#;= plus whitespace and newline
is_non_ident_char : U8 -> Bool
is_non_ident_char = |byte|
    when byte is
        40 | 41 | 123 | 125 | 91 | 93 | 47 | 92 | 34 | 35 | 59 | 61 -> Bool.true
        _ -> is_whitespace(byte) or is_newline(byte)

# Check if a byte is a valid first character for an identifier
is_ident_start : U8 -> Bool
is_ident_start = |byte|
    !(is_ascii_digit(byte) or is_non_ident_char(byte))

###############################################################################
# Quoted String
###############################################################################
# Read a KDL quoted string from the input.
# The input must be positioned at the opening '"' (U+0022).
read_quoted_string : Str -> Result { string_value : Str, next : Str } [UnterminatedString, InvalidEscape, InvalidUtf8, DisallowedCodePoint]
read_quoted_string = |input|
    when first_byte input is
        Err _ -> Err UnterminatedString
        Ok byte ->
            if byte != quotation_mark then
                Err UnterminatedString
            else
                read_quoted_body (skip_one input) ""

# Recursive helper that accumulates decoded string content
read_quoted_body : Str, Str -> Result { string_value : Str, next : Str } [UnterminatedString, InvalidEscape, InvalidUtf8, DisallowedCodePoint]
read_quoted_body = |input, acc|
    when first_byte input is
        Err _ -> Err UnterminatedString
        Ok byte ->
            if byte == quotation_mark then
                # closing '"'
                Ok { string_value: acc, next: skip_one input }
            else if byte == reverse_solidus then
                # '\' - escape sequence
                when read_escape (skip_one input) is
                    Err err -> Err err
                    Ok { escaped_str, next } ->
                        read_quoted_body next (Str.concat acc escaped_str)
            else if is_between(byte, 0, 8) or is_between(byte, 14, 31) or byte == 127 then
                # Disallowed literal code points
                Err DisallowedCodePoint
            else
                char_str = Str.from_utf8 [byte] |> result_or_empty
                read_quoted_body (skip_one input) (Str.concat acc char_str)

# Read an escape sequence after the initial '\'
read_escape : Str -> Result { escaped_str : Str, next : Str } [UnterminatedString, InvalidEscape]
read_escape = |input|
    when first_byte input is
        Err _ -> Err UnterminatedString
        Ok byte ->
            when byte is
                110 -> Ok { escaped_str: "\n", next: skip_one input }      # 'n' → LF
                114 -> Ok { escaped_str: "\r", next: skip_one input }      # 'r' → CR
                116 -> Ok { escaped_str: "\t", next: skip_one input }      # 't' → Tab
                92  -> Ok { escaped_str: "\\", next: skip_one input }      # '\\' → backslash
                34  -> Ok { escaped_str: "\"", next: skip_one input }      # '\"' → double quote
                98  -> Ok { escaped_str: from_byte 8, next: skip_one input }     # 'b' → backspace
                102 -> Ok { escaped_str: from_byte 12, next: skip_one input }    # 'f' → form feed
                115 -> Ok { escaped_str: " ", next: skip_one input }              # 's' → space
                117 -> read_unicode_escape (skip_one input)                       # 'u{' → Unicode
                _ ->
                    if is_whitespace(byte) or is_newline(byte) then
                        skip_escaped_whitespace input
                    else
                        Err InvalidEscape

from_byte : U8 -> Str
from_byte = |byte|
    Str.from_utf8 [byte] |> result_or_empty

# Read a \u{XXXXXX} Unicode escape sequence
read_unicode_escape : Str -> Result { escaped_str : Str, next : Str } [UnterminatedString, InvalidEscape]
read_unicode_escape = |input|
    when first_byte input is
        Err _ -> Err UnterminatedString
        Ok byte ->
            if byte != left_brace then
                Err InvalidEscape
            else
                read_hex_digits (skip_one input) []

# Accumulate hex digits until closing '}'
read_hex_digits : Str, List U8 -> Result { escaped_str : Str, next : Str } [UnterminatedString, InvalidEscape]
read_hex_digits = |input, acc|
    when first_byte input is
        Err _ -> Err UnterminatedString
        Ok byte ->
            if byte == right_brace then
                # '}'
                when parse_hex_scalar acc is
                    Err _ -> Err InvalidEscape
                    Ok scalar ->
                        encoded = encode_utf8 scalar
                        when Str.from_utf8 encoded is
                            Err _ -> Err InvalidEscape
                            Ok s -> Ok { escaped_str: s, next: skip_one input }
            else if is_hex_digit(byte) then
                read_hex_digits (skip_one input) (List.append acc byte)
            else
                Err InvalidEscape

# Parse hex digit bytes into a Unicode scalar value
parse_hex_scalar : List U8 -> Result U32 [InvalidEscape]
parse_hex_scalar = |digits|
    when fold_hex digits 0 is
        Err _ -> Err InvalidEscape
        Ok scalar ->
            if scalar > 0x10FFFF then Err InvalidEscape
            else if scalar >= 0xD800 and scalar <= 0xDFFF then Err InvalidEscape
            else Ok scalar

fold_hex : List U8, U32 -> Result U32 [InvalidEscape]
fold_hex = |digits, acc|
    when digits is
        [] -> Ok acc
        [d, .. as rest] ->
            if is_hex_digit(d) then
                fold_hex rest (acc * 16 + hex_digit_to_u32 d)
            else
                Err InvalidEscape

hex_digit_to_u32 : U8 -> U32
hex_digit_to_u32 = |byte| hex_value byte |> Num.to_u32

# Encode a Unicode scalar value as UTF-8 bytes
encode_utf8 : U32 -> List U8
encode_utf8 = |scalar|
    if scalar <= 127 then
        [Num.to_u8 scalar]
    else if scalar <= 2047 then
        [
            Num.to_u8 (192 + (scalar // 64)),
            Num.to_u8 (128 + (scalar % 64)),
        ]
    else if scalar <= 65535 then
        [
            Num.to_u8 (224 + (scalar // 4096)),
            Num.to_u8 (128 + ((scalar // 64) % 64)),
            Num.to_u8 (128 + (scalar % 64)),
        ]
    else
        [
            Num.to_u8 (240 + (scalar // 262144)),
            Num.to_u8 (128 + ((scalar // 4096) % 64)),
            Num.to_u8 (128 + ((scalar // 64) % 64)),
            Num.to_u8 (128 + (scalar % 64)),
        ]

# Skip whitespace after a whitespace escape '\'
skip_escaped_whitespace : Str -> Result { escaped_str : Str, next : Str } [UnterminatedString, InvalidEscape]
skip_escaped_whitespace = |input|
    when first_byte input is
        Err _ -> Err UnterminatedString
        Ok byte ->
            if is_whitespace(byte) or is_newline(byte) then
                skip_escaped_whitespace (skip_one input)
            else
                Ok { escaped_str: "", next: input }

###############################################################################
# Raw String
###############################################################################
# Read a KDL raw string. Input must be positioned at the first '#' character.
read_raw_string : Str -> Result { string_value : Str, next : Str } [UnterminatedString, InvalidUtf8, DisallowedCodePoint]
read_raw_string = |input|
    when count_hashes input 0 is
        Err _ -> Err UnterminatedString
        Ok { hash_count, next: after_hashes } ->
            when first_byte after_hashes is
                Err _ -> Err UnterminatedString
                Ok byte ->
                    if byte == quotation_mark then
                        when read_raw_opening after_hashes hash_count is
                            Err err -> Err err
                            Ok { body_start, is_multiline } ->
                                if is_multiline then
                                    read_raw_multi_line_body body_start hash_count
                                else
                                    read_raw_single_line_body body_start hash_count
                    else
                        Err UnterminatedString

count_hashes : Str, U64 -> Result { hash_count : U64, next : Str } [EndOfStream]
count_hashes = |input, count|
    when first_byte input is
        Err _ -> Ok { hash_count: count, next: input }
        Ok byte ->
            if byte == number_sign then
                count_hashes (skip_one input) (count + 1)
            else
                Ok { hash_count: count, next: input }

read_raw_opening : Str, U64 -> Result { body_start : Str, is_multiline : Bool } [UnterminatedString]
read_raw_opening = |input, _hash_count|
    when first_byte input is
        Err _ -> Err UnterminatedString
        Ok b1 ->
            if b1 != 34 then Err UnterminatedString
            else
                after1 = skip_one input
                when first_byte after1 is
                    Err _ -> Ok { body_start: after1, is_multiline: Bool.false }
                    Ok b2 ->
                        if b2 != 34 then Ok { body_start: after1, is_multiline: Bool.false }
                        else
                            after2 = skip_one after1
                            when first_byte after2 is
                                Err _ -> Ok { body_start: after2, is_multiline: Bool.false }
                                Ok b3 ->
                                    Ok {
                                        body_start: if b3 == 34 then skip_one after2 else after2,
                                        is_multiline: b3 == 34,
                                    }

read_raw_single_line_body : Str, U64 -> Result { string_value : Str, next : Str } [UnterminatedString, InvalidUtf8, DisallowedCodePoint]
read_raw_single_line_body = |_input, _hash_count|
    Err UnterminatedString

read_raw_multi_line_body : Str, U64 -> Result { string_value : Str, next : Str } [UnterminatedString, InvalidUtf8, DisallowedCodePoint]
read_raw_multi_line_body = |_input, _hash_count|
    Err UnterminatedString

###############################################################################
# Multiline String
###############################################################################
# TODO: implement this
read_multi_line_string : Str -> Result { string_value : Str, next : Str } [UnterminatedString, InvalidEscape, InvalidUtf8, DisallowedCodePoint, InvalidDedent]
read_multi_line_string = |_input|
    Err UnterminatedString
