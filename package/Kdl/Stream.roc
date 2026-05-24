module [
    starts_with,
    drop_prefix,
    first_byte,
    skip_bytes,
    advance_one,
    skip_while,
    is_whitespace,
    is_newline,
    is_hex_digit,
    hex_value,
]

###############################################################################
# Cursor Primities
###############################################################################
starts_with : Str, Str -> Bool
starts_with = |str, prefix| Str.starts_with str prefix

expect
    starts_with "hello" "hel" == Bool.true

expect
    starts_with "hello" "foo" == Bool.false

drop_prefix : Str, Str -> Str
drop_prefix = |str, prefix| Str.drop_prefix str prefix

# Peek at the first byte of a string (for ASCII classification).
first_byte : Str -> Result U8 [EndOfStream]
first_byte = |str|
    bytes = Str.to_utf8 str
    when bytes is
        [c, ..] -> Ok c
        [] -> Err EndOfStream

expect
    result = first_byte "ABC"
    when result is
        Ok 65 -> Bool.true
        _ -> Bool.false

# Skip n bytes from the start of the string.
skip_bytes : Str, U64 -> Str
skip_bytes = |str, n|
    bytes = Str.to_utf8 str
    remaining = List.drop_first bytes n
    when Str.from_utf8 remaining is
        Ok s -> s
        Err _ -> ""

# Advance past a single byte.
advance_one : Str -> Str
advance_one = |str| skip_bytes str 1

# Skip while the predicate holds on the first byte.
skip_while : Str, (U8 -> Bool) -> Str
skip_while = |str, pred|
    when first_byte str is
        Err _ -> str
        Ok byte ->
            if pred byte then
                skip_while (advance_one str) pred
            else
                str

is_whitespace : U8 -> Bool
is_whitespace = |c| c == 9 or c == 32  # TAB, SPACE

is_newline : U8 -> Bool
is_newline = |c| c == 10 or c == 13  # LF, CR

is_hex_digit : U8 -> Bool
is_hex_digit = |c|
    (c >= 48 and c <= 57) or (c >= 65 and c <= 70) or (c >= 97 and c <= 102)

hex_value : U8 -> U8
hex_value = |c|
    if c >= 48 and c <= 57 then c - 48
    else if c >= 65 and c <= 70 then c - 65 + 10
    else if c >= 97 and c <= 102 then c - 97 + 10
    else 0
