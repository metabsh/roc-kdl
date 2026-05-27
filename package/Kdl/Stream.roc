module [
    asterisk,
    carriage_return,
    digit_nine,
    digit_one,
    digit_zero,
    drop_prefix,
    equals_sign,
    first_byte,
    full_stop,
    hex_value,
    hyphen_minus,
    is_ascii_digit,
    is_between,
    is_hex_digit,
    is_newline,
    is_whitespace,
    left_brace,
    left_paren,
    line_feed,
    number_sign,
    plus_sign,
    quotation_mark,
    reverse_solidus,
    right_brace,
    right_paren,
    semicolon,
    skip_bytes,
    skip_line_space,
    skip_newline,
    skip_node_space,
    skip_one,
    skip_terminator,
    skip_while,
    solidus,
    space,
    starts_with,
    tab,
    underscore,
]

###############################################################################
# ASCII Constants (KDL Spec Names)
###############################################################################
# KDL spec names
quotation_mark = 34     # '"'
hyphen_minus = 45       # '-'
number_sign = 35        # '#'
solidus = 47            # '/'
reverse_solidus = 92    # '\'
left_paren = 40         # '('
right_paren = 41        # ')'
left_brace = 123        # '{'
right_brace = 125       # '}'
line_feed = 10          # '\n'
carriage_return = 13    # '\r'
asterisk = 42           # '*'

# ASCII character names
digit_zero = 48         # '0'
digit_one = 49          # '1'
digit_nine = 57         # '9'
equals_sign = 61        # '='
full_stop = 46          # '.'
plus_sign = 43          # '+'
semicolon = 59          # ';'
space = 32              # ' '
tab = 9                 # '\t'
underscore = 95         # '_'

###############################################################################
# Cursor Primitives
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

###############################################################################
# Cursor Skipping
###############################################################################
# Skip a block comment (/* ... */). Supports nested comments.
skip_block_comment : Str -> Result Str [UnterminatedComment]
skip_block_comment = |input|
    when first_byte input is
        Err _ -> Err UnterminatedComment
        Ok byte ->
            if byte == asterisk then
                after_star = skip_one input
                when first_byte after_star is
                    Ok b ->
                        if b == solidus then Ok (skip_one after_star)
                        else skip_block_comment after_star
                    Err _ -> skip_block_comment after_star
            else if byte == solidus then
                after_slash = skip_one input
                when first_byte after_slash is
                    Ok b ->
                        if b == asterisk then
                            when skip_block_comment (skip_one after_slash) is
                                Ok after_inner -> skip_block_comment after_inner
                                Err _ -> Err UnterminatedComment
                        else
                            skip_block_comment after_slash
                    Err _ -> skip_block_comment after_slash
            else
                skip_block_comment (skip_one input)

# Skip n bytes from the start of the string.
skip_bytes : Str, U64 -> Str
skip_bytes = |str, n|
    bytes = Str.to_utf8 str
    remaining = List.drop_first bytes n
    when Str.from_utf8 remaining is
        Ok s -> s
        Err _ -> ""

# Skip an escape-line continuation: \ (whitespace | // comment)* newline
skip_escline : Str -> Result Str [EndOfStream]
skip_escline = |input|
    when first_byte input is
        Err _ -> Err EndOfStream
        Ok byte ->
            if is_whitespace(byte) then
                skip_escline (skip_one input)
            else if byte == solidus then
                after_slash = skip_one input
                when first_byte after_slash is
                    Ok b ->
                        if b == solidus then Ok (skip_one (skip_line_comment (skip_one after_slash)))
                        else Ok input
                    Err _ -> Ok input
            else if is_newline(byte) then
                Ok (skip_one input)
            else
                Ok input

# Skip a single-line comment (// ... up to newline or EOF).
# The terminating newline is NOT consumed — it remains in the input.
skip_line_comment : Str -> Str
skip_line_comment = |input|
    when first_byte input is
        Err _ -> input
        Ok byte ->
            if is_newline(byte) then
                input
            else
                skip_line_comment (skip_one input)

# Skip whitespace, newlines, line comments (//), and block comments (/* */).
# CRLF is treated as a single newline.
skip_line_space : Str -> Str
skip_line_space = |input|
    when first_byte input is
        Err _ -> input
        Ok byte ->
            if is_whitespace(byte) or is_newline(byte) then
                after_nl =
                    if byte == carriage_return then
                        after_cr = skip_one input
                        when first_byte after_cr is
                            Ok b -> if b == line_feed then skip_one after_cr else after_cr
                            Err _ -> after_cr
                    else
                        skip_one input
                skip_line_space after_nl
            else if byte == solidus then
                after_slash = skip_one input
                when first_byte after_slash is
                    Err _ -> input
                    Ok b ->
                        if b == solidus then
                            skip_line_space (skip_line_comment (skip_one after_slash))
                        else if b == asterisk then
                            when skip_block_comment (skip_one after_slash) is
                                Ok after_comment -> skip_line_space after_comment
                                Err _ -> input
                        else
                            input
            else
                input

# Skip past a single newline sequence. CRLF is consumed as one unit.
skip_newline : Str -> Str
skip_newline = |str|
    when first_byte str is
        Err _ -> str
        Ok byte ->
            if byte == carriage_return then
                after_cr = skip_one str
                when first_byte after_cr is
                    Ok b -> if b == line_feed then skip_one after_cr else after_cr
                    Err _ -> after_cr
            else if byte == line_feed then
                skip_one str
            else
                str

# Skip whitespace, block comments, and line continuations.
# Also skips line comments (//) since they can appear within node bodies and effectively terminate the current token.
# Does NOT skip newlines (node-level only).
skip_node_space : Str -> Str
skip_node_space = |input|
    when first_byte input is
        Err _ -> input
        Ok byte ->
            if is_whitespace(byte) then
                skip_node_space (skip_one input)
            else if byte == solidus then
                after_slash = skip_one input
                when first_byte after_slash is
                    Ok b ->
                        if b == solidus then
                            # // line comment — skip comment content, leave newline for terminator detection
                            skip_node_space (skip_line_comment (skip_one after_slash))
                        else if b == asterisk then
                            when skip_block_comment (skip_one after_slash) is
                                Ok after_comment -> skip_node_space after_comment
                                Err _ -> input
                        else
                            input
                    Err _ -> input
            else if byte == reverse_solidus then
                when skip_escline (skip_one input) is
                    Ok after_esc -> skip_node_space after_esc
                    Err _ -> input
            else
                input

# Advance past a single byte.
skip_one : Str -> Str
skip_one = |str| skip_bytes str 1

# Skip past a node terminator (newline or semicolon).
skip_terminator : Str -> Str
skip_terminator = |str|
    when first_byte str is
        Err _ -> str
        Ok byte ->
            if byte == semicolon then skip_one str else skip_newline str

# Skip while the predicate holds on the first byte.
skip_while : Str, (U8 -> Bool) -> Str
skip_while = |str, pred|
    when first_byte str is
        Err _ -> str
        Ok byte ->
            if pred byte then
                skip_while (skip_one str) pred
            else
                str

###############################################################################
# Character Classification
###############################################################################
hex_value : U8 -> U8
hex_value = |c|
    if is_ascii_digit(c) then c - digit_zero
    else if is_between(c, 65, 70) then c - 65 + 10
    else if is_between(c, 97, 102) then c - 97 + 10
    else 0

is_ascii_digit : U8 -> Bool
is_ascii_digit = |c| c >= digit_zero and c <= digit_nine

is_between : U8, U8, U8 -> Bool
is_between = |byte, low, high| byte >= low and byte <= high

is_hex_digit : U8 -> Bool
is_hex_digit = |c|
    (is_ascii_digit(c)) or is_between(c, 65, 70) or is_between(c, 97, 102)

expect
    is_hex_digit 70 == Bool.true

is_newline : U8 -> Bool
is_newline = |c| c == line_feed or c == carriage_return

is_whitespace : U8 -> Bool
is_whitespace = |c| c == tab or c == space

expect
    is_whitespace 32 == Bool.true
