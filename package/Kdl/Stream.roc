module [
    advance_one,
    asterisk,
    carriage_return,
    digit_nine,
    digit_zero,
    drop_prefix,
    first_byte,
    hex_value,
    hyphen_minus,
    is_ascii_digit,
    is_hex_digit,
    is_newline,
    is_whitespace,
    left_brace,
    left_paren,
    line_feed,
    number_sign,
    quotation_mark,
    reverse_solidus,
    right_brace,
    right_paren,
    skip_bytes,
    skip_line_space,
    skip_newline,
    skip_node_space,
    skip_terminator,
    skip_while,
    solidus,
    starts_with,
]

###############################################################################
# ASCII Constants (KDL Spec Names)
###############################################################################
quotation_mark = 34     # '"'
number_sign = 35        # '#'
asterisk = 42           # '*'
left_paren = 40         # '('
right_paren = 41        # ')'
left_brace = 123        # '{'
right_brace = 125       # '}'
hyphen_minus = 45       # '-'
solidus = 47            # '/'
reverse_solidus = 92    # '\'
line_feed = 10          # '\n'
carriage_return = 13    # '\r'
digit_zero = 48
digit_nine = 57

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

###############################################################################
# Cursor Skips
###############################################################################
# Skip past a single newline sequence. CRLF is consumed as one unit.
skip_newline : Str -> Str
skip_newline = |str|
    when first_byte str is
        Err _ -> str
        Ok byte ->
            if byte == carriage_return then
                after_cr = advance_one str
                when first_byte after_cr is
                    Ok b -> if b == line_feed then advance_one after_cr else after_cr
                    Err _ -> after_cr
            else if byte == line_feed then
                advance_one str
            else
                str

# Skip past a node terminator (newline or semicolon).
skip_terminator : Str -> Str
skip_terminator = |str|
    when first_byte str is
        Err _ -> str
        Ok 59 -> advance_one str  # ';'
        _ -> skip_newline str

###############################################################################
# Character Classification
###############################################################################
is_whitespace : U8 -> Bool
is_whitespace = |c| c == 9 or c == 32  # TAB, SPACE

expect
    is_whitespace 32 == Bool.true

is_newline : U8 -> Bool
is_newline = |c| c == line_feed or c == carriage_return

is_hex_digit : U8 -> Bool
is_hex_digit = |c|
    (c >= 48 and c <= 57) or (c >= 65 and c <= 70) or (c >= 97 and c <= 102)

expect
    is_hex_digit 70 == Bool.true

hex_value : U8 -> U8
hex_value = |c|
    if c >= 48 and c <= 57 then c - 48
    else if c >= 65 and c <= 70 then c - 65 + 10
    else if c >= 97 and c <= 102 then c - 97 + 10
    else 0

is_ascii_digit : U8 -> Bool
is_ascii_digit = |c| c >= digit_zero and c <= digit_nine

###############################################################################
# Layout Skipping
###############################################################################
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
                        after_cr = advance_one input
                        when first_byte after_cr is
                            Ok b -> if b == line_feed then advance_one after_cr else after_cr
                            Err _ -> after_cr
                    else
                        advance_one input
                skip_line_space after_nl
            else if byte == solidus then
                after_slash = advance_one input
                when first_byte after_slash is
                    Err _ -> input
                    Ok b ->
                        if b == solidus then
                            skip_line_space (skip_line_comment (advance_one after_slash))
                        else if b == asterisk then
                            when skip_block_comment (advance_one after_slash) is
                                Ok after_comment -> skip_line_space after_comment
                                Err _ -> input
                        else
                            input
            else
                input

# Skip whitespace, block comments, and line continuations.
# Also skips line comments (//) since they can appear within node bodies
# and effectively terminate the current token.
# Does NOT skip newlines (node-level only).
skip_node_space : Str -> Str
skip_node_space = |input|
    when first_byte input is
        Err _ -> input
        Ok byte ->
            if is_whitespace(byte) then
                skip_node_space (advance_one input)
            else if byte == solidus then
                after_slash = advance_one input
                when first_byte after_slash is
                    Ok b ->
                        if b == solidus then
                            # // line comment — skip comment content, leave newline for terminator detection
                            skip_node_space (skip_line_comment (advance_one after_slash))
                        else if b == asterisk then
                            when skip_block_comment (advance_one after_slash) is
                                Ok after_comment -> skip_node_space after_comment
                                Err _ -> input
                        else
                            input
                    Err _ -> input
            else if byte == reverse_solidus then
                when skip_escline (advance_one input) is
                    Ok after_esc -> skip_node_space after_esc
                    Err _ -> input
            else
                input

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
                skip_line_comment (advance_one input)

# Skip a block comment (/* ... */). Supports nested comments.
skip_block_comment : Str -> Result Str [UnterminatedComment]
skip_block_comment = |input|
    when first_byte input is
        Err _ -> Err UnterminatedComment
        Ok byte ->
            if byte == asterisk then
                after_star = advance_one input
                when first_byte after_star is
                    Ok b ->
                        if b == solidus then Ok (advance_one after_star)
                        else skip_block_comment after_star
                    Err _ -> skip_block_comment after_star
            else if byte == solidus then
                after_slash = advance_one input
                when first_byte after_slash is
                    Ok b ->
                        if b == asterisk then
                            when skip_block_comment (advance_one after_slash) is
                                Ok after_inner -> skip_block_comment after_inner
                                Err _ -> Err UnterminatedComment
                        else
                            skip_block_comment after_slash
                    Err _ -> skip_block_comment after_slash
            else
                skip_block_comment (advance_one input)

# Skip an escape-line continuation: \ (whitespace | // comment)* newline
skip_escline : Str -> Result Str [EndOfStream]
skip_escline = |input|
    when first_byte input is
        Err _ -> Err EndOfStream
        Ok byte ->
            if is_whitespace(byte) then
                skip_escline (advance_one input)
            else if byte == solidus then
                after_slash = advance_one input
                when first_byte after_slash is
                    Ok b ->
                        if b == solidus then Ok (advance_one (skip_line_comment (advance_one after_slash)))
                        else Ok input
                    Err _ -> Ok input
            else if is_newline(byte) then
                Ok (advance_one input)
            else
                Ok input
