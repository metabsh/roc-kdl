module [
    read_keyword_number,
    read_number,
]

import Kdl.Stream exposing [digit_nine, digit_one, digit_zero, first_byte, full_stop, hex_value, hyphen_minus, is_hex_digit, number_sign, plus_sign, skip_one, underscore]

###############################################################################
# IEEE 754 constants
###############################################################################
inf : F64
inf = Num.infinity_f64

neg_inf : F64
neg_inf = Num.neg Num.infinity_f64

nan : F64
nan = Num.nan_f64

###############################################################################
# High-Level Number handling
###############################################################################
# Dispatches based on prefix: 0x → hex, 0o → octal, 0b → binary, else decimal.
read_number : Str -> Result { float_value : F64, next : Str } [InvalidNumericLiteral]
read_number = |input|
    when first_byte input is
        Err _ -> Err InvalidNumericLiteral
        Ok byte ->
            if byte == digit_zero then  # '0'
                after_zero = skip_one input
                when first_byte after_zero is
                    Err _ ->
                        Ok { float_value: 0.0, next: after_zero }
                    Ok next_byte ->
                        when next_byte is
                            120 -> read_hex_number (skip_one after_zero)      # 'x'
                            111 -> read_octal_number (skip_one after_zero)    # 'o'
                            98  -> read_binary_number (skip_one after_zero)   # 'b'
                            _   -> read_decimal_number input
            else
                read_decimal_number input

###############################################################################
# Decimal
###############################################################################
read_decimal_number : Str -> Result { float_value : F64, next : Str } [InvalidNumericLiteral]
read_decimal_number = |input|
    after_sign = skip_sign input
    { num_str, next } = (collect_number_str after_sign "")?
    when Str.to_f64 num_str is
        Err _ -> Err InvalidNumericLiteral
        Ok val -> Ok { float_value: val, next }

skip_sign : Str -> Str
skip_sign = |input|
    when first_byte input is
        Err _ -> input
        Ok byte ->
            if byte == plus_sign or byte == hyphen_minus then skip_one input else input  # '+' or '-'

# Collect a number string (digits, '.', 'e', 'E', '+', '-', '_').
# Stops at the first character that isn't part of a number.
# Rejects numbers starting with '.' (no integer part).
collect_number_str : Str, Str -> Result { num_str : Str, next : Str } [InvalidNumericLiteral]
collect_number_str = |input, acc|
    when first_byte input is
        Err _ ->
            if Str.is_empty acc then Err InvalidNumericLiteral
            else Ok { num_str: strip_underscores acc, next: input }
        Ok byte ->
            if byte >= digit_zero and byte <= digit_nine then
                char_str = Str.from_utf8 [byte] |> result_or_empty
                collect_number_str (skip_one input) (Str.concat acc char_str)
            else if byte == full_stop then
                # '.' only valid if we've already seen a digit
                if Str.is_empty acc then
                    Err InvalidNumericLiteral
                else
                    char_str = Str.from_utf8 [byte] |> result_or_empty
                    collect_number_str (skip_one input) (Str.concat acc char_str)
            else if (byte == 69 or byte == 101) or byte == underscore or byte == plus_sign or byte == hyphen_minus then
                # 'e', 'E', '_', '+', '-' only valid after at least one digit
                if Str.is_empty acc then
                    Err InvalidNumericLiteral
                else
                    char_str = Str.from_utf8 [byte] |> result_or_empty
                    collect_number_str (skip_one input) (Str.concat acc char_str)
            else if Str.is_empty acc then
                Err InvalidNumericLiteral
            else
                Ok { num_str: strip_underscores acc, next: input }

result_or_empty : Result Str [BadUtf8 _] -> Str
result_or_empty = |r|
    when r is
        Ok s -> s
        Err _ -> ""

strip_underscores : Str -> Str
strip_underscores = |s|
    bytes = Str.to_utf8 s
    filtered = List.keep_if bytes |b| b != underscore
    when Str.from_utf8 filtered is
        Ok s2 -> s2
        Err _ -> ""

###############################################################################
# Hexadecimal
###############################################################################
read_hex_number : Str -> Result { float_value : F64, next : Str } [InvalidNumericLiteral]
read_hex_number = |input|
    { num_str, next } = (collect_hex_str input "")?
    val = (parse_hex_to_u64 num_str)?
    Ok { float_value: Num.to_f64 val, next }

collect_hex_str : Str, Str -> Result { num_str : Str, next : Str } [InvalidNumericLiteral]
collect_hex_str = |input, acc|
    when first_byte input is
        Err _ ->
            if Str.is_empty acc then Err InvalidNumericLiteral
            else Ok { num_str: strip_underscores acc, next: input }
        Ok byte ->
            if is_hex_digit(byte) or byte == underscore then
                char_str = Str.from_utf8 [byte] |> result_or_empty
                collect_hex_str (skip_one input) (Str.concat acc char_str)
            else if Str.is_empty acc then
                Err InvalidNumericLiteral
            else
                Ok { num_str: strip_underscores acc, next: input }

parse_hex_to_u64 : Str -> Result U64 [InvalidNumericLiteral]
parse_hex_to_u64 = |str|
    bytes = Str.to_utf8 str
    List.walk bytes (Ok 0) |state_result, byte|
        acc = state_result?
        Ok (acc * 16 + hex_byte_to_u64 byte)

hex_byte_to_u64 : U8 -> U64
hex_byte_to_u64 = |byte| hex_value byte |> Num.to_u64

###############################################################################
# Octal
###############################################################################
read_octal_number : Str -> Result { float_value : F64, next : Str } [InvalidNumericLiteral]
read_octal_number = |input|
    { num_str, next } = (collect_octal_str input "")?
    val = (parse_octal_to_u64 num_str)?
    Ok { float_value: Num.to_f64 val, next }

collect_octal_str : Str, Str -> Result { num_str : Str, next : Str } [InvalidNumericLiteral]
collect_octal_str = |input, acc|
    when first_byte input is
        Err _ ->
            if Str.is_empty acc then Err InvalidNumericLiteral
            else Ok { num_str: acc, next: input }
        Ok byte ->
            if (byte >= digit_zero and byte <= 55) or byte == underscore then
                char_str = Str.from_utf8 [byte] |> result_or_empty
                collect_octal_str (skip_one input) (Str.concat acc char_str)
            else if byte >= digit_zero and byte <= digit_nine then
                # 8-9: invalid octal digit
                Err InvalidNumericLiteral
            else if Str.is_empty acc then
                Err InvalidNumericLiteral
            else
                Ok { num_str: strip_underscores acc, next: input }

parse_octal_to_u64 : Str -> Result U64 [InvalidNumericLiteral]
parse_octal_to_u64 = |str|
    bytes = Str.to_utf8 str
    List.walk bytes (Ok 0) |state_result, byte|
        acc = state_result?
        Ok (acc * 8 + Num.to_u64 (byte - digit_zero))

###############################################################################
# Binary
###############################################################################
read_binary_number : Str -> Result { float_value : F64, next : Str } [InvalidNumericLiteral]
read_binary_number = |input|
    { num_str, next } = (collect_binary_str input "")?
    val = (parse_binary_to_u64 num_str)?
    Ok { float_value: Num.to_f64 val, next }

collect_binary_str : Str, Str -> Result { num_str : Str, next : Str } [InvalidNumericLiteral]
collect_binary_str = |input, acc|
    when first_byte input is
        Err _ ->
            if Str.is_empty acc then Err InvalidNumericLiteral
            else Ok { num_str: acc, next: input }
        Ok byte ->
            if byte == digit_zero or byte == digit_one or byte == underscore then
                char_str = Str.from_utf8 [byte] |> result_or_empty
                collect_binary_str (skip_one input) (Str.concat acc char_str)
            else if byte >= 50 and byte <= 57 then
                # 2-9: invalid binary digit
                Err InvalidNumericLiteral
            else if Str.is_empty acc then
                Err InvalidNumericLiteral
            else
                Ok { num_str: strip_underscores acc, next: input }

parse_binary_to_u64 : Str -> Result U64 [InvalidNumericLiteral]
parse_binary_to_u64 = |str|
    bytes = Str.to_utf8 str
    List.walk bytes (Ok 0) |state_result, byte|
        acc = state_result?
        Ok (acc * 2 + Num.to_u64 (byte - digit_zero))

###############################################################################
# Special Number Types: Inf, -Inf, NaN
###############################################################################
read_keyword_number : Str -> Result { float_value : F64, next : Str } [InvalidNumericLiteral, EndOfBuffer]
read_keyword_number = |input|
    when first_byte input is
        Err _ -> Err EndOfBuffer
        Ok byte ->
            if byte != number_sign then Err InvalidNumericLiteral  # '#'
            else
                after_hash = skip_one input
                when first_byte after_hash is
                    Err _ -> Err InvalidNumericLiteral
                    Ok next_byte ->
                        when next_byte is
                            105 -> expect_keyword after_hash "inf" inf
                            110 -> expect_keyword after_hash "nan" nan
                            45  ->
                                after_dash = skip_one after_hash
                                expect_keyword after_dash "inf" neg_inf
                            _ -> Err InvalidNumericLiteral

expect_keyword : Str, Str, F64 -> Result { float_value : F64, next : Str } [InvalidNumericLiteral]
expect_keyword = |input, expected, value|
    if Str.starts_with input expected then
        after_word = Str.drop_prefix input expected
        Ok { float_value: value, next: after_word }
    else
        Err InvalidNumericLiteral
