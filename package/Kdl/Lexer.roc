module [
    Token,
    peek_token,
    read_annotation,
    read_identifier,
    read_value,
]

import Kdl.Stream exposing [carriage_return, digit_nine, digit_zero, first_byte, hyphen_minus, left_brace, left_paren, line_feed, number_sign, quotation_mark, right_brace, right_paren, semicolon, skip_node_space, skip_one]
import Kdl.String as String
import Kdl.Number as Number
import Kdl.Common exposing [KdlValue]

LexerError : [
    FormatError,
    InvalidIdentifier,
    InvalidUtf8,
    UnterminatedString,
]

Token : [
    StringLiteral,
    NumericLiteral,
    NodeTerminator,
    ChildStart,
    ChildEnd,
    IdentifierStart,
    Slashdash,
    EndOfStream,
]

###############################################################################
# Token Lookahead
###############################################################################
# Classify the next token without consuming it.
# Skips node_space first.
peek_token : Str -> Token
peek_token = |input|
    clean = skip_node_space input
    when first_byte clean is
        Err _ -> EndOfStream
        Ok byte ->
            if Str.starts_with clean "/-" then
                Slashdash
            else if byte == quotation_mark then
                StringLiteral
            else if byte == hyphen_minus then
                after_dash = skip_one clean
                when first_byte after_dash is
                    Ok b -> if b >= digit_zero and b <= digit_nine then NumericLiteral else IdentifierStart
                    Err _ -> IdentifierStart
            else if byte == semicolon or byte == line_feed or byte == carriage_return then
                NodeTerminator
            else if byte == left_brace then
                ChildStart
            else if byte == right_brace then
                ChildEnd
            else if byte >= digit_zero and byte <= digit_nine then
                NumericLiteral
            else
                IdentifierStart

###############################################################################
# Type Readers
###############################################################################
read_annotation : Str -> Result { annotation_name : Str, next : Str } [NoAnnotationFound, MalformedAnnotation]
read_annotation = |input|
    clean = skip_node_space input
    when first_byte clean is
        Err _ -> Err NoAnnotationFound
        Ok byte ->
            if byte != left_paren then Err NoAnnotationFound
            else
                after_open = skip_one clean
                inner_start = skip_node_space after_open
                when String.read_identifier inner_start is
                    Err _ -> Err MalformedAnnotation
                    Ok { string_value, next } ->
                        before_close = skip_node_space next
                        when first_byte before_close is
                            Ok b ->
                                if b == right_paren then
                                    Ok { annotation_name: string_value, next: skip_one before_close }
                                else
                                    Err MalformedAnnotation
                            Err _ -> Err MalformedAnnotation

# Shadows the import, use fully qualified name
read_identifier : Str -> Result { string_value : Str, next : Str } [InvalidIdentifier, ReservedKeyword, EndOfBuffer]
read_identifier = |input|
    clean = skip_node_space input
    String.read_identifier clean

read_value : Str -> Result { value : KdlValue, next : Str } LexerError
read_value = |input|
    clean = skip_node_space input
    when first_byte clean is
        Err _ -> Err FormatError
        Ok byte ->
            if byte == quotation_mark then
                when String.read_quoted_string clean is
                    Err _ -> Err FormatError
                    Ok { string_value, next } ->
                        Ok { value: KdlStr string_value, next }
            else if byte == number_sign then
                read_hash_value clean
            else if byte == hyphen_minus then
                read_number_or_ident clean
            else if byte >= digit_zero and byte <= digit_nine then
                read_number clean
            else
                when String.read_identifier clean is
                    Err _ -> Err FormatError
                    Ok { string_value, next } ->
                        Ok { value: KdlStr string_value, next }

read_hash_value : Str -> Result { value : KdlValue, next : Str } LexerError
read_hash_value = |input|
    when read_keyword input is
        Err _ -> Err FormatError
        Ok { word, after } ->
            when word is
                "true" -> Ok { value: KdlBool Bool.true, next: after }
                "false" -> Ok { value: KdlBool Bool.false, next: after }
                "null" -> Ok { value: KdlNull, next: after }
                "inf" -> Ok { value: KdlInf, next: after }
                "-inf" -> Ok { value: KdlNegInf, next: after }
                "nan" -> Ok { value: KdlNaN, next: after }
                _ -> Err FormatError

read_keyword : Str -> Result { word : Str, after : Str } LexerError
read_keyword = |input|
    # Skip the '#' character first
    after_hash = skip_one input
    loop_keyword after_hash ""

loop_keyword : Str, Str -> Result { word : Str, after : Str } LexerError
loop_keyword = |input, acc|
    when first_byte input is
        Err _ ->
            if Str.is_empty acc then Err FormatError else Ok { word: acc, after: input }
        Ok byte ->
            if (byte >= 97 and byte <= 122) or byte == 45 then  # a-z, '-'
                loop_keyword (skip_one input) (Str.concat acc (Str.from_utf8 [byte] |> result_or_empty))
            else if Str.is_empty acc then
                Err FormatError
            else
                Ok { word: acc, after: input }

result_or_empty : Result Str [BadUtf8 _] -> Str
result_or_empty = |r|
    when r is
        Ok s -> s
        Err _ -> ""

read_number_or_ident : Str -> Result { value : KdlValue, next : Str } LexerError
read_number_or_ident = |input|
    after_dash = skip_one input
    when first_byte after_dash is
        Err _ -> Ok { value: KdlStr "-", next: after_dash }
        Ok next_byte ->
            if next_byte >= digit_zero and next_byte <= digit_nine then
                read_number input
            else
                when String.read_identifier input is
                    Err _ -> Err FormatError
                    Ok { string_value, next } ->
                        Ok { value: KdlStr string_value, next }

# Shadows the import, use fully qualified name
read_number : Str -> Result { value : KdlValue, next : Str } LexerError
read_number = |input|
    when Number.read_number input is
        Err _ -> Err FormatError
        Ok { float_value, next } ->
            Ok { value: KdlNum float_value, next }
