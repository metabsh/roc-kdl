module [
    Token,
    skip_line_space,
    skip_node_space,
    peek_token,
    read_annotation,
    read_identifier,
    read_value,
]

import Kdl.Stream as Stream
import Kdl.String as String
import Kdl.Number as Number
import Kdl.Common exposing [KdlValue]

LexerError : [
    FormatError,
    InvalidIdentifier,
    InvalidUtf8,
    UnterminatedString,
]

###############################################################################
# ASCII Constants (KDL Spec Names)
###############################################################################
quotation_mark = 34     # '"'
left_paren = 40         # '('
right_paren = 41        # ')'
left_brace = 123        # '{'
right_brace = 125       # '}'
solidus = 47            # '/'
reverse_solidus = 92    # '\'
number_sign = 35        # '#'
hyphen_minus = 45       # '-'
line_feed = 10          # '\n'
carriage_return = 13    # '\r'
digit_zero = 48
digit_nine = 57
asterisk = 42           # '*'

Token : [
    StringLiteral,
    NumericLiteral,
    NodeTerminator,
    ChildStart,
    ChildEnd,
    IdentifierStart,
    EndOfStream,
]

###############################################################################
# Layout Skipping
###############################################################################
# Skip whitespace, newlines, line comments (//), and block comments (/* */).
# CRLF is treated as a single newline.
skip_line_space : Str -> Str
skip_line_space = |input|
    when Stream.first_byte input is
        Err _ -> input
        Ok byte ->
            if Stream.is_whitespace(byte) or Stream.is_newline(byte) then
                after_nl =
                    if byte == carriage_return then
                        after_cr = Stream.advance_one input
                        when Stream.first_byte after_cr is
                            Ok b -> if b == line_feed then Stream.advance_one after_cr else after_cr
                            Err _ -> after_cr
                    else
                        Stream.advance_one input
                skip_line_space after_nl
            else if byte == solidus then
                after_slash = Stream.advance_one input
                when Stream.first_byte after_slash is
                    Err _ -> input
                    Ok b ->
                        if b == solidus then
                            skip_line_space (skip_line_comment (Stream.advance_one after_slash))
                        else if b == asterisk then
                            when skip_block_comment (Stream.advance_one after_slash) is
                                Ok after_comment -> skip_line_space after_comment
                                Err _ -> input
                        else
                            input
            else
                input

# Skip whitespace, block comments, and line continuations.
# Does NOT skip newlines (node-level only).
skip_node_space : Str -> Str
skip_node_space = |input|
    when Stream.first_byte input is
        Err _ -> input
        Ok byte ->
            if Stream.is_whitespace(byte) then
                skip_node_space (Stream.advance_one input)
            else if byte == solidus then
                after_slash = Stream.advance_one input
                when Stream.first_byte after_slash is
                    Ok b ->
                        if b == asterisk then
                            when skip_block_comment (Stream.advance_one after_slash) is
                                Ok after_comment -> skip_node_space after_comment
                                Err _ -> input
                        else
                            input
                    Err _ -> input
            else if byte == reverse_solidus then
                when skip_escline (Stream.advance_one input) is
                    Ok after_esc -> skip_node_space after_esc
                    Err _ -> input
            else
                input

# Skip a single-line comment (// ... up to newline or EOF).
skip_line_comment : Str -> Str
skip_line_comment = |input|
    when Stream.first_byte input is
        Err _ -> input
        Ok byte ->
            if Stream.is_newline(byte) then
                if byte == carriage_return then
                    after_cr = Stream.advance_one input
                    when Stream.first_byte after_cr is
                        Ok b -> if b == line_feed then Stream.advance_one after_cr else after_cr
                        Err _ -> after_cr
                else
                    Stream.advance_one input
            else
                skip_line_comment (Stream.advance_one input)

# Skip a block comment (/* ... */). Supports nested comments.
skip_block_comment : Str -> Result Str [UnterminatedComment]
skip_block_comment = |input|
    when Stream.first_byte input is
        Err _ -> Err UnterminatedComment
        Ok byte ->
            if byte == asterisk then
                after_star = Stream.advance_one input
                when Stream.first_byte after_star is
                    Ok b ->
                        if b == solidus then Ok (Stream.advance_one after_star)
                        else skip_block_comment after_star
                    Err _ -> skip_block_comment after_star
            else if byte == solidus then
                after_slash = Stream.advance_one input
                when Stream.first_byte after_slash is
                    Ok b ->
                        if b == asterisk then
                            when skip_block_comment (Stream.advance_one after_slash) is
                                Ok after_inner -> skip_block_comment after_inner
                                Err _ -> Err UnterminatedComment
                        else
                            skip_block_comment after_slash
                    Err _ -> skip_block_comment after_slash
            else
                skip_block_comment (Stream.advance_one input)

# Skip an escape-line continuation: \ (whitespace | // comment)* newline
skip_escline : Str -> Result Str [EndOfStream]
skip_escline = |input|
    when Stream.first_byte input is
        Err _ -> Err EndOfStream
        Ok byte ->
            if Stream.is_whitespace(byte) then
                skip_escline (Stream.advance_one input)
            else if byte == solidus then
                after_slash = Stream.advance_one input
                when Stream.first_byte after_slash is
                    Ok b ->
                        if b == solidus then Ok (skip_line_comment (Stream.advance_one after_slash))
                        else Ok input
                    Err _ -> Ok input
            else if Stream.is_newline(byte) then
                Ok (Stream.advance_one input)
            else
                Ok input

###############################################################################
# Token Lookahead
###############################################################################
# Classify the next token without consuming it.
# Skips node_space first.
peek_token : Str -> Token
peek_token = |input|
    clean = skip_node_space input
    when Stream.first_byte clean is
        Err _ -> EndOfStream
        Ok byte ->
            if byte == quotation_mark then
                StringLiteral
            else if byte == hyphen_minus then
                after_dash = Stream.advance_one clean
                when Stream.first_byte after_dash is
                    Ok b -> if b >= digit_zero and b <= digit_nine then NumericLiteral else IdentifierStart
                    Err _ -> IdentifierStart
            else if byte == 59 or byte == line_feed or byte == carriage_return then
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
    when Stream.first_byte clean is
        Err _ -> Err NoAnnotationFound
        Ok byte ->
            if byte != left_paren then Err NoAnnotationFound
            else
                after_open = Stream.advance_one clean
                inner_start = skip_node_space after_open
                when String.read_identifier inner_start is
                    Err _ -> Err MalformedAnnotation
                    Ok { string_value, next } ->
                        before_close = skip_node_space next
                        when Stream.first_byte before_close is
                            Ok b ->
                                if b == right_paren then
                                    Ok { annotation_name: string_value, next: Stream.advance_one before_close }
                                else
                                    Err MalformedAnnotation
                            Err _ -> Err MalformedAnnotation

read_identifier : Str -> Result { string_value : Str, next : Str } [InvalidIdentifier, ReservedKeyword, EndOfBuffer]
read_identifier = |input|
    clean = skip_node_space input
    String.read_identifier clean

read_value : Str -> Result { value : KdlValue, next : Str } LexerError
read_value = |input|
    clean = skip_node_space input
    when Stream.first_byte clean is
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
    after_hash = Stream.advance_one input
    loop_keyword after_hash ""

loop_keyword : Str, Str -> Result { word : Str, after : Str } LexerError
loop_keyword = |input, acc|
    when Stream.first_byte input is
        Err _ ->
            if Str.is_empty acc then Err FormatError else Ok { word: acc, after: input }
        Ok byte ->
            if (byte >= 97 and byte <= 122) or byte == 45 then  # a-z, '-'
                loop_keyword (Stream.advance_one input) (Str.concat acc (Str.from_utf8 [byte] |> result_or_empty))
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
    after_dash = Stream.advance_one input
    when Stream.first_byte after_dash is
        Err _ -> Ok { value: KdlStr "-", next: after_dash }
        Ok next_byte ->
            if next_byte >= digit_zero and next_byte <= digit_nine then
                read_number input
            else
                when String.read_identifier input is
                    Err _ -> Err FormatError
                    Ok { string_value, next } ->
                        Ok { value: KdlStr string_value, next }

read_number : Str -> Result { value : KdlValue, next : Str } LexerError
read_number = |input|
    when Number.read_number input is
        Err _ -> Err FormatError
        Ok { float_value, next } ->
            Ok { value: KdlNum float_value, next }
