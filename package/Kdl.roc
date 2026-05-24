module [KdlError, KdlNode, KdlValue, Entry, parse]

import Kdl.Parser

###############################################################################
# Types (structurally identical to Kdl.Common)
###############################################################################
KdlValue : [
    KdlStr Str,
    KdlNum F64,
    KdlBool Bool,
    KdlNull,
    KdlInf,
    KdlNegInf,
    KdlNaN,
]

KdlNode : [KdlNodeRecord {
    name : Str,
    type_annotation : Result Str [None],
    entries : List Entry,
    children : List KdlNode,
}]

KdlError : [
    KdlFormatError,
    InvalidIdentifier,
    InvalidNumericLiteral,
    InvalidTypeAnnotation Str,
    InvalidUtf8,
    MalformedAnnotation,
    NoAnnotationFound,
    NoIdentifierFound,
    UnexpectedEof,
    UnterminatedString,
]

Entry : {
    name : Result Str [None],
    value : KdlValue,
}

###############################################################################
# Public API
###############################################################################
parse : Str -> Result (List KdlNode) KdlError
parse = Kdl.Parser.parse

###############################################################################
# Tests
###############################################################################
expect
    result = parse ""
    when result is
        Ok nodes -> List.is_empty nodes
        _ -> Bool.false

expect
    result = parse "foo"
    when result is
        Ok _ -> Bool.true
        _ -> Bool.false

expect
    result = parse "parent {}"
    when result is
        Ok nodes -> List.len nodes == 1
        _ -> Bool.false

expect
    result = parse "foo;bar"
    when result is
        Ok nodes -> List.len nodes == 2
        _ -> Bool.false

expect
    result = parse "\"hello\""
    when result is
        Err _ -> Bool.true
        _ -> Bool.false

expect
    result = parse "(published)date"
    when result is
        Ok nodes -> List.len nodes == 1
        _ -> Bool.false

expect
    result = parse "node key=\"val\""
    when result is
        Ok _ -> Bool.true
        _ -> Bool.false

expect
    result = parse "node port=8080"
    when result is
        Ok _ -> Bool.true
        _ -> Bool.false
