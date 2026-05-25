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
# Public Parse API
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

expect
    # CRLF between two nodes produces 2 nodes
    result = parse "node1\r\nnode2\r\n"
    when result is
        Ok nodes -> List.len nodes == 2
        _ -> Bool.false

expect
    # // comment followed by newline correctly terminates the node
    result = parse "node1 // comment\nnode2\n"
    when result is
        Ok nodes -> List.len nodes == 2
        _ -> Bool.false

expect
    # CR after // comment correctly terminates the node
    result = parse "node1 // comment\rnode2\r"
    when result is
        Ok nodes -> List.len nodes == 2
        _ -> Bool.false

expect
    # CRLF after // comment — both consumed as one unit
    result = parse "node1 // comment\r\nnode2\r\n"
    when result is
        Ok nodes -> List.len nodes == 2
        _ -> Bool.false

expect
    # children block without semicolon, followed by another node
    result = parse "node1 {\nchild\n}\nnode2\n"
    when result is
        Ok nodes -> List.len nodes == 2
        _ -> Bool.false

expect
    # optional_child_semicolon: no ; after } at EOF
    result = parse "node {foo;bar;baz}"
    Result.is_ok result

expect
    # semicolon_after_child: ; after } on next line
    result = parse "node {\n     childnode\n};\n"
    Result.is_ok result
