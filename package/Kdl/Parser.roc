module [
    parse,
]

import Kdl.Stream exposing [advance_one, first_byte, skip_terminator]
import Kdl.Lexer exposing [peek_token, read_annotation, read_identifier, read_value, skip_line_space, skip_node_space]
import Kdl.Common exposing [KdlNode, KdlError, Entry]

parse : Str -> Result (List KdlNode) KdlError
parse = |input|
    when parse_nodes input [] is
        Ok { nodes, next: _ } -> Ok nodes
        Err err -> Err err

###############################################################################
# Helpers
###############################################################################

# Parse a children block: skips '{', parses nodes until '}', skips '}'.
parse_child_block : Str -> Result { children : List KdlNode, next : Str } KdlError
parse_child_block = |input|
    inside = advance_one input
    when parse_nodes inside [] is
        Err err -> Err err
        Ok { nodes, next: after_children } ->
            Ok { children: nodes, next: advance_one after_children }

# Read a value (argument or property key), optionally followed by '=' .
read_entry : Str -> Result { entry : Entry, next : Str } KdlError
read_entry = |input|
    when read_value input is
        Err _ -> Err KdlFormatError
        Ok { value, next } ->
            when value is
                KdlStr str_val ->
                    after_id = skip_node_space next
                    if first_byte after_id == Ok 61 then
                        after_equals = skip_node_space (advance_one after_id)
                        when read_value after_equals is
                            Err _ -> Err KdlFormatError
                            Ok { value: prop_val, next: after_value } ->
                                Ok { entry: { name: Ok str_val, value: prop_val }, next: after_value }
                    else
                        Ok { entry: { name: Err None, value }, next }
                _ ->
                    Ok { entry: { name: Err None, value }, next }

# Skip the element after /- (argument, property, or children block).
skip_slashdash_entry : Str -> Result Str KdlError
skip_slashdash_entry = |input|
    clean = skip_node_space input
    when peek_token clean is
        ChildStart ->
            when parse_child_block clean is
                Err err -> Err err
                Ok { next } -> Ok (skip_node_space next)
        NodeTerminator ->
            Ok (skip_terminator clean)
        _ ->
            when read_entry clean is
                Err _ -> Err KdlFormatError
                Ok { next } -> Ok next

###############################################################################
# Single Node
###############################################################################
parse_node : Str -> Result { node : KdlNode, next : Str } KdlError
parse_node = |input|
    active = skip_line_space input
    { stream: with_anno, annotation } =
        when read_annotation active is
            Ok { annotation_name, next } ->
                { stream: next, annotation: Ok annotation_name }
            Err _ ->
                { stream: active, annotation: Err None }

    when read_identifier with_anno is
        Err _ -> Err KdlFormatError
        Ok { string_value: node_name, next: after_name } ->
            when parse_node_body after_name [] is
                Err err -> Err err
                Ok { entries: entries_list, children, next } ->
                    node = KdlNodeRecord {
                        name: node_name,
                        type_annotation: annotation,
                        entries: entries_list,
                        children,
                    }
                    Ok { node, next }

###############################################################################
# Node List
###############################################################################
parse_nodes : Str, List KdlNode -> Result { nodes : List KdlNode, next : Str } KdlError
parse_nodes = |input, acc|
    active = skip_line_space input
    when peek_token active is
        EndOfStream -> Ok { nodes: acc, next: active }
        ChildEnd -> Ok { nodes: acc, next: active }
        NodeTerminator ->
            after = skip_terminator active
            parse_nodes after acc
        _ ->
            if Str.starts_with active "/-" then
                after_slash = Str.drop_prefix active "/-"
                clean = skip_line_space after_slash
                when parse_node clean is
                    Err _ -> Err KdlFormatError
                    Ok { next } -> parse_nodes next acc
            else
                when parse_node active is
                    Err err -> Err err
                    Ok { node, next } ->
                        parse_nodes next (List.append acc node)

###############################################################################
# Node Body
###############################################################################
parse_node_body : Str, List Entry -> Result { entries : List Entry, children : List KdlNode, next : Str } KdlError
parse_node_body = |input, entries_acc|
    active = skip_node_space input
    if Str.starts_with active "/-" then
        when skip_slashdash_entry (Str.drop_prefix active "/-") is
            Err err -> Err err
            Ok next -> parse_node_body next entries_acc
    else
        when peek_token active is
            NodeTerminator ->
                Ok { entries: entries_acc, children: [], next: skip_terminator active }

            EndOfStream | ChildEnd ->
                Ok { entries: entries_acc, children: [], next: active }

            ChildStart ->
                when parse_child_block active is
                    Err err -> Err err
                    Ok { children, next } ->
                        Ok { entries: entries_acc, children, next: skip_node_space next }

            NumericLiteral | StringLiteral | IdentifierStart ->
                when read_entry active is
                    Err _ -> Err KdlFormatError
                    Ok { entry, next } ->
                        parse_node_body next (List.append entries_acc entry)

            _ -> Err KdlFormatError
