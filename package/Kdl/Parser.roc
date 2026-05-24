module [
    parse,
]

import Kdl.Stream as Stream
import Kdl.Lexer as Lexer
import Kdl.Common exposing [KdlNode, KdlError, Entry]

parse : Str -> Result (List KdlNode) KdlError
parse = |input|
    when parse_nodes input [] is
        Ok { nodes, next: _ } -> Ok nodes
        Err err -> Err err

parse_nodes : Str, List KdlNode -> Result { nodes : List KdlNode, next : Str } KdlError
parse_nodes = |input, acc|
    active = Lexer.skip_line_space input
    when Lexer.peek_token active is
        EndOfStream -> Ok { nodes: acc, next: active }
        ChildEnd -> Ok { nodes: acc, next: active }
        _ ->
            { stream: with_anno, annotation } =
                when Lexer.read_annotation active is
                    Ok { annotation_name, next } ->
                        { stream: next, annotation: Ok annotation_name }
                    Err _ ->
                        { stream: active, annotation: Err None }

            when Lexer.read_identifier with_anno is
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
                            parse_nodes next (List.append acc node)

parse_node_body : Str, List Entry -> Result { entries : List Entry, children : List KdlNode, next : Str } KdlError
parse_node_body = |input, entries_acc|
    active = Lexer.skip_node_space input
    when Lexer.peek_token active is
        NodeTerminator ->
            after = Stream.advance_one active
            Ok { entries: entries_acc, children: [], next: after }

        EndOfStream ->
            Ok { entries: entries_acc, children: [], next: active }

        ChildStart ->
            inside = Stream.advance_one active
            when parse_nodes inside [] is
                Err err -> Err err
                Ok { nodes: nested_children, next: after_children } ->
                    after_close = Stream.advance_one after_children
                    Ok { entries: entries_acc, children: nested_children, next: Lexer.skip_node_space after_close }

        NumericLiteral | StringLiteral ->
            when Lexer.read_value active is
                Err _ -> Err KdlFormatError
                Ok { value, next } ->
                    parse_node_body next (List.append entries_acc { name: Err None, value })

        IdentifierStart ->
            when Lexer.read_value active is
                Err _ -> Err KdlFormatError
                Ok { value, next } ->
                    when value is
                        KdlStr str_val ->
                            after_id = Lexer.skip_node_space next
                            when Stream.first_byte after_id is
                                Ok 61 ->
                                    after_equals = Lexer.skip_node_space (Stream.advance_one after_id)
                                    when Lexer.read_value after_equals is
                                        Err _ -> Err KdlFormatError
                                        Ok { value: prop_val, next: after_value } ->
                                            parse_node_body after_value (List.append entries_acc { name: Ok str_val, value: prop_val })
                                _ ->
                                    parse_node_body next (List.append entries_acc { name: Err None, value })
                        _ ->
                            parse_node_body next (List.append entries_acc { name: Err None, value })

        _ -> Err KdlFormatError
