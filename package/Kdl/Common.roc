module [
    KdlValue,
    KdlNode,
    Entry,
]

KdlValue : [
    KdlStr Str,
    KdlNum F64,
    KdlBool Bool,
    KdlNull,
    KdlInf,
    KdlNegInf,
    KdlNaN,
]

Entry : {
    name : Result Str [None],
    value : KdlValue,
}

KdlNode : [KdlNodeRecord {
    name : Str,
    type_annotation : Result Str [None],
    entries : List Entry,
    children : List KdlNode,
}]

