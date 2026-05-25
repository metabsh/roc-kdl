# KDL for Roc
A [Roc](https://www.roc-lang.org/) package for parsing [KDL 2.0](https://kdl.dev/).


## Specification Compliance

KDL 2.0.0 conformance is tracked against the [official test suite](https://github.com/kdl-org/kdl/tree/main/tests/test_cases).
Test fixtures live in `tests/fixtures/kdl/test_cases/`. `scripts/generate_compliance.py` currently generates fixture tests from
inputs but does not verify against expected_kdl. A generator is used instead of a proper Roc app as a compiler bug breaks on
file IO when running a proper test.

| Category | Status | Fixtures | Notes |
|----------|--------|----------|-------|
| Empty / whitespace documents | ✅ | 3/3 | `empty.kdl`, `just_newline.kdl`, `just_space.kdl`, `empty_line_comment.kdl` |
| Bare identifier nodes | ✅ | 5/5 | `bare_ident_*.kdl`, `dash_dash.kdl`, `just_node_id.kdl` |
| Quoted strings | 🚧 | 2/7 | Quoted node names / prop keys / type annotations not handled |
| Numbers (decimal) | 🚧 | 9/11 | Type-annotated decimal args fail |
| Numbers (hex/octal/binary) | ✅ | 9/9 | |
| Numbers (keyword) | ✅ | 1/1 | `floating_point_keywords.kdl` — `#inf`, `#-inf`, `#nan` |
| Booleans & null | ✅ | 2/2 | `boolean_arg.kdl`, `boolean_prop.kdl` — `#true`/`#false`/`#null` |
| Properties (`key=value`) | ✅ | 11/11 | `arg_and_prop_same_name.kdl`, ordering, duplicate keys |
| Children blocks | 🚧 | 8/10 | 🚨 2 bugs: optional semicolon after children not handled |
| Multiple nodes | ✅ | 5/5 | Semicolons, newlines, `crlf_between_nodes.kdl` |
| Type annotations | 🚧 | 6/38 | Quoted/raw/blank/commented/spaced annotations fail |
| Line comments (`//`) | 🚧 | 7/12 | 🚨 1 bug: `comment_and_newline` boundary; slashdash+comment edge cases |
| Block comments (`/* */`) | ✅ | 10/10 | Includes nesting |
| Slashdash (`/-`) | 🚧 | 11/31 | Not yet implemented (passes are false-positives) |
| Line continuations (`\`) | 🚧 | 7/8 | Not yet implemented (passes are false-positives) |
| Multi-line strings (`"""`) | 🚧 | 15/20 | Not yet implemented (false passes) |
| Raw strings (`#"..."#`) | 🚧 | 5/10 | Not yet implemented (false passes) |
| Unicode whitespace | ✅ | 2/2 | `emoji.kdl`, `bare_emoji.kdl` |
| Identifier validation | 🚧 | 91/95 | 🚨 4 false-negatives: 1 spec interpretation, 3 multi-line/whitespace false passes |
| BOM / version marker | ✅ | 1/1 | `bom_initial.kdl`; `bom_later_fail.kdl` grouped in fail cases |
| Slashdash children | 🚧 | (in Slashdash) | `commented_child.kdl` |
| **Total** | — | **257 / 338** | 81 failures: 3 bugs, 4 false-negatives, 47 unimplemented, 27 annotation gaps |
