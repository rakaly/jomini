//! The unit tests of the lossless parser.

use super::lexer::{
    STOP, TOKEN_COMMENT_BEFORE, TOKEN_ERROR, is_calc_stop, lex, lex_simd, lex_trivia,
    quote_hides_fields, run_lanes, scan_number,
};
use super::parser::{Builder, resolve_interpolations};
use super::*;
use crate::text::Operator;
use fearless_simd::{Level, Simd, dispatch, prelude::*, u8x16};
use quickcheck_macros::quickcheck;

/// The source of `parse` with its closing edit applied.
fn closed(parse: &Parse<'_>) -> Vec<u8> {
    apply_edits(parse.source(), parse.closing_edit()).unwrap()
}

fn error_kinds(tree: SyntaxTree<'_, '_>) -> Vec<SyntaxErrorKind> {
    tree.errors().iter().map(|e| e.kind).collect()
}

/// The CK3 reader directives of `game/reader_export/_reader_export.info`.
#[test]
fn reader_directives() {
    let source = b"@:define cool_block = { color = { 1 0 1 } trait = $trait$ }
@:insert cool_block = { trait = lunatic }
@:insert plain_block
@:register_variable size = 0.5
x = @:load_variable size
@:log \"Text\"
@:assert
@:unknown_thing
@@size = 1
";
    let parse = parse(source);
    assert!(parse.errors().is_empty(), "{:?}", parse.errors());
    rt(source);

    let items: Vec<_> = parse.ast().items().collect();
    let directive = |i: usize| items[i].as_reader_directive().unwrap();
    let arg = |i: usize| directive(i).argument().map(|t| t.text());

    assert_eq!(directive(0).kind(), DirectiveKind::Define);
    assert_eq!(arg(0), Some(&b"cool_block"[..]));
    let body = directive(0).value().unwrap().as_block().unwrap();
    assert_eq!(body.fields().count(), 2);

    assert_eq!(directive(1).kind(), DirectiveKind::Insert);
    assert_eq!(arg(1), Some(&b"cool_block"[..]));
    assert!(directive(1).value().unwrap().as_block().is_some());

    assert_eq!(directive(2).kind(), DirectiveKind::Insert);
    assert_eq!(arg(2), Some(&b"plain_block"[..]));
    assert!(directive(2).value().is_none());

    assert_eq!(directive(3).kind(), DirectiveKind::RegisterVariable);
    assert_eq!(arg(3), Some(&b"size"[..]));
    assert_eq!(directive(3).value().unwrap().to_f64(), Some(0.5));

    let field = items[4].as_field().unwrap();
    let Some(Value::ReaderDirective(load)) = field.value() else {
        panic!("expected a directive value");
    };
    assert_eq!(load.kind(), DirectiveKind::LoadVariable);
    assert_eq!(load.argument().unwrap().text(), b"size");

    assert_eq!(directive(5).kind(), DirectiveKind::Log);
    assert_eq!(arg(5), Some(&b"\"Text\""[..]));
    assert_eq!(directive(6).kind(), DirectiveKind::Assert);
    assert_eq!(arg(6), None);
    assert_eq!(directive(7).kind(), DirectiveKind::Unknown);

    let key = items[8].as_field().unwrap().key();
    assert_eq!(key.kind(), SyntaxKind::VariableKey);
    assert_eq!(key.text(), b"@@size");
}

/// `@:assert` takes no argument, so the next line is a separate item.
#[test]
fn assert_directive_takes_no_argument() {
    let parse = parse(b"@:assert\na = b");
    let items: Vec<_> = parse.ast().items().collect();
    assert_eq!(items.len(), 2);
    assert!(items[1].as_field().is_some());
}

/// Without reader variables, `@:name` and `@@name` are plain scalars.
#[test]
fn directives_need_the_variables_flavor() {
    let flavor = Flavor {
        variables: false,
        ..Flavor::default()
    };
    let parse = parse_with(b"@:insert a @@b = 1", flavor);
    let kinds: Vec<_> = parse.tree().tokens().map(|t| t.kind()).collect();
    assert!(!kinds.contains(&SyntaxKind::Directive));
    assert!(!kinds.contains(&SyntaxKind::VariableKey));
}

#[test]
fn format_keeps_reader_directives() {
    let source = b"@:insert  cool_block  =  { trait = lunatic }\nx = @:load_variable   size\n";
    let formatted = format(source);
    let reparsed = parse(&formatted);
    assert!(reparsed.errors().is_empty());
    let directives = reparsed
        .tree()
        .root()
        .descendants()
        .filter(|n| n.kind() == SyntaxKind::ReaderDirective)
        .count();
    assert_eq!(directives, 2, "{}", String::from_utf8_lossy(&formatted));
}

/// The fast [`Sink::scalar_field`] of the builder must make the same
/// nodes and flags as the four separate events.
#[quickcheck]
fn scalar_field_matches_events(kinds: (u8, u8), flags: (u8, u8, u8), nested: bool) -> bool {
    const KINDS: &[SyntaxKind] = &[
        SyntaxKind::Unquoted,
        SyntaxKind::Quoted,
        SyntaxKind::Variable,
        SyntaxKind::MacroParam,
    ];
    let token = |kind, flags| Token {
        start: 0,
        len: 1,
        kind,
        flags: flags & 3,
    };
    let key = token(KINDS[kinds.0 as usize % KINDS.len()], flags.0);
    let op = token(SyntaxKind::Operator, flags.1);
    let value = token(KINDS[kinds.1 as usize % KINDS.len()], flags.2);
    let mut actual = Builder::with_capacity(16);
    let mut expected = Builder::with_capacity(16);
    for builder in [&mut actual, &mut expected] {
        builder.start_node(SyntaxKind::Root);
        if nested {
            builder.start_node(SyntaxKind::Block);
            builder.token(token(SyntaxKind::OpenBrace, 0));
        }
    }
    actual.scalar_field(key, op, value);
    expected.start_node(SyntaxKind::Field);
    expected.token(key);
    expected.token(op);
    expected.token(value);
    expected.finish_node();
    for builder in [&mut actual, &mut expected] {
        while !builder.stack.is_empty() {
            builder.finish_node();
        }
    }
    let nodes = |builder: Builder| {
        builder
            .nodes
            .into_iter()
            .map(|node| {
                (
                    node.kind,
                    node.flags,
                    node.first_token,
                    node.token_end,
                    node.subtree_end,
                )
            })
            .collect::<Vec<_>>()
    };
    nodes(actual) == nodes(expected)
}

/// Assert the lexer output and the leaf iterator tile the input: the
/// significant tokens are in order and do not overlap, every gap is only
/// trivia, and the leaves are contiguous spans covering `[0, len)`.
fn assert_tiles(data: &[u8], flavor: Flavor) {
    let lexed = lex(data, flavor);
    let mut at = 0u32;
    for t in &lexed.tokens {
        assert!(t.start >= at, "overlap at {} in {:?}", at, data);
        let mut gap = at;
        while gap < t.start {
            let (kind, len) = lex_trivia(data, gap, t.start);
            assert!(kind.is_trivia());
            gap += len;
        }
        at = t.end();
    }
    let tree = parse_with(data, flavor);
    let mut at = TextSize::new(0);
    for t in tree.tokens() {
        assert_eq!(
            t.text_range().start(),
            at,
            "gap/overlap at {} in {:?}",
            at,
            data
        );
        at = t.text_range().end();
    }
    assert_eq!(
        at.to_usize(),
        data.len(),
        "tokens do not reach end of input"
    );
}

/// Check the navigation API against the iterators: each query must agree
/// with a walk over `tokens()`, `children()`, and the node tape.
fn assert_navigation(data: &[u8], flavor: Flavor) {
    let parse = parse_with(data, flavor);
    let tree = parse.tree();
    let root = tree.root();
    let leaves: Vec<SyntaxToken<'_, '_>> = tree.tokens().collect();

    // `next_token` and `prev_token` walk the same leaves as `tokens()`.
    let forward: Vec<_> = std::iter::successors(root.first_token(), |t| t.next_token()).collect();
    assert_eq!(forward, leaves, "next_token in {data:?}");
    let mut backward: Vec<_> =
        std::iter::successors(root.last_token(), |t| t.prev_token()).collect();
    backward.reverse();
    assert_eq!(backward, leaves, "prev_token in {data:?}");

    // Each byte belongs to exactly one leaf.
    for leaf in leaves.iter().filter(|t| !t.text_range().is_empty()) {
        let range = leaf.text_range();
        for offset in range.start().to_u32()..range.end().to_u32() {
            let at = tree.token_at_offset(TextSize::new(offset));
            assert_eq!(
                at.right_biased(),
                Some(*leaf),
                "offset {offset} in {data:?}"
            );
            if offset > range.start().to_u32() {
                assert_eq!(at, TokenAtOffset::Single(*leaf));
            }
        }
        assert_eq!(tree.covering_element(range), SyntaxElement::Token(*leaf));
    }
    assert_eq!(
        tree.token_at_offset(TextSize::of(data) + TextSize::new(1)),
        TokenAtOffset::None
    );

    // `descendants` and the `Enter` events of `preorder` give every node in
    // pre-order, and each `Enter` has a `Leave` in nested order.
    let nodes: Vec<_> = root.descendants().collect();
    assert_eq!(nodes.len(), tree.data.nodes.len());
    let mut stack = Vec::new();
    let mut entered = Vec::new();
    for event in root.preorder() {
        match event {
            WalkEvent::Enter(node) => {
                if let Some(&parent) = stack.last() {
                    assert_eq!(node.parent(), Some(parent));
                }
                stack.push(node);
                entered.push(node);
            }
            WalkEvent::Leave(node) => assert_eq!(stack.pop(), Some(node)),
        }
    }
    assert!(stack.is_empty());
    assert_eq!(entered, nodes);

    for node in nodes {
        let children: Vec<_> = node.child_nodes().collect();
        assert_eq!(node.first_child(), children.first().copied());
        assert_eq!(node.last_child(), children.last().copied());
        for pair in children.windows(2) {
            assert_eq!(pair[0].next_sibling(), Some(pair[1]));
            assert_eq!(pair[1].prev_sibling(), Some(pair[0]));
        }
        if let (Some(first), Some(last)) = (children.first(), children.last()) {
            assert_eq!(first.prev_sibling(), None);
            assert_eq!(last.next_sibling(), None);
        }
        assert_eq!(node.ancestors().next(), Some(node));
        assert_eq!(node.ancestors().last(), Some(root));

        // The leaves of a subtree are the leaves inside its range.
        let own: Vec<_> = node.tokens().collect();
        let inside: Vec<_> = leaves
            .iter()
            .copied()
            .filter(|t| node.text_range().contains_range(t.text_range()))
            .filter(|t| node == root || t.ancestors().any(|a| a == node))
            .collect();
        assert_eq!(own, inside, "tokens of {node:?} in {data:?}");
        assert_eq!(node.first_token(), own.first().copied());
        assert_eq!(node.last_token(), own.last().copied());
        let text: Vec<u8> = own.iter().flat_map(|t| t.text()).copied().collect();
        assert_eq!(text, node.text());

        // No child covers the range of the element that covers a node.
        let range = node.text_range();
        let cover = tree.covering_element(range);
        assert!(cover.text_range().contains_range(range));
        if let SyntaxElement::Node(cover) = cover {
            assert!(
                cover
                    .children()
                    .all(|c| !c.text_range().contains_range(range) || range.is_empty()),
                "covering_element of {range:?} in {data:?}"
            );
        }
    }
}

#[quickcheck]
fn prop_navigation(data: Vec<u8>) -> bool {
    assert_navigation(&data, Flavor::default());
    assert_navigation(&data, Flavor::hoi4());
    true
}

#[quickcheck]
fn prop_navigation_alphabet(data: Vec<u8>) -> bool {
    const ALPHABET: &[u8] = b"ab{}[]()=<>#\"@$ \t\n\r";
    let data: Vec<u8> = data
        .iter()
        .map(|b| ALPHABET[*b as usize % ALPHABET.len()])
        .collect();
    assert_navigation(&data, Flavor::default());
    assert_navigation(&data, Flavor::hoi4());
    true
}

#[test]
fn navigation_examples() {
    for source in [
        &b""[..],
        b"   ",
        b"\xef\xbb\xbf# c\n",
        b"a = { b = 1 c = { d = 2 } } # end",
        b"x = { \"unclosed",
        b"a = { [[x] b = $X$ ] @[1 + 2] }",
    ] {
        assert_navigation(source, Flavor::default());
    }
}

/// Parse, then assert byte-exact round-trip via the cursor traversal.
fn rt(data: &[u8]) {
    assert_tiles(data, Flavor::default());
    assert_tiles(data, Flavor::hoi4());
    let tree = parse(data);
    assert_eq!(
        tree.reconstruct(),
        data,
        "round-trip mismatch\n--- tree ---\n{}",
        tree.debug_tree()
    );
    assert_eq!(parse_with(data, Flavor::hoi4()).reconstruct(), data);
}

#[test]
fn round_trip_comprehensive() {
    let cases: &[&[u8]] = &[
        b"",
        b"   ",
        b"\n\t ;; ",
        b"foo = bar",
        b"foo=bar",
        b"open={1 2}",
        b"field1=-100.535",
        br#""foo"="bar" "3"="1444.11.11""#,
        br#"custom_name="THE !@#$%^&*( '\"LEGION\"')""#,
        b"foo{bar=qux}",
        b"foo=abc#def\nbar=qux",
        b"flavor_tur.8=yes",
        b"dashed-identifier=yes",
        b"province_id = event_target:agenda_province",
        b"mult = value:job_weights_research_modifier|JOB|head_researcher|",
        b"@planet_standard_scale = 11",
        b"window_name = @default_window_name",
        b"value=\"win\"; a=b",
        b"foo = 0.3;",
        b"a = 1; b = 2;; c = 3;",
        b";;;key = value;;;",
        b"age > 16",
        b"a==b c<=d e>=f g!=h i?=j k<l m>n",
        b"position = { @[1-leopard_x] @leopard_y }",
        b"my_calc = @[(-half-half)*half]",
        b"a = @[ tier + 1 ]",
        b"a = @[1+2*3]",
        b"a = @[ (1 - leopard_x) ]",
        b"a = @[10.0f / 2.5]",
        b"a = @[ -x ]",
        b"a = @[((@var))]",
        b"a = @[]",
        b"a = @[   ]",
        b"a = @[1 2 3]", // malformed: loose operands, still lossless
        b"a = @[1 +",    // unterminated calc
        b"a = @[)*/]",   // malformed operators, still lossless
        b"[[scaled_skill] code here ] [[!var_name] other code ]",
        b"stats={{id=0 type=general} {id=1 type=admiral}}",
        b"868416617618464 = { 11777 4108 { 5632 4187=1089 } 0=1089 }",
        b"weird = $MACRO$ ( ) * +",
        b"name = $TIER|capital$",
        b"color = rgb { 255 0 255 }",
        b"hsv_color = hsv360 { 180 10 60 }",
        b"\xef\xbb\xbf# BOM\nfoo = bar",
        b"name = \"J\xc3\xa5hk\xc3\xa5m\xc3\xa5hkke\"",
        b"\xa7GRichard\xa7",
        b"a =",
        b"a = }",
        b"} stray",
        b"{{{{}}}}",
        b"a = { b = c",
    ];
    for c in cases {
        rt(c);
    }
}

#[test]
fn round_trip_fixtures() {
    let fixtures: &[&[u8]] = &[
        include_bytes!("../../../tests/fixtures/meta.txt"),
        include_bytes!("../../../tests/fixtures/ck3-header.txt"),
        include_bytes!("../../../tests/fixtures/campaign_stats.txt"),
        include_bytes!("../../../tests/fixtures/string-array.txt"),
        include_bytes!("../../../tests/fixtures/nested-hidden-obj.txt"),
    ];
    for f in fixtures {
        rt(f);
    }
}

#[test]
fn quoted_string_continues_on_the_next_line() {
    let src = b"x=\"ab\ncd\"";
    for flavor in [Flavor::default(), Flavor::hoi4()] {
        let toks = lex(src, flavor).tokens;
        let quoted: Vec<_> = toks
            .iter()
            .filter(|t| t.kind == SyntaxKind::Quoted)
            .map(|t| &src[t.start as usize..(t.start + t.len) as usize])
            .collect();
        assert_eq!(quoted, [&b"\"ab\ncd\""[..]]);
        assert_eq!(parse_with(src, flavor).reconstruct(), src);
    }
}

#[test]
fn owned_parse() {
    fn assert_send_sync<T: Send + Sync + 'static>(_: &T) {}

    let parse = parse_owned(std::sync::Arc::<[u8]>::from(&b"a = { b = 1 }"[..]));
    assert_send_sync(&parse);
    #[cfg(not(target_family = "wasm"))]
    {
        let count = std::thread::spawn(move || parse.root().children().count())
            .join()
            .unwrap();
        assert_eq!(count, 1);
    }
    #[cfg(target_family = "wasm")]
    assert_eq!(parse.root().children().count(), 1);

    let parse = parse_owned(b"a = 1".to_vec());
    assert_eq!(parse.reconstruct(), b"a = 1");
    assert_eq!(parse.into_source(), b"a = 1");
}

#[test]
fn unclosed_quote() {
    let source = b"a = \"x\nb = 2";
    let tree = parse(source);
    let kinds: Vec<_> = tree.errors().iter().map(|e| e.kind).collect();
    assert_eq!(kinds, vec![SyntaxErrorKind::UnclosedQuote]);
    assert_eq!(tree.errors()[0].range, TextRange::span(4, 5));
    assert!(tree.root().has_error());
    assert_eq!(tree.reconstruct(), source);

    let quoted = tree
        .tokens()
        .find(|t| t.kind() == SyntaxKind::Quoted)
        .unwrap();
    assert!(quoted.has_error());

    // An escaped quote or a trailing backslash does not close the string.
    for source in [&b"a = \"x\\\""[..], b"a = \"x\\"] {
        let kinds: Vec<_> = parse(source).errors().iter().map(|e| e.kind).collect();
        assert_eq!(kinds, vec![SyntaxErrorKind::UnclosedQuote]);
    }

    // The error marks each node that holds the string.
    let tree = parse(b"a = { b = \"x");
    let kinds: Vec<_> = tree.errors().iter().map(|e| e.kind).collect();
    assert!(kinds.contains(&SyntaxErrorKind::UnclosedQuote));
    let block = tree
        .root()
        .child_nodes()
        .find_map(|n| n.child_nodes().find(|n| n.kind() == SyntaxKind::Block))
        .unwrap();
    assert!(block.has_error());

    // Closed strings have no error.
    assert!(parse(b"a = \"x\"").errors().is_empty());
    assert!(parse(b"a = \"x\\\"y\"").errors().is_empty());
}

#[test]
fn flavor_without_variables_treats_at_as_identifier() {
    // Without variables, `@` is an ordinary identifier byte, not a
    // variable marker.
    let toks = lex(b"@foo", Flavor::default().with_variables(false)).tokens;
    assert_eq!(toks.len(), 1);
    assert_eq!(toks[0].kind, SyntaxKind::Unquoted);

    let toks = lex(b"@foo", Flavor::default()).tokens;
    assert_eq!(toks[0].kind, SyntaxKind::Variable);
}

#[test]
fn missing_block_is_zero_width_and_closable() {
    let source = b"a = { b";
    let tree = parse(source);
    let error = tree
        .errors()
        .iter()
        .find(|error| error.missing_delimiter.is_some())
        .unwrap();
    let delimiter = error.missing_delimiter.unwrap();
    assert_eq!(delimiter.expected(), SyntaxKind::CloseBrace);
    assert_eq!(delimiter.missing, SyntaxKind::MissingCloseBrace);
    assert_eq!(delimiter.opening_range, TextRange::span(4, 5));
    assert_eq!(error.range, TextRange::empty(TextSize::of(source)));

    let missing = tree.tokens().find(|token| token.is_missing()).unwrap();
    assert_eq!(missing.kind(), SyntaxKind::MissingCloseBrace);
    assert_eq!(missing.text(), b"");
    assert_eq!(missing.text_range(), TextRange::empty(TextSize::of(source)));
    assert!(missing.has_error());
    assert_eq!(missing.parent().unwrap().kind(), SyntaxKind::Block);
    assert!(missing.parent().unwrap().has_error());
    assert!(tree.root().has_error());

    let block = tree
        .ast()
        .fields()
        .next()
        .unwrap()
        .value()
        .unwrap()
        .as_block()
        .unwrap();
    assert!(!block.is_closed_in_source());
    assert!(block.close_brace().is_none());
    assert_eq!(tree.closing_edit(), Some(&TextEdit::new(range(7, 7), "}")));
    assert_eq!(closed(&tree), b"a = { b}");
    assert!(parse(&closed(&tree)).errors().is_empty());
    assert_eq!(tree.reconstruct(), source);
    assert!(!format(source).contains(&b'}'));
}

#[test]
fn nested_repairs_are_inside_out_and_keep_siblings_clean() {
    let source = b"outer = { clean = {} bad = { value";
    let tree = parse(source);
    let missing: Vec<_> = tree
        .tokens()
        .filter(|token| token.is_missing())
        .map(|token| token.kind())
        .collect();
    assert_eq!(
        missing,
        [SyntaxKind::MissingCloseBrace, SyntaxKind::MissingCloseBrace]
    );
    assert_eq!(tree.closing_edit().unwrap().new_text, b"}}");
    assert_eq!(closed(&tree), b"outer = { clean = {} bad = { value}}");
    assert!(parse(&closed(&tree)).errors().is_empty());

    let outer = tree
        .ast()
        .fields()
        .next()
        .unwrap()
        .value()
        .unwrap()
        .as_block()
        .unwrap();
    let blocks: Vec<_> = outer
        .syntax()
        .child_nodes()
        .filter(|node| node.kind() == SyntaxKind::Field)
        .filter_map(|field| Field::cast(field)?.value()?.as_block())
        .collect();
    assert_eq!(blocks.len(), 2);
    assert!(!blocks[0].syntax().has_error());
    assert!(blocks[1].syntax().has_error());
}

#[test]
fn recovery_refuses_mismatches_and_interrupted_constructs() {
    for (source, kinds) in [
        (
            b"a = { b ]".as_slice(),
            [
                SyntaxErrorKind::UnmatchedCloseBracket,
                SyntaxErrorKind::UnclosedBlock,
            ],
        ),
        (
            b"a = { b = @[ (1]".as_slice(),
            [
                SyntaxErrorKind::UnclosedCalcParen,
                SyntaxErrorKind::UnclosedBlock,
            ],
        ),
    ] {
        let tree = parse(source);
        assert_eq!(error_kinds(tree.tree()), kinds, "{source:?}");
        assert!(
            tree.closing_edit().is_none(),
            "unexpected fix for {source:?}"
        );
    }

    let mismatched_block = parse(b"a = { b ]");
    let block = mismatched_block
        .ast()
        .fields()
        .next()
        .and_then(|field| field.value())
        .and_then(|value| value.as_block())
        .unwrap();
    assert!(block.syntax().has_error());

    let interrupted_calc = parse(b"a = { b = @[1 }");
    let calc = interrupted_calc
        .ast()
        .fields()
        .next()
        .and_then(|field| field.value())
        .and_then(|value| value.as_block())
        .and_then(|block| block.fields().next())
        .and_then(|field| field.value())
        .and_then(|value| value.as_calc())
        .unwrap();
    assert!(calc.syntax().has_error());
    assert_eq!(
        error_kinds(interrupted_calc.tree()),
        [SyntaxErrorKind::UnclosedCalc]
    );
    assert!(interrupted_calc.closing_edit().is_none());

    let ambiguous_bracket = parse(b"a = { [");
    assert!(ambiguous_bracket.closing_edit().is_none());
}

#[test]
fn trailing_comments_and_line_endings_get_safe_closing_edits() {
    let cases = [
        (b"a = { x # tail".as_slice(), b"\n}".as_slice()),
        (b"a = { x # tail\n".as_slice(), b"}".as_slice()),
        (b"a = {\r\n x # tail".as_slice(), b"\r\n}".as_slice()),
        (b"a = { x\r\n".as_slice(), b"}".as_slice()),
    ];
    for (source, replacement) in cases {
        let tree = parse(source);
        assert_eq!(tree.closing_edit().unwrap().new_text, replacement);
        assert!(
            parse(&closed(&tree)).errors().is_empty(),
            "source={source:?}"
        );
    }

    for source in [
        b"a = { \"unterminated".as_slice(),
        b"a = { \"escaped\\\"".as_slice(),
    ] {
        let tree = parse(source);
        assert!(tree.closing_edit().is_none());
        let delimiter = tree.errors().iter().find_map(|e| e.missing_delimiter);
        assert!(!delimiter.unwrap().safe);
    }
}

/// The missing delimiters of a source share one edit at the end of the
/// source, which closes the constructs from the inside out.
#[test]
fn missing_delimiters_share_one_closing_edit() {
    let cases: [(&[u8], usize, &str); 4] = [
        (b"a = { b = 1", 1, "}"),
        (b"a = { b = { c = 1", 2, "}}"),
        (b"x = @[(1 + 2", 2, ")]"),
        // The edited source keeps the other errors of the source.
        (b"a = { b =", 1, "}"),
    ];
    for (source, delimiters, text) in cases {
        let tree = parse(source);
        let count = tree
            .errors()
            .iter()
            .filter(|e| e.missing_delimiter.is_some())
            .count();
        assert_eq!(count, delimiters, "{source:?}");
        let edit = tree.closing_edit().unwrap();
        assert_eq!(
            *edit,
            TextEdit::new(range(source.len() as u32, source.len() as u32), text)
        );
        assert!(std::ptr::eq(edit, tree.closing_edit().unwrap()));
    }
}

#[test]
fn unclosed_parameter_with_inner_bracket_is_not_closed() {
    let tree = parse(b"[[x] [ } ]");
    let parameter = tree
        .ast()
        .items()
        .find_map(|item| item.as_parameter())
        .unwrap();
    assert!(!parameter.is_closed_in_source(), "{}", tree.debug_tree());

    let tree = parse(b"[[!x] [ } ]");
    let parameter = tree
        .ast()
        .items()
        .find_map(|item| match item {
            Item::UndefinedParameter(value) => Some(value),
            _ => None,
        })
        .unwrap();
    assert!(!parameter.is_closed_in_source(), "{}", tree.debug_tree());

    let tree = parse(b"[[x] a = b ]");
    let parameter = tree
        .ast()
        .items()
        .find_map(|item| item.as_parameter())
        .unwrap();
    assert!(parameter.is_closed_in_source());
}

#[test]
fn incomplete_parameters_and_code_payloads_preserve_typed_content() {
    let parameter = parse(b"[[name] value");
    let parameter_node = parameter
        .ast()
        .items()
        .find_map(|item| match item {
            Item::Parameter(value) => Some(value),
            _ => None,
        })
        .unwrap();
    assert_eq!(parameter_node.values().count(), 1);
    assert!(parameter_node.close_bracket().is_none());
    assert_eq!(parameter.closing_edit().unwrap().new_text, b"]");
    assert!(parse(&closed(&parameter)).errors().is_empty());

    let header = parse(b"[[name");
    assert!(
        header
            .ast()
            .items()
            .any(|item| matches!(item, Item::Parameter(_)))
    );
    assert_eq!(header.closing_edit().unwrap().new_text, b"]]");
    assert!(parse(&closed(&header)).errors().is_empty());

    let one = parse(b"code [[value]");
    assert_eq!(one.closing_edit().unwrap().new_text, b"]");
    assert_eq!(one.tokens().filter(|token| token.is_missing()).count(), 1);
    assert!(parse(&closed(&one)).errors().is_empty());

    let both = parse(b"code [[value");
    assert_eq!(both.closing_edit().unwrap().new_text, b"]]");
    assert_eq!(both.tokens().filter(|token| token.is_missing()).count(), 2);
    assert!(parse(&closed(&both)).errors().is_empty());
}

#[test]
fn nested_calc_repairs_include_parentheses_and_calc_close() {
    let source = b"a = { x = @[ (1";
    let tree = parse(source);
    let kinds: Vec<_> = tree
        .tokens()
        .filter(|token| token.is_missing())
        .map(|token| token.kind())
        .collect();
    assert_eq!(
        kinds,
        [
            SyntaxKind::MissingCloseParen,
            SyntaxKind::MissingCalcClose,
            SyntaxKind::MissingCloseBrace
        ]
    );
    assert_eq!(tree.closing_edit().unwrap().new_text, b")]}");
    assert!(parse(&closed(&tree)).errors().is_empty());
    assert_eq!(tree.reconstruct(), source);
}

#[test]
fn interpolation_recovery_is_typed_and_lossless() {
    let source = b"value = [ROOT.GetName";
    let tree = parse(source);
    let interpolation = tree
        .ast()
        .fields()
        .next()
        .and_then(|field| field.value())
        .and_then(|value| value.as_interpolation())
        .unwrap();

    assert!(interpolation.syntax().has_error());
    assert!(!interpolation.is_closed_in_source());
    assert_eq!(interpolation.syntax().text(), b"[ROOT.GetName");
    assert!(
        interpolation
            .body()
            .any(|element| element.kind() == SyntaxKind::Unquoted)
    );
    assert_eq!(tree.closing_edit().unwrap().new_text, b"]");
    assert_eq!(closed(&tree), b"value = [ROOT.GetName]");
    assert!(parse(&closed(&tree)).errors().is_empty());
}

#[test]
fn unmatched_close_brace_is_a_bogus_node() {
    let tree = parse(b"x } y");
    assert_eq!(
        error_kinds(tree.tree()),
        [SyntaxErrorKind::UnmatchedCloseBrace]
    );
    let bogus: Vec<_> = tree
        .root()
        .child_nodes()
        .filter(|n| n.kind() == SyntaxKind::Bogus)
        .collect();
    assert_eq!(bogus.len(), 1);
    assert_eq!(bogus[0].text(), b"}");
    assert!(bogus[0].has_error());
    assert!(tree.root().has_error());
}

#[test]
fn bang_equal_splits_from_a_scalar() {
    let tree = parse(b"a!=1");
    let field = tree.ast().fields().next().unwrap();
    assert_eq!(field.key().text(), b"a");
    assert_eq!(field.op(), Some(Operator::NotEqual));
    assert_eq!(tree.reconstruct(), b"a!=1");
}

#[test]
fn question_equal_splits_from_a_scalar() {
    // Vic3 `common/objective_subgoals` has `c:GBR?= THIS`.
    let tree = parse(b"c:GBR?= THIS");
    let field = tree.ast().fields().next().unwrap();
    assert_eq!(field.key().text(), b"c:GBR");
    assert_eq!(field.op(), Some(Operator::Exists));
    assert_eq!(tree.reconstruct(), b"c:GBR?= THIS");

    // A `?` that no `=` follows stays in the scalar, as in HoI4 `cost = a?50`.
    let tree = parse_with(b"cost = a?50", Flavor::hoi4());
    let field = tree.ast().fields().next().unwrap();
    assert_eq!(
        field.value().unwrap().as_scalar().unwrap().as_bytes(),
        b"a?50"
    );
}

#[test]
fn lone_open_bracket_has_a_diagnostic() {
    for source in [&b"desc = ["[..], b"desc = [ ", b"a = { desc = [ }"] {
        let tree = parse(source);
        let kinds: Vec<_> = tree.errors().iter().map(|e| e.kind).collect();
        assert_eq!(kinds, vec![SyntaxErrorKind::UnexpectedOpenBracket]);
        assert!(tree.errors()[0].missing_delimiter.is_none());
        assert!(tree.closing_edit().is_none());
        assert_eq!(tree.reconstruct(), source);
    }

    // A `[` in a parameter body is an ordinary bracket.
    assert!(parse(b"[[x] a = [ ] ]").errors().is_empty());
}

#[test]
fn stray_close_bracket_keeps_block_structure() {
    let source = b"a = { pos = { 0.5 0.9] } scale = { 1 } } b = c";
    let tree = parse(source);
    let kinds: Vec<_> = tree.errors().iter().map(|e| e.kind).collect();
    assert_eq!(kinds, vec![SyntaxErrorKind::UnmatchedCloseBracket]);
    let fields: Vec<_> = tree.ast().fields().collect();
    assert_eq!(fields.len(), 2);
    let block = fields[0].value().unwrap().as_block().unwrap();
    assert!(block.is_closed_in_source());
    assert_eq!(block.fields().count(), 2);
    assert_eq!(tree.reconstruct(), source);

    // The `]` refuses the virtual `}` at EOF.
    let tree = parse(b"a = { b ] c");
    assert!(tree.closing_edit().is_none());
}

#[test]
fn close_bracket_key_is_a_field() {
    let source = b"active_idea_groups = { ]=0 defensive_ideas=2 }";
    let tree = parse(source);
    assert!(tree.errors().is_empty());
    let field = tree.ast().fields().next().unwrap();
    let block = field.value().unwrap().as_block().unwrap();
    let keys: Vec<_> = block.fields().map(|f| f.key().text().to_vec()).collect();
    assert_eq!(keys, vec![b"]".to_vec(), b"defensive_ideas".to_vec()]);
    assert_eq!(tree.reconstruct(), source);
}

#[test]
#[cfg_attr(miri, ignore)] // the deep input is slow under miri
fn deeply_nested_blocks_do_not_overflow() {
    // Analogous to TextTape's `test_too_heavily_nested`: tens of thousands
    // of open braces would blow a naive recursive descent's stack. The
    // depth guard caps recursion, records a diagnostic, and the tree still
    // round-trips byte-for-byte.
    let mut data = Vec::new();
    data.extend_from_slice(b"foo=");
    data.resize(data.len() + 100_000, b'{');
    let tree = parse(&data);
    assert_eq!(tree.reconstruct(), data);
    assert!(
        tree.errors()
            .iter()
            .any(|e| e.kind == SyntaxErrorKind::BlockDepthExceeded)
    );
}

#[test]
#[cfg_attr(miri, ignore)]
fn deeply_nested_balanced_blocks_round_trip() {
    // Balanced this time, so flattening must still pair every brace.
    let mut data = vec![b'{'; 50_000];
    data.resize(100_000, b'}');
    let tree = parse(&data);
    assert_eq!(tree.reconstruct(), data);
}

#[test]
#[cfg_attr(miri, ignore)]
fn deeply_nested_calc_parens_do_not_overflow() {
    // Calc expressions recurse through `parse_operand`/`parse_expr`; the
    // same guard covers `@[((((…))))]`.
    let mut data = Vec::new();
    data.extend_from_slice(b"x = @[");
    data.resize(data.len() + 100_000, b'(');
    data.push(b'1');
    data.resize(data.len() + 100_000, b')');
    data.push(b']');
    let tree = parse(&data);
    assert_eq!(tree.reconstruct(), data);
    assert!(
        tree.errors()
            .iter()
            .any(|e| e.kind == SyntaxErrorKind::CalcDepthExceeded)
    );
}

#[test]
#[cfg_attr(miri, ignore)]
fn deeply_nested_calc_unary_do_not_overflow() {
    // Unary `-` also recurses through `parse_operand`.
    let mut data = Vec::new();
    data.extend_from_slice(b"x = @[");
    data.resize(data.len() + 100_000, b'-');
    data.extend_from_slice(b"x]");
    let tree = parse(&data);
    assert_eq!(tree.reconstruct(), data);
    assert!(
        tree.errors()
            .iter()
            .any(|e| e.kind == SyntaxErrorKind::CalcDepthExceeded)
    );
}

#[test]
#[cfg_attr(miri, ignore)]
fn deeply_nested_calc_binary_chain_does_not_overflow() {
    // A flat chain `@[1+1+1+…]` is folded iteratively, but yields a
    // left-nested BinaryExpr tree of depth = chain length that emit_calc and
    // the tree's recursive Drop walk — so the fold cap must bound it too.
    let mut data = Vec::new();
    data.extend_from_slice(b"x = @[1");
    for _ in 0..100_000 {
        data.extend_from_slice(b"+1");
    }
    data.push(b']');
    let tree = parse(&data);
    assert_eq!(tree.reconstruct(), data);
    assert!(
        tree.errors()
            .iter()
            .any(|e| e.kind == SyntaxErrorKind::CalcDepthExceeded)
    );
    // A pure `*` chain folds the same way.
    let mut data = Vec::new();
    data.extend_from_slice(b"x = @[1");
    for _ in 0..100_000 {
        data.extend_from_slice(b"*1");
    }
    data.push(b']');
    assert_eq!(parse(&data).reconstruct(), data);
}

#[test]
fn macro_param_is_a_scalar_key_and_value() {
    // `$P$` counts as a scalar everywhere (template files use it as a key,
    // value, and array element), so `$P$ = $Q$` is a Field.
    let tree = parse(b"$P$ = $Q$");
    let field = tree.ast().fields().next().expect("a Field");
    assert_eq!(field.key().text(), b"$P$");
    assert_eq!(
        field.value().unwrap().as_scalar().unwrap().as_bytes(),
        b"$Q$"
    );
}

#[test]
fn ast_field_key_op_value() {
    let tree = parse(b"name = \"Ragusa\"");
    let field = tree.ast().fields().next().unwrap();
    assert_eq!(field.key().text(), b"name");
    assert_eq!(field.op(), Some(Operator::Equal));
    let value = field.value().unwrap();
    // Quoted value: as_scalar() strips the surrounding quotes.
    assert_eq!(value.as_scalar().unwrap().as_bytes(), b"Ragusa");
}

#[test]
fn ast_block_items_fields_and_values() {
    let tree = parse(b"obj = { a = 1 b = 2 } arr = { x y z }");
    let mut fields = tree.ast().fields();
    let obj = fields.next().unwrap().value().unwrap().as_block().unwrap();
    assert_eq!(obj.fields().count(), 2);
    assert!(obj.values().next().is_none());
    assert!(!obj.is_empty());

    let arr = fields.next().unwrap().value().unwrap().as_block().unwrap();
    let elems: Vec<_> = arr
        .values()
        .filter_map(|v| v.as_scalar())
        .map(|s| s.as_bytes().to_vec())
        .collect();
    assert_eq!(elems, [b"x", b"y", b"z"]);
    assert!(arr.fields().next().is_none());
}

#[test]
fn ast_block_with_only_trivia_is_empty() {
    let tree = parse(b"a = {} b = { # note\n } c = { = }");
    let blocks: Vec<_> = tree
        .ast()
        .fields()
        .map(|f| f.value().unwrap().as_block().unwrap())
        .collect();
    assert!(blocks[0].is_empty());
    assert!(blocks[1].is_empty());
    // A stray operator is an `Item::Other`, so the block is not empty.
    assert!(!blocks[2].is_empty());
}

#[test]
fn ast_headered_block_is_generic() {
    let tree = parse(b"color = rgb { 1 0.5 0 } custom = tag { x = y }");
    let mut fields = tree.ast().fields();

    let rgb = fields
        .next()
        .unwrap()
        .value()
        .unwrap()
        .as_headered_block()
        .unwrap();
    assert_eq!(rgb.header().text(), b"rgb");
    let channels: Vec<_> = rgb.block().values().map(|value| value.to_f64()).collect();
    assert_eq!(channels, [Some(1.0), Some(0.5), Some(0.0)]);

    let custom = fields
        .next()
        .unwrap()
        .value()
        .unwrap()
        .as_headered_block()
        .unwrap();
    assert_eq!(custom.header().text(), b"tag");
    assert_eq!(custom.block().fields().count(), 1);
}

#[test]
fn ast_reader_variable_definition_with_calc_value() {
    let tree = parse(b"@half = @[1 / 2]");
    let field = tree.ast().fields().next().unwrap();

    let key = field.key();
    assert_eq!(key.kind(), SyntaxKind::Variable);
    assert_eq!(key.text(), b"@half");
    assert_eq!(field.op(), Some(Operator::Equal));

    let calc = field.value().unwrap().as_calc().unwrap();
    let Some(Expr::Binary(div)) = calc.expr() else {
        panic!("expected a binary calculation");
    };
    assert_eq!(div.op(), BinOp::Div);
    assert!(matches!(div.lhs(), Some(Expr::Number(_))));
    assert!(matches!(div.rhs(), Some(Expr::Number(_))));
}

#[test]
fn ast_headered_block_with_variables_and_calcs() {
    let tree = parse(b"color = rgb { @red @[green / 2] 0 }");
    let headered = tree
        .ast()
        .fields()
        .next()
        .unwrap()
        .value()
        .unwrap()
        .as_headered_block()
        .unwrap();
    assert_eq!(headered.header().text(), b"rgb");

    let values: Vec<_> = headered.block().values().collect();
    let Value::Scalar(red) = values[0] else {
        panic!("expected a variable scalar");
    };
    assert_eq!(red.kind(), SyntaxKind::Variable);
    assert_eq!(red.text(), b"@red");

    let Value::Calc(calc) = values[1] else {
        panic!("expected a calculation");
    };
    let Some(Expr::Binary(div)) = calc.expr() else {
        panic!("expected a binary calculation");
    };
    assert!(matches!(div.lhs(), Some(Expr::Ident(_))));
    assert!(matches!(div.rhs(), Some(Expr::Number(_))));

    let Value::Scalar(zero) = values[2] else {
        panic!("expected a scalar channel");
    };
    assert_eq!(zero.text(), b"0");
}

#[test]
fn ast_calc_expr_structure() {
    let tree = parse(b"x = @[1 + tier * 2]");
    let calc = tree
        .ast()
        .fields()
        .next()
        .unwrap()
        .value()
        .unwrap()
        .as_calc()
        .unwrap();
    // 1 + (tier * 2): top is a binary `+`.
    let Some(Expr::Binary(add)) = calc.expr() else {
        panic!("expected a binary expression");
    };
    assert_eq!(add.op(), BinOp::Add);
    assert!(matches!(add.lhs(), Some(Expr::Number(_))));
    // rhs is the tighter `tier * 2`.
    let Some(Expr::Binary(mul)) = add.rhs() else {
        panic!("expected a nested binary expression");
    };
    assert_eq!(mul.op_token().text(), b"*");
    assert!(matches!(mul.lhs(), Some(Expr::Ident(_))));
    assert!(matches!(mul.rhs(), Some(Expr::Number(_))));
}

#[test]
fn ast_calc_unary_and_paren() {
    let tree = parse(b"x = @[ -(a) ]");
    let calc = tree
        .ast()
        .fields()
        .next()
        .unwrap()
        .value()
        .unwrap()
        .as_calc()
        .unwrap();
    let Some(Expr::Unary(neg)) = calc.expr() else {
        panic!("expected a unary expression");
    };
    assert_eq!(neg.op(), UnaryOp::Neg);
    let Some(Expr::Paren(paren)) = neg.operand() else {
        panic!("expected a parenthesized operand");
    };
    assert!(matches!(paren.inner(), Some(Expr::Ident(_))));
}

/// Write an expression with each operation in parentheses, a group in
/// brackets, and `?` for a part that the strict accessors do not give.
fn show_expr(expr: Option<Expr>) -> String {
    match expr {
        None => "?".to_string(),
        Some(Expr::Number(token) | Expr::Ident(token)) => {
            String::from_utf8(token.text().to_vec()).unwrap()
        }
        Some(Expr::Binary(binary)) => format!(
            "({} {} {})",
            show_expr(binary.lhs()),
            binary.op().symbol(),
            show_expr(binary.rhs())
        ),
        Some(Expr::Unary(unary)) => {
            format!("({}{})", unary.op().symbol(), show_expr(unary.operand()))
        }
        Some(Expr::Paren(paren)) => format!("[{}]", show_expr(paren.inner())),
    }
}

#[test]
fn calc_accessors_give_only_the_expected_parts() {
    let cases: [(&[u8], &str); 12] = [
        (b"x = @[1 + 2 * 3]", "(1 + (2 * 3))"),
        (b"x = @[-(a) / b]", "((-[a]) / b)"),
        (b"x = @[]", "?"),
        (b"x = @[1 2]", "?"),
        (b"x = @[min(1, 2)]", "?"),
        (b"x = @[* * 2]", "(? * 2)"),
        (b"x = @[1 +]", "(1 + ?)"),
        (b"x = @[-]", "(-?)"),
        (b"x = @[- *]", "(-?)"),
        (b"x = @[()]", "[?]"),
        (b"x = @[(1 2)]", "?"),
        (b"x = @[(1", "[1]"),
    ];
    for (source, expected) in cases {
        let tree = parse(source);
        let calc = tree.ast().fields().next().unwrap().value().unwrap();
        let calc = calc.as_calc().unwrap();
        assert_eq!(
            show_expr(calc.expr()),
            expected,
            "{}",
            String::from_utf8_lossy(source)
        );
    }
}

#[test]
fn ast_parameter_nodes_preserve_body() {
    let source = b"generate_advisor = { [[scaled_skill] a = { b = c } ] }";
    let tree = parse(source);
    assert!(tree.errors().is_empty());
    assert_eq!(tree.reconstruct(), source);

    let block = tree
        .ast()
        .fields()
        .next()
        .unwrap()
        .value()
        .unwrap()
        .as_block()
        .unwrap();
    let entries = block.items().collect::<Vec<_>>();
    let [Item::Parameter(parameter)] = entries.as_slice() else {
        panic!("expected one parameter entry");
    };
    assert_eq!(parameter.syntax().kind(), SyntaxKind::Parameter);
    assert_eq!(
        parameter.syntax().text(),
        b"[[scaled_skill] a = { b = c } ]"
    );
    assert_eq!(parameter.name().unwrap().text(), b"scaled_skill");
    assert_eq!(parameter.fields().count(), 1);
    assert_eq!(parameter.fields().next().unwrap().key().text(), b"a");
}

#[test]
fn ast_undefined_parameter_node_is_distinct() {
    let source = b"[[!scaled_skill] a = b ]";
    let tree = parse(source);
    assert!(tree.errors().is_empty());
    assert_eq!(tree.reconstruct(), source);

    let entries = tree.ast().items().collect::<Vec<_>>();
    let [Item::UndefinedParameter(parameter)] = entries.as_slice() else {
        panic!("expected one undefined parameter entry");
    };
    assert_eq!(parameter.syntax().kind(), SyntaxKind::UndefinedParameter);
    assert_eq!(parameter.name().unwrap().text(), b"scaled_skill");
    assert_eq!(parameter.fields().next().unwrap().key().text(), b"a");
}

#[test]
fn ast_empty_parameter_node_is_valid() {
    let source = b"[[dip_reward]\n]";
    let tree = parse(source);
    assert!(tree.errors().is_empty());
    assert_eq!(tree.reconstruct(), source);

    let entries = tree.ast().items().collect::<Vec<_>>();
    let [Item::Parameter(parameter)] = entries.as_slice() else {
        panic!("expected one empty parameter entry");
    };
    assert_eq!(parameter.name().unwrap().text(), b"dip_reward");
    assert_eq!(parameter.items().count(), 0);
}

#[test]
fn interpolation_nodes_are_distinct_from_parameters() {
    let source = b"text = [PdxAccount.GetLastLinkErrorLocalized]\nname = [ROOT.GetName]\n";
    let tree = parse(source);
    assert!(tree.errors().is_empty());
    assert_eq!(tree.reconstruct(), source);

    let fields = tree.ast().fields().collect::<Vec<_>>();
    let first = fields[0].value().unwrap().as_interpolation().unwrap();
    assert_eq!(first.syntax().kind(), SyntaxKind::Interpolation);
    assert_eq!(
        first.syntax().text(),
        b"[PdxAccount.GetLastLinkErrorLocalized]"
    );
    assert_eq!(first.body().count(), 1);

    let second = fields[1].value().unwrap().as_interpolation().unwrap();
    assert_eq!(second.syntax().text(), b"[ROOT.GetName]");
}

#[test]
fn eu5_code_payload_is_not_a_parameter() {
    let source = b"template example { code [[\n\tvalue = { name = wall }\n]] }";
    let tree = parse(source);
    assert!(tree.errors().is_empty());
    assert_eq!(tree.reconstruct(), source);

    let block = tree
        .ast()
        .items()
        .find_map(|item| match item {
            // `example { ... }` is a field with no operator.
            Item::Field(field) => field.value()?.as_block(),
            _ => None,
        })
        .unwrap();
    let entries = block.items().collect::<Vec<_>>();
    let [Item::Code(code)] = entries.as_slice() else {
        panic!("expected one code payload");
    };
    assert_eq!(code.syntax().kind(), SyntaxKind::Code);
    assert_eq!(code.keyword().unwrap().text(), b"code");
    assert_eq!(
        code.syntax().text(),
        b"code [[\n\tvalue = { name = wall }\n]]"
    );
    assert_eq!(code.fields().next().unwrap().key().text(), b"value");
    assert!(
        code.fields()
            .next()
            .unwrap()
            .value()
            .unwrap()
            .as_block()
            .is_some()
    );

    let compact = parse(b"template example { code = [[name] body ]] }");
    assert!(compact.errors().is_empty());
    let compact_block = compact
        .ast()
        .items()
        .find_map(|item| match item {
            // `example { ... }` is a field with no operator.
            Item::Field(field) => field.value()?.as_block(),
            _ => None,
        })
        .unwrap();
    let code = compact_block.fields().next().unwrap();
    assert_eq!(code.key().text(), b"code");
    assert_eq!(code.op(), Some(Operator::Equal));
    let payload = code.value().unwrap().as_code().unwrap();
    assert!(payload.keyword().is_none());
    assert_eq!(payload.fields().count(), 0);
}

#[test]
fn eu5_code_payload_allows_empty_bodies() {
    for source in [b"code [[]]".as_slice(), b"code [[ ]]".as_slice()] {
        let tree = parse(source);
        assert!(tree.errors().is_empty());
        assert_eq!(tree.reconstruct(), source);
        let entries = tree.ast().items().collect::<Vec<_>>();
        let [Item::Code(code)] = entries.as_slice() else {
            panic!("expected a code payload");
        };
        assert_eq!(code.items().count(), 0);
    }

    for source in [b"code = [[]]".as_slice(), b"code = [[ ]]".as_slice()] {
        let tree = parse(source);
        assert!(tree.errors().is_empty());
        assert_eq!(tree.reconstruct(), source);
        let field = tree.ast().fields().next().unwrap();
        assert_eq!(field.value().unwrap().as_code().unwrap().items().count(), 0);
    }
}

#[test]
fn eu5_code_payload_handles_nested_braces_and_brackets() {
    let source = b"code [[ value = { nested = { link = [ROOT.GetName] } } ]]";
    let tree = parse(source);
    assert!(tree.errors().is_empty());
    assert_eq!(tree.reconstruct(), source);

    let entries = tree.ast().items().collect::<Vec<_>>();
    let [Item::Code(code)] = entries.as_slice() else {
        panic!("expected a code payload");
    };
    let nested = code
        .fields()
        .next()
        .unwrap()
        .value()
        .unwrap()
        .as_block()
        .unwrap()
        .fields()
        .next()
        .unwrap()
        .value()
        .unwrap()
        .as_block()
        .unwrap();
    let interpolation = nested
        .fields()
        .next()
        .unwrap()
        .value()
        .unwrap()
        .as_interpolation()
        .unwrap();
    assert_eq!(interpolation.syntax().text(), b"[ROOT.GetName]");
}

#[test]
fn eu5_code_payload_recovery_keeps_following_block_delimiter() {
    let source = b"outer = { code [[value = yes }";
    let tree = parse(source);
    assert_eq!(tree.reconstruct(), source);
    assert_eq!(error_kinds(tree.tree()), [SyntaxErrorKind::UnclosedCode]);
    let outer = tree
        .ast()
        .fields()
        .next()
        .unwrap()
        .value()
        .unwrap()
        .as_block()
        .unwrap();
    assert!(
        outer
            .syntax()
            .child_tokens()
            .any(|token| token.kind() == SyntaxKind::CloseBrace)
    );
}

#[test]
fn eu5_code_payload_recovery_handles_eof() {
    let sources = [
        b"code [[value = yes".as_slice(),
        b"code = [[value = yes".as_slice(),
    ];
    for source in sources {
        let tree = parse(source);
        assert_eq!(tree.reconstruct(), source);
        assert_eq!(
            error_kinds(tree.tree()),
            [
                SyntaxErrorKind::CodeMissingFirstBracket,
                SyntaxErrorKind::CodeMissingSecondBracket,
            ]
        );
        assert_eq!(tree.closing_edit().unwrap().new_text, b"]]");
    }
}

#[test]
fn close_bracket_is_not_a_field_value() {
    for source in [b"[[p] a = ]\nb = 1".as_slice(), b"x = { code [[ a = ]] }"] {
        let tree = parse(source);
        assert_eq!(tree.reconstruct(), source);
        assert_eq!(error_kinds(tree.tree()), [SyntaxErrorKind::MissingValue]);
    }

    let tree = parse(b"[[p] a = ]\nb = 1");
    let parameter = tree
        .root()
        .child_nodes()
        .find(|node| node.kind() == SyntaxKind::Parameter)
        .unwrap();
    assert_eq!(parameter.text(), b"[[p] a = ]");
}

#[test]
fn operator_without_value_is_an_error() {
    let cases: [(&[u8], std::ops::Range<usize>); 5] = [
        (b"foo=bar\nline=", 12..13),
        (b"a = { b >= } c = 1", 8..10),
        (b"a = # note\n", 2..3),
        (b"@:define x =", 11..12),
        (b"a = { b = c d = }", 14..15),
    ];
    for (source, range) in cases {
        let tree = parse(source);
        assert_eq!(tree.reconstruct(), source);
        let [error] = tree.errors() else {
            panic!("expected one error: {:?}", tree.errors());
        };
        assert_eq!(error.kind, SyntaxErrorKind::MissingValue);
        assert_eq!(error.range.to_range(), range);
        assert_eq!(error.missing_delimiter, None);
    }

    let tree = parse(b"a = { b = } c = 1");
    let [error] = tree.errors() else {
        panic!("expected one error: {:?}", tree.errors());
    };
    assert_eq!(error.to_string(), "operator has no value at 8..9");
    let a = tree.ast().get(b"a").unwrap();
    assert!(a.syntax().has_error());
    let b = a.value().unwrap().as_block().unwrap().get(b"b").unwrap();
    assert_eq!(b.op(), Some(Operator::Equal));
    assert!(b.value().is_none());
    let c = tree.ast().get(b"c").unwrap();
    assert!(!c.syntax().has_error());
    assert_eq!(c.value().unwrap().to_u64(), Some(1));

    // An operator that follows an operator is the value.
    assert!(parse(b"OPERATOR = <=").errors().is_empty());
}

#[test]
fn interpolation_close_is_not_half_of_code_close() {
    let tree = parse(b"x = { code [[ a[b]");
    let repaired = closed(&tree);
    assert_eq!(repaired, b"x = { code [[ a[b]]]}");
    assert!(parse(&repaired).errors().is_empty());
}

#[test]
fn trailing_comment_flags_the_unclosed_block() {
    let tree = parse(b"a = { b = 1 # c");
    let field = tree.root().child_nodes().next().unwrap();
    let block = field
        .child_nodes()
        .find(|node| node.kind() == SyntaxKind::Block)
        .unwrap();
    assert!(field.has_comment());
    assert!(block.has_comment());
}

#[test]
fn deeply_nested_code_payloads_do_not_overflow() {
    let depth = MAX_DEPTH as usize + 16;
    let mut source = Vec::new();
    for _ in 0..depth {
        source.extend_from_slice(b"code [[");
    }
    for _ in 0..depth {
        source.extend_from_slice(b"]]");
    }

    let tree = parse(&source);
    assert_eq!(tree.reconstruct(), source);
    assert!(
        tree.errors()
            .iter()
            .any(|e| e.kind == SyntaxErrorKind::CodeDepthExceeded)
    );
}

#[test]
fn parameter_recovery_keeps_following_block_delimiter() {
    let source = b"outer = { [[name] value = yes }";
    let tree = parse(source);
    assert_eq!(tree.reconstruct(), source);
    assert_eq!(
        error_kinds(tree.tree()),
        [SyntaxErrorKind::UnclosedParameter]
    );
    let outer = tree
        .ast()
        .fields()
        .next()
        .unwrap()
        .value()
        .unwrap()
        .as_block()
        .unwrap();
    assert!(
        outer
            .syntax()
            .child_tokens()
            .any(|token| token.kind() == SyntaxKind::CloseBrace)
    );
}

#[test]
fn parameter_body_can_continue_a_brace_across_parameters() {
    let source = b"outer = { [[tooltip] tooltip = { ] [[tooltip] } ] }";
    let tree = parse(source);
    assert!(tree.errors().is_empty());
    assert_eq!(tree.reconstruct(), source);
    let outer = tree
        .ast()
        .fields()
        .next()
        .unwrap()
        .value()
        .unwrap()
        .as_block()
        .unwrap();
    assert_eq!(
        outer
            .items()
            .filter(|entry| matches!(entry, Item::Parameter(_)))
            .count(),
        2
    );
}

fn fmt(src: &[u8]) -> String {
    String::from_utf8(format(src)).unwrap()
}

/// The significant-token stream (everything but whitespace and comments) as
/// `(kind, text)` pairs. The formatter must preserve this exactly, in order.
fn significant(src: &[u8]) -> Vec<(SyntaxKind, Vec<u8>)> {
    parse(src)
        .tokens()
        .filter(|t| {
            !t.kind().is_missing()
                && !matches!(t.kind(), SyntaxKind::Whitespace | SyntaxKind::Comment)
        })
        .map(|t| (t.kind(), t.text().to_vec()))
        .collect()
}

/// The multiset of comment texts (comments may be relocated, never dropped,
/// merged, or altered).
fn comment_texts(src: &[u8]) -> Vec<Vec<u8>> {
    let mut v: Vec<Vec<u8>> = parse(src)
        .tokens()
        .filter(|t| t.kind() == SyntaxKind::Comment)
        .map(|t| t.text().to_vec())
        .collect();
    v.sort();
    v
}

#[test]
fn fmt_normalizes_field_spacing() {
    assert_eq!(fmt(b"a=b"), "a = b\n");
    assert_eq!(fmt(b"a    =\tb"), "a = b\n");
    assert_eq!(fmt(b"a == b"), "a == b\n"); // operator text kept verbatim
    assert_eq!(fmt(b"age>16"), "age > 16\n");
}

#[test]
fn fmt_inline_scalar_blocks() {
    assert_eq!(fmt(b"color=rgb{255 0 128}"), "color = rgb { 255 0 128 }\n");
    assert_eq!(fmt(b"arr = {1    2 3}"), "arr = { 1 2 3 }\n");
}

#[test]
fn fmt_empty_block_collapses() {
    assert_eq!(fmt(b"a = {   }"), "a = {}\n");
    assert_eq!(fmt(b"a={\n\n}"), "a = {}\n");
}

#[test]
fn fmt_nested_blocks_indent_with_tabs() {
    assert_eq!(fmt(b"a={b={c=d}}"), "a = {\n\tb = {\n\t\tc = d\n\t}\n}\n");
}

#[test]
fn fmt_block_with_fields_is_multiline() {
    assert_eq!(fmt(b"o={x=1 y=2}"), "o = {\n\tx = 1\n\ty = 2\n}\n");
}

#[test]
fn fmt_comments_leading_trailing_and_in_block() {
    assert_eq!(fmt(b"a = b # note"), "a = b # note\n");
    assert_eq!(fmt(b"# header\na = b"), "# header\na = b\n");
    assert_eq!(
        fmt(b"o = {\n# inner\nx = 1\n}"),
        "o = {\n\t# inner\n\tx = 1\n}\n"
    );
}

#[test]
fn fmt_relocated_interior_comment_is_safe() {
    // A comment between `=` and the value is hoisted to its own line before
    // the field so it cannot comment out the value; both tokens survive.
    let out = fmt(b"a = # oops\n b");
    assert_eq!(out, "# oops\na = b\n");
    assert_eq!(significant(b"a = # oops\n b"), significant(out.as_bytes()));
}

#[test]
fn fmt_preserves_single_blank_line() {
    assert_eq!(fmt(b"a = 1\n\n\n\nb = 2"), "a = 1\n\nb = 2\n");
}

#[test]
fn fmt_calc_is_verbatim() {
    assert_eq!(fmt(b"x=@[1+2]"), "x = @[1+2]\n");
    assert_eq!(fmt(b"x = @[ a + b ]"), "x = @[ a + b ]\n");
}

#[test]
fn fmt_unterminated_quote_keeps_no_trailing_newline() {
    // The trailing newline would be absorbed into the open string, so it is
    // suppressed and the (significant) token stream is preserved.
    assert_eq!(format(b"a = \"abc"), b"a = \"abc");
    assert_eq!(
        significant(b"a = \"abc"),
        significant(&format(b"a = \"abc"))
    );
}

#[test]
fn fmt_bom_preserved_at_top() {
    assert_eq!(format(b"\xef\xbb\xbfa=b"), b"\xef\xbb\xbfa = b\n");
}

#[test]
fn fmt_bom_bytes_in_first_token_stay_a_token() {
    let source = b" \xef\xbb\xbf = 1";
    let out = format(source);
    assert_eq!(significant(source), significant(&out));
    assert_eq!(format(&out), out);
}

#[test]
fn fmt_empty_input() {
    assert_eq!(format(b""), b"");
    assert_eq!(format(b"   \n\t "), b"");
}

#[test]
fn fmt_unclosed_block_keeps_brace_count() {
    // No phantom `}` is invented for an unclosed block.
    let out = fmt(b"a = { b = c");
    assert_eq!(significant(b"a = { b = c"), significant(out.as_bytes()));
    assert_eq!(out.matches('}').count(), 0);
}

#[test]
fn fmt_interrupted_calc_keeps_physical_tokens() {
    let source = b"@[{";
    let formatted = format(source);
    assert_eq!(significant(source), significant(&formatted));
    assert_eq!(format(&formatted), formatted);
}

#[quickcheck]
fn prop_format_idempotent(data: Vec<u8>) -> bool {
    let once = format(&data);
    let twice = format(&once);
    once == twice
}

#[quickcheck]
fn prop_format_preserves_significant_and_comments(data: Vec<u8>) -> bool {
    let f = format(&data);
    significant(&data) == significant(&f) && comment_texts(&data) == comment_texts(&f)
}

/// Stress the formatter on a dense alphabet of structural bytes (random
/// input almost never forms blocks/calcs/comments otherwise).
#[quickcheck]
fn prop_format_alphabet(data: Vec<u8>) -> bool {
    const ALPHABET: &[u8] = b"{}[]()=<># \n\t\"@$.0123456789abc-+*/";
    let m: Vec<u8> = data
        .iter()
        .map(|b| ALPHABET[*b as usize % ALPHABET.len()])
        .collect();
    let f = format(&m);
    significant(&m) == significant(&f) && comment_texts(&m) == comment_texts(&f) && format(&f) == f
}

#[test]
fn node_flags_summarize_subtrees() {
    // The `outer` block has a comment and a macro. Its `inner` block has
    // neither. Only the sibling field `c` has a calc.
    let tree = parse(b"outer = { # hi\n a = $P$ inner = { b = c } } c = @[1+2]");
    let root = tree.root();
    assert!(root.has_comment() && root.has_macro() && root.has_calc());
    assert!(!root.has_error());

    let mut fields = tree.ast().fields();
    let outer_block = fields
        .next()
        .unwrap()
        .value()
        .unwrap()
        .as_block()
        .unwrap()
        .syntax();
    assert!(outer_block.has_comment());
    assert!(outer_block.has_macro());
    assert!(!outer_block.has_calc());

    let inner = outer_block
        .child_nodes()
        .filter_map(|f| Field::cast(f)?.value()?.as_block())
        .map(|b| b.syntax())
        .next()
        .expect("the `inner` block");
    assert!(!inner.has_comment() && !inner.has_macro() && !inner.has_calc());

    let calc_field = fields.next().unwrap().syntax();
    assert!(calc_field.has_calc() && !calc_field.has_comment() && !calc_field.has_macro());
}

/// Compact structural rendering of a subtree: interior nodes as
/// `Kind(child ...)`, significant tokens as their bare `Kind`, trivia
/// omitted. Terse enough to assert precedence and associativity directly.
fn shape(el: SyntaxElement<'_, '_>) -> String {
    match el {
        SyntaxElement::Node(n) => {
            let kids: Vec<String> = n
                .children()
                .filter(|c| !c.kind().is_trivia())
                .map(shape)
                .collect();
            format!("{:?}({})", n.kind(), kids.join(" "))
        }
        SyntaxElement::Token(t) => format!("{:?}", t.kind()),
    }
}

/// Parse `key = <calc>` and return the structural shape of the `Calc` node.
fn calc_shape(src: &[u8]) -> String {
    let tree = parse(src);
    let field = tree.root().child_nodes().next().unwrap();
    let calc = field
        .child_nodes()
        .find(|n| n.kind() == SyntaxKind::Calc)
        .expect("a Calc node");
    shape(SyntaxElement::Node(calc))
}

#[test]
fn calc_precedence_mul_binds_tighter() {
    // `*` binds tighter than `+`: 1 + (2 * 3).
    assert_eq!(
        calc_shape(b"x = @[1+2*3]"),
        "Calc(CalcOpen BinaryExpr(Number Plus BinaryExpr(Number Star Number)) CalcClose)"
    );
    // Mirror: (1 * 2) + 3.
    assert_eq!(
        calc_shape(b"x = @[1*2+3]"),
        "Calc(CalcOpen BinaryExpr(BinaryExpr(Number Star Number) Plus Number) CalcClose)"
    );
}

#[test]
fn calc_addition_is_left_associative() {
    // 1 - 2 - 3 parses as (1 - 2) - 3, not 1 - (2 - 3).
    assert_eq!(
        calc_shape(b"x = @[1-2-3]"),
        "Calc(CalcOpen BinaryExpr(BinaryExpr(Number Minus Number) Minus Number) CalcClose)"
    );
}

#[test]
fn calc_unary_minus() {
    assert_eq!(
        calc_shape(b"x = @[ -x ]"),
        "Calc(CalcOpen UnaryExpr(Minus CalcIdent) CalcClose)"
    );
    // Unary binds tighter than binary: (-half) - half.
    assert_eq!(
        calc_shape(b"x = @[-half-half]"),
        "Calc(CalcOpen BinaryExpr(UnaryExpr(Minus CalcIdent) Minus CalcIdent) CalcClose)"
    );
}

#[test]
fn calc_nested_parens() {
    assert_eq!(
        calc_shape(b"x = @[((1))]"),
        "Calc(CalcOpen ParenExpr(OpenParen ParenExpr(OpenParen Number CloseParen) \
             CloseParen) CalcClose)"
    );
    // A parenthesized sub-expression lowers `*` below it: (a + b) * c.
    assert_eq!(
        calc_shape(b"x = @[(a+b)*c]"),
        "Calc(CalcOpen BinaryExpr(ParenExpr(OpenParen BinaryExpr(CalcIdent Plus CalcIdent) \
             CloseParen) Star CalcIdent) CalcClose)"
    );
}

#[test]
fn calc_operands_are_single_tokens() {
    // `10.0f`, `.5`, `@var`, and `leopard_x` are each one operand token.
    assert_eq!(
        calc_shape(b"x = @[10.0f / @var]"),
        "Calc(CalcOpen BinaryExpr(Number Slash CalcIdent) CalcClose)"
    );
    assert_eq!(
        calc_shape(b"x = @[.5*leopard_x]"),
        "Calc(CalcOpen BinaryExpr(Number Star CalcIdent) CalcClose)"
    );
    // A `-` ends an identifier, so it is an operator and not a name byte.
    assert_eq!(
        calc_shape(b"x = @[a-b]"),
        "Calc(CalcOpen BinaryExpr(CalcIdent Minus CalcIdent) CalcClose)"
    );
}

#[test]
fn calc_in_array_position() {
    // A bare calc as an array element (no `=`) is still grouped as a Calc.
    let tree = parse(b"position = { @[1-leopard_x] @leopard_y }");
    let block = tree
        .root()
        .child_nodes()
        .next()
        .unwrap()
        .child_nodes()
        .next()
        .unwrap();
    assert_eq!(block.kind(), SyntaxKind::Block);
    let calc = block
        .child_nodes()
        .find(|n| n.kind() == SyntaxKind::Calc)
        .unwrap();
    assert_eq!(calc.text(), b"@[1-leopard_x]");
    // The following `@leopard_y` is a Variable token, not folded into the calc.
    assert!(
        block
            .child_tokens()
            .any(|t| t.kind() == SyntaxKind::Variable)
    );
}

#[test]
fn calc_empty() {
    assert_eq!(calc_shape(b"x = @[]"), "Calc(CalcOpen CalcClose)");
    assert!(parse(b"x = @[]").errors().is_empty());
}

#[test]
fn calc_unterminated_diagnostic() {
    let source = b"x = @[1 + 2";
    let tree = parse(source);
    assert_eq!(error_kinds(tree.tree()), [SyntaxErrorKind::UnclosedCalc]);
    // The expression is complete. Only the `]` is missing.
    assert_eq!(
        calc_shape(source),
        "Calc(CalcOpen BinaryExpr(Number Plus Number) MissingCalcClose)"
    );
    assert_eq!(tree.closing_edit().unwrap().new_text, b"]");
    assert!(parse(&closed(&tree)).errors().is_empty());
}

#[test]
fn calc_combined_tree_depth_is_bounded() {
    let mut nested = String::from("a=@[");
    nested.push_str(&"(".repeat(200));
    nested.push('1');
    for _ in 0..200 {
        nested.push_str(&"+1".repeat(200));
        nested.push(')');
    }
    nested.push(']');
    for source in [nested.as_bytes(), &nested.as_bytes()[..nested.len() - 1]] {
        let tree = parse(source);
        assert!(
            tree.errors()
                .iter()
                .any(|e| e.kind == SyntaxErrorKind::CalcDepthExceeded)
        );
        assert!(tree.root().has_error());
        assert_eq!(tree.reconstruct(), source);
        assert!(tree.closing_edit().is_none());
        let mut stack = vec![(tree.root(), 0)];
        while let Some((node, depth)) = stack.pop() {
            assert!(depth <= MAX_DEPTH + 3);
            stack.extend(node.child_nodes().map(|child| (child, depth + 1)));
        }
    }

    let source = format!("a=@[{}1{}]", "(".repeat(100), "+1)".repeat(100));
    let tree = parse(source.as_bytes());
    assert!(tree.errors().is_empty());
    assert_eq!(tree.reconstruct(), source.as_bytes());
}

#[test]
fn calc_missing_parens_before_close_have_diagnostics() {
    for source in [b"a=@[(1]".as_slice(), b"a=@[((1]", b"a={b=@[(1}"] {
        let tree = parse(source);
        assert!(
            tree.errors()
                .iter()
                .any(|e| e.kind == SyntaxErrorKind::UnclosedCalcParen)
        );
        assert!(tree.errors().iter().all(|e| e.missing_delimiter.is_none()));
        assert!(tree.root().has_error());
        assert!(tree.closing_edit().is_none());
        assert_eq!(tree.reconstruct(), source);
        let mut stack = vec![tree.root()];
        while let Some(node) = stack.pop() {
            if node.kind() == SyntaxKind::ParenExpr {
                assert!(node.has_error());
            }
            stack.extend(node.child_nodes());
        }
    }
}

#[test]
fn calc_recovery_tokens_keep_their_emitted_parents() {
    for source in [b"a=@[(1 2".as_slice(), b"a=@[((1 2", b"a={b=@[(1 2 "] {
        let tree = parse(source);
        assert_eq!(tree.reconstruct(), source);
        let mut previous_end = TextSize::new(0);
        for token in tree.significant_tokens() {
            assert!(token.text_range().start() >= previous_end);
            previous_end = token.text_range().end();
            if token.text() == b"2" {
                assert_eq!(token.parent().unwrap().kind(), SyntaxKind::Calc);
            }
        }
        let calc = if source.starts_with(b"a={") {
            tree.ast()
                .fields()
                .next()
                .unwrap()
                .value()
                .unwrap()
                .as_block()
                .unwrap()
                .fields()
                .next()
                .unwrap()
                .value()
                .unwrap()
                .as_calc()
                .unwrap()
        } else {
            tree.ast()
                .fields()
                .next()
                .unwrap()
                .value()
                .unwrap()
                .as_calc()
                .unwrap()
        };
        // The `2` is not a part of the expression, so `Calc::expr` gives
        // `None`.
        assert_eq!(calc.expr(), None);
        let paren = calc
            .syntax()
            .child_nodes()
            .find_map(ParenExpr::cast)
            .unwrap();
        // The `)` is missing before the `2`, so the tree has no zero-width
        // `)` at the end of the source.
        assert!(
            !paren
                .syntax()
                .significant_child_tokens()
                .any(|token| token.kind() == SyntaxKind::MissingCloseParen)
        );
        assert_eq!(
            error_kinds(tree.tree())[0],
            SyntaxErrorKind::UnclosedCalcParen
        );
    }
}

#[test]
fn hoi4_flavor_has_variables() {
    // From HoI4 `common/technologies/infantry.txt`.
    let source = b"@1936 = 2\nposition = { x = 0 y = @1936 }";
    let tree = parse_with(source, Flavor::hoi4());
    assert_eq!(tree.reconstruct(), source);
    let variables = tree
        .tokens()
        .filter(|t| t.kind() == SyntaxKind::Variable)
        .count();
    assert_eq!(variables, 2);

    // HoI4 has no macros, so `$` is an ordinary identifier byte.
    let tree = parse_with(b"x = $NAME$", Flavor::hoi4());
    assert!(tree.tokens().all(|t| t.kind() != SyntaxKind::MacroParam));
}

#[quickcheck]
fn prop_round_trip(data: Vec<u8>) -> bool {
    parse(&data).reconstruct() == data
}

#[quickcheck]
fn prop_round_trip_hoi4(data: Vec<u8>) -> bool {
    parse_with(&data, Flavor::hoi4()).reconstruct() == data
}

/// Round-trip over a calc-heavy alphabet: random bytes almost never form a
/// `@[ ... ]`, so map each byte onto the small set of calc-relevant tokens
/// to actually exercise the calc lexer/parser under quickcheck.
#[quickcheck]
fn prop_round_trip_calc_alphabet(data: Vec<u8>) -> bool {
    const ALPHABET: &[u8] = b"@[]()+-*/ .0123456789fxy_";
    let mapped: Vec<u8> = data
        .iter()
        .map(|b| ALPHABET[*b as usize % ALPHABET.len()])
        .collect();
    parse(&mapped).reconstruct() == mapped
}

/// A byte-at-a-time lexer with the same rules as [`lex`]. It checks the
/// SIMD scans, which handle 16 bytes at a time.
fn reference_lex(source: &[u8], flavor: Flavor) -> Vec<(SyntaxKind, u32, u32, u8)> {
    let n = source.len();
    let mut i = if source.starts_with(BOM) { 3 } else { 0 };
    let mut flags = 0u8;
    let mut out = Vec::new();
    let push = |out: &mut Vec<_>, kind, start: usize, end: usize, flags: &mut u8| {
        out.push((kind, start as u32, end as u32, *flags));
        *flags = 0;
    };
    while i < n {
        let b = source[i];
        let start = i;
        if is_ws(b) {
            while i < n && is_ws(source[i]) {
                i += 1;
            }
            continue;
        }
        if b == b'#' {
            while i < n && source[i] != b'\n' && source[i] != b'\r' {
                i += 1;
            }
            flags = TOKEN_COMMENT_BEFORE;
            continue;
        }
        if flavor.variables && b == b'@' && source.get(i + 1) == Some(&b'[') {
            push(&mut out, SyntaxKind::CalcOpen, i, i + 2, &mut flags);
            i += 2;
            while i < n {
                let c = source[i];
                let start = i;
                let kind = match c {
                    b']' => {
                        i += 1;
                        push(&mut out, SyntaxKind::CalcClose, start, i, &mut flags);
                        break;
                    }
                    b'{' | b'}' => break,
                    c if is_ws(c) => {
                        i += 1;
                        continue;
                    }
                    b'(' => SyntaxKind::OpenParen,
                    b')' => SyntaxKind::CloseParen,
                    b'+' => SyntaxKind::Plus,
                    b'-' => SyntaxKind::Minus,
                    b'*' => SyntaxKind::Star,
                    b'/' => SyntaxKind::Slash,
                    c if c.is_ascii_digit()
                        || (c == b'.' && source.get(i + 1).is_some_and(u8::is_ascii_digit)) =>
                    {
                        i = scan_number(source, i);
                        push(&mut out, SyntaxKind::Number, start, i, &mut flags);
                        continue;
                    }
                    _ => {
                        i += 1;
                        while i < n && !is_calc_stop(source[i]) {
                            i += 1;
                        }
                        push(&mut out, SyntaxKind::CalcIdent, start, i, &mut flags);
                        continue;
                    }
                };
                i += 1;
                push(&mut out, kind, start, i, &mut flags);
            }
            continue;
        }
        let kind = match b {
            b'"' => {
                i += 1;
                let mut closed = false;
                while i < n {
                    match source[i] {
                        b'\\' => i = (i + 2).min(n),
                        b'"' => {
                            i += 1;
                            closed = true;
                            break;
                        }
                        _ => i += 1,
                    }
                }
                if (!closed && i >= n) || quote_hides_fields(&source[start..i]) {
                    flags |= TOKEN_ERROR;
                }
                SyntaxKind::Quoted
            }
            b'{' | b'}' | b'[' | b']' => {
                i += 1;
                match b {
                    b'{' => SyntaxKind::OpenBrace,
                    b'}' => SyntaxKind::CloseBrace,
                    b'[' => SyntaxKind::OpenBracket,
                    _ => SyntaxKind::CloseBracket,
                }
            }
            b'=' | b'<' | b'>' => {
                i += 1;
                if source.get(i) == Some(&b'=') {
                    i += 1;
                }
                SyntaxKind::Operator
            }
            b'!' | b'?' if source.get(i + 1) == Some(&b'=') => {
                i += 2;
                SyntaxKind::Operator
            }
            b'!' => {
                i += 1;
                SyntaxKind::Bang
            }
            _ => {
                i += 1;
                while i < n && !STOP[source[i] as usize] {
                    i += 1;
                }
                if source.get(i) == Some(&b'=')
                    && i - start > 1
                    && matches!(source[i - 1], b'!' | b'?')
                {
                    i -= 1;
                }
                let run = &source[start..i];
                if flavor.variables && run.len() > 2 && run.starts_with(b"@:") {
                    SyntaxKind::Directive
                } else if flavor.variables && run.len() > 2 && run.starts_with(b"@@") {
                    SyntaxKind::VariableKey
                } else if flavor.variables && run.len() > 1 && run[0] == b'@' && run[1] != b'@' {
                    SyntaxKind::Variable
                } else if flavor.macros && run.iter().filter(|&&b| b == b'$').count() >= 2 {
                    SyntaxKind::MacroParam
                } else {
                    SyntaxKind::Unquoted
                }
            }
        };
        push(&mut out, kind, start, i, &mut flags);
    }
    out
}

/// Compare the dispatched lexer and the portable fallback lexer (the path
/// for targets without SIMD) with [`reference_lex`].
fn lex_matches_reference(source: &[u8]) -> bool {
    let simplify = |tokens: &[Token]| -> Vec<_> {
        tokens
            .iter()
            .map(|t| (t.kind, t.start, t.end(), t.flags))
            .collect()
    };
    [Flavor::default(), Flavor::hoi4()]
        .into_iter()
        .all(|flavor| {
            let expected = reference_lex(source, flavor);
            let lexed = lex(source, flavor);
            let mut fallback = Vec::new();
            let start = if source.starts_with(BOM) { 3 } else { 0 };
            let (_, flags) = lex_simd(
                fearless_simd::Fallback::new(),
                source,
                flavor,
                (start, 0),
                usize::MAX,
                &mut fallback,
                &mut Vec::new(),
            );
            let fallback_comment = flags & TOKEN_COMMENT_BEFORE != 0;
            simplify(&lexed.tokens) == expected
                && simplify(&fallback) == expected
                && fallback_comment == lexed.trailing_comment
        })
}

#[quickcheck]
fn prop_lex_matches_reference(data: Vec<u8>) -> bool {
    lex_matches_reference(&data)
}

/// Map random bytes onto the bytes that the lexer treats specially, so
/// that the SIMD masks see every class at every lane position.
#[quickcheck]
fn prop_lex_matches_reference_alphabet(data: Vec<u8>) -> bool {
    const ALPHABET: &[u8] = b"ab01._-$@#\"\\ \t\r\n\x0b\x0c;{}[]=<>!?()+*/|:\x80\xff";
    let mapped: Vec<u8> = data
        .iter()
        .map(|b| ALPHABET[*b as usize % ALPHABET.len()])
        .collect();
    lex_matches_reference(&mapped)
}

/// Check the nibble tables of [`run_lanes`] against [`STOP`] for every
/// byte, in the fallback and in the dispatched SIMD level.
#[test]
fn run_lanes_match_stop_table() {
    fn check<S: Simd>(simd: S) {
        for b in 0..=255u8 {
            let mut bytes = [b'a'; 16];
            bytes[b as usize % 16] = b;
            let (stops, dollars) = run_lanes(simd, u8x16::from_slice(simd, &bytes));
            let lane = 1u64 << (b as usize % 16);
            assert_eq!(stops == lane, STOP[b as usize], "stop byte {b:#04x}");
            assert_eq!(stops & !lane, 0, "stop byte {b:#04x}");
            assert_eq!(dollars, if b == b'$' { lane } else { 0 }, "byte {b:#04x}");
        }
    }
    check(fearless_simd::Fallback::new());
    dispatch!(Level::new(), simd => check(simd));
}

#[test]
fn lex_scans_across_vector_boundaries() {
    for len in 0..48 {
        let word = "x".repeat(len);
        let cases = [
            format!("{word}={word}"),
            format!("\"{word}\\\"{word}\""),
            format!("#{word}\n{word}"),
            format!("{}{word}", " ".repeat(len)),
            format!("$a$_{word}"),
            format!("{word}$a$ {word}"),
            format!("\"{word}\n{word}"),
        ];
        for case in &cases {
            assert!(lex_matches_reference(case.as_bytes()), "{case:?}");
            rt(case.as_bytes());
        }
    }
}

/// The scan that [`resolve_interpolations`] replaces: from one `[`, count
/// brackets until the match, and stop at a `}` directly inside it.
fn naive_interpolation_start(tokens: &[Token], pos: usize) -> bool {
    let mut depth = 1u32;
    for token in &tokens[pos + 1..] {
        match token.kind {
            SyntaxKind::OpenBracket => depth += 1,
            SyntaxKind::CloseBracket => {
                depth -= 1;
                if depth == 0 {
                    return true;
                }
            }
            SyntaxKind::CloseBrace if depth == 1 => return false,
            _ => {}
        }
    }
    pos + 1 < tokens.len()
}

#[quickcheck]
fn prop_interpolation_starts_match_naive_scan(data: Vec<u8>) -> bool {
    const ALPHABET: &[u8] = b"[]{} a";
    let mapped: Vec<u8> = data
        .iter()
        .map(|b| ALPHABET[*b as usize % ALPHABET.len()])
        .collect();
    let tokens = lex(&mapped, Flavor::default()).tokens;
    let mut starts = Vec::new();
    let mut resolved_until = 0;
    tokens.iter().enumerate().all(|(pos, token)| {
        if token.kind != SyntaxKind::OpenBracket {
            return true;
        }
        if pos >= resolved_until {
            resolved_until =
                resolve_interpolations(pos, |index| tokens.get(index).map(|t| t.kind), &mut starts);
        }
        starts.binary_search(&(pos as u32)).is_ok() == naive_interpolation_start(&tokens, pos)
    })
}

#[test]
fn scalar_followed_by_block_is_a_field_without_operator() {
    let tree = parse(b"foo{bar=qux} color = { rgb { 1 2 3 } }");
    assert!(tree.errors().is_empty());
    let fields: Vec<_> = tree.ast().fields().collect();
    assert_eq!(fields.len(), 2);

    assert_eq!(fields[0].key().text(), b"foo");
    assert!(fields[0].op_token().is_none());
    assert!(fields[0].op().is_none());
    let block = fields[0].value().unwrap().as_block().unwrap();
    assert_eq!(block.fields().next().unwrap().key().text(), b"bar");

    // In item position, a header such as `rgb` is a field too. The
    // semantic layer decides that it is a header.
    let inner = fields[1].value().unwrap().as_block().unwrap();
    let rgb = inner.fields().next().unwrap();
    assert_eq!(rgb.key().text(), b"rgb");
    let values = rgb.value().unwrap().as_block().unwrap().values().count();
    assert_eq!(values, 3);

    // In value position, a header is still a headered block.
    let tree = parse(b"color = rgb { 1 2 3 }");
    let value = tree.ast().fields().next().unwrap().value().unwrap();
    assert!(value.as_headered_block().is_some());
}

#[test]
fn macro_parameters_glue_to_the_surrounding_scalar() {
    for (source, kind) in [
        (b"$a$_b".as_slice(), SyntaxKind::MacroParam),
        (b"foo_$a$", SyntaxKind::MacroParam),
        (b"$a$_$b$_c", SyntaxKind::MacroParam),
        (b"$TIER|capital$", SyntaxKind::MacroParam),
        (b"$$", SyntaxKind::MacroParam),
        (b"$5", SyntaxKind::Unquoted),
        (b"a$b", SyntaxKind::Unquoted),
    ] {
        let tokens = lex(source, Flavor::default()).tokens;
        assert_eq!(tokens.len(), 1, "{source:?}");
        assert_eq!(tokens[0].kind, kind, "{source:?}");
    }

    let tree = parse(b"k = $a$_b");
    let field = tree.ast().fields().next().unwrap();
    let value = field.value().unwrap().as_scalar().unwrap();
    assert_eq!(value.as_bytes(), b"$a$_b");
    assert!(tree.root().has_macro());
    assert_eq!(tree.root().child_nodes().count(), 1);

    // HoI4 has no macros.
    let tokens = lex(b"$a$_b", Flavor::hoi4()).tokens;
    assert_eq!(tokens[0].kind, SyntaxKind::Unquoted);
}

#[test]
fn trivia_belongs_to_the_nearest_common_ancestor() {
    let source = b"# lead\na # inside\n= { b }  # tail\n";
    let tree = parse(source);
    let comments: Vec<_> = tree
        .tokens()
        .filter(|t| t.kind() == SyntaxKind::Comment)
        .collect();
    assert_eq!(comments.len(), 3);
    assert!(comments.iter().all(|c| c.is_trivia()));
    assert_eq!(comments[0].parent().unwrap().kind(), SyntaxKind::Root);
    assert_eq!(comments[1].parent().unwrap().kind(), SyntaxKind::Field);
    assert_eq!(comments[2].parent().unwrap().kind(), SyntaxKind::Root);

    // The space after `{` belongs to the block.
    let space = tree
        .tokens()
        .find(|t| t.kind() == SyntaxKind::Whitespace && t.text_range().start() == TextSize::new(21))
        .unwrap();
    assert_eq!(space.parent().unwrap().kind(), SyntaxKind::Block);

    let field = tree.root().child_nodes().next().unwrap();
    assert!(field.has_comment());
    assert!(tree.root().has_comment());
    let block = field.child_nodes().next().unwrap();
    assert!(!block.has_comment());

    // The node's children hold the same trivia as the cursor parents.
    let field_comments = field
        .child_tokens()
        .filter(|t| t.kind() == SyntaxKind::Comment)
        .count();
    assert_eq!(field_comments, 1);
    assert_eq!(field.text(), b"a # inside\n= { b }");
    assert_eq!(field.significant_children().count(), 3);
}

/// The parent of each leaf is the node whose children hold the leaf, on
/// random input of structural bytes.
#[quickcheck]
fn prop_leaf_parent_holds_the_leaf(data: Vec<u8>) -> bool {
    const ALPHABET: &[u8] = b"{}[]()=@# \n\"ab1+";
    let source: Vec<u8> = data
        .iter()
        .map(|b| ALPHABET[*b as usize % ALPHABET.len()])
        .collect();
    fn walk(node: SyntaxNode<'_, '_>) -> bool {
        node.children().all(|child| match child {
            SyntaxElement::Token(token) => token.parent() == Some(node),
            SyntaxElement::Node(child) => child.parent() == Some(node) && walk(child),
        })
    }
    walk(parse(&source[..]).root())
}

#[test]
fn iterators_are_fused_and_sized() {
    let tree = parse(b"a = { b } # c");
    let mut tokens = tree.tokens();
    assert_eq!(tokens.size_hint(), (5, None));
    assert_eq!(tokens.clone().count(), 11);
    tokens.by_ref().for_each(drop);
    assert_eq!((tokens.next(), tokens.next()), (None, None));

    let mut significant = tree.significant_tokens();
    assert_eq!(significant.len(), 5);
    assert_eq!(significant.next_back().unwrap().text(), b"}");
}

#[test]
fn every_leaf_parent_holds_the_leaf() {
    let source = b"# c\na = { b = @[ (1 + x) * 2 ] [[p] c = d ] } e { f } g\n";
    let tree = parse(source);
    fn walk(node: SyntaxNode<'_, '_>) {
        for child in node.children() {
            match child {
                SyntaxElement::Token(token) => {
                    assert_eq!(token.parent().unwrap().text_range(), node.text_range());
                    assert_eq!(token.parent().unwrap().kind(), node.kind());
                }
                SyntaxElement::Node(child) => {
                    assert_eq!(child.parent().unwrap().text_range(), node.text_range());
                    walk(child);
                }
            }
        }
    }
    walk(tree.root());
    assert_eq!(tree.reconstruct(), source);
}

#[test]
fn decode_removes_quotes_and_escapes() {
    let tree = parse(b"a = \"x \\\"y\\\"\" b = \xe4");
    let values: Vec<_> = tree
        .ast()
        .fields()
        .map(|f| {
            f.value()
                .unwrap()
                .decode(crate::Windows1252Encoding::new())
                .unwrap()
        })
        .collect();
    assert_eq!(values, ["x \"y\"", "\u{e4}"]);
}

#[test]
fn closing_edit_is_lazy_and_cached() {
    let tree = parse(b"a = { b");
    assert!(tree.data.closing_edit.get().is_none());
    let edit = tree.closing_edit().cloned();
    assert_eq!(edit, Some(TextEdit::new(range(7, 7), "}")));
    assert_eq!(tree.data.closing_edit.get(), Some(&edit));
}

#[test]
fn field_parts_of_each_shape() {
    // (source, key, operator text, value text)
    type Case = (
        &'static [u8],
        &'static [u8],
        Option<&'static [u8]>,
        Option<&'static [u8]>,
    );
    let cases: [Case; 8] = [
        (b"a = b", b"a", Some(b"="), Some(b"b")),
        (b"a >= 5", b"a", Some(b">="), Some(b"5")),
        (b"a = { b }", b"a", Some(b"="), Some(b"{ b }")),
        (b"a { b }", b"a", None, Some(b"{ b }")),
        (b"a = rgb { 1 }", b"a", Some(b"="), Some(b"rgb { 1 }")),
        (b"a = @[1 + 2]", b"a", Some(b"="), Some(b"@[1 + 2]")),
        (b"a =", b"a", Some(b"="), None),
        (b"a = <=", b"a", Some(b"="), Some(b"<=")),
    ];
    for (source, key, op, value) in cases {
        let parse = parse(source);
        let field = parse.ast().fields().next().unwrap();
        assert_eq!(field.key().text(), key, "{}", source.escape_ascii());
        assert_eq!(
            field.op_token().map(|t| t.text()),
            op,
            "{}",
            source.escape_ascii()
        );
        assert_eq!(
            field.value().map(|v| v.syntax().text()),
            value,
            "{}",
            source.escape_ascii()
        );
    }
}

#[test]
fn operator_is_a_value_only_in_a_field() {
    let parse = parse(b"OPERATOR = <= list = { a >= }");
    let fields: Vec<_> = parse.ast().fields().collect();
    let Some(Value::Operator(op)) = fields[0].value() else {
        panic!("expected an operator value");
    };
    assert_eq!(op.op(), Some(Operator::LessThanEqual));
    assert_eq!(fields[0].value().unwrap().as_scalar(), None);

    // A field with no value has no operator value.
    let list = fields[1].value().unwrap().as_block().unwrap();
    let inner = list.fields().next().unwrap();
    assert_eq!(inner.op(), Some(Operator::GreaterThanEqual));
    assert_eq!(inner.value(), None);
    assert_eq!(list.values().count(), 0);
}

#[test]
fn significant_children_match_children_without_trivia() {
    let source = b"# c\na = { b = 1 # d\n rgb { 1 2 } [[p] x = $p$ ] } e = @[ 1 + (2 * f) ] {";
    let parse = parse(source);
    for node in parse.root().descendants() {
        let all: Vec<_> = node
            .children()
            .filter(|el| !el.kind().is_trivia())
            .collect();
        let significant: Vec<_> = node.significant_children().collect();
        assert_eq!(all, significant, "{node:?}");
    }
}

fn range(start: u32, end: u32) -> TextRange {
    TextRange::new(TextSize::new(start), TextSize::new(end))
}

#[test]
fn apply_edits_uses_the_ranges_of_one_source() {
    let source = b"a = 1 b = 2 c = 3";
    let edits = [
        TextEdit::new(range(16, 17), "300"),
        TextEdit::new(range(5, 11), ""),
        TextEdit::new(range(0, 0), b"# start\n"),
    ];
    assert_eq!(
        apply_edits(source, &edits).unwrap(),
        b"# start\na = 1 c = 300"
    );
    assert_eq!(apply_edits(source, &[]).unwrap(), source);
}

#[test]
fn apply_edits_keeps_the_order_of_insertions_at_one_offset() {
    let source = b"a = { b";
    let edits = [
        TextEdit::new(range(7, 7), "}"),
        TextEdit::new(range(6, 7), "c"),
        TextEdit::new(range(7, 7), b" }".as_slice()),
        TextEdit::new(range(6, 6), "x"),
    ];
    assert_eq!(apply_edits(source, &edits).unwrap(), b"a = { xc} }");
}

#[test]
fn apply_edits_keeps_windows_1252_bytes() {
    let source = b"name = Jean";
    let edits = [TextEdit::new(range(7, 11), b"J\xe9r\xf4me".to_vec())];
    assert_eq!(apply_edits(source, &edits).unwrap(), b"name = J\xe9r\xf4me");
}

#[test]
fn apply_edits_rejects_an_overlap() {
    let source = b"a = 1 b = 2";
    let edits = [
        TextEdit::new(range(4, 7), "x"),
        TextEdit::new(range(0, 5), "y"),
    ];
    let error = apply_edits(source, &edits).unwrap_err();
    assert_eq!(
        error,
        EditError::Overlap {
            first: range(0, 5),
            second: range(4, 7),
        }
    );
    assert_eq!(
        error.to_string(),
        "the edit at 4..7 overlaps the edit at 0..5"
    );

    let inside = [
        TextEdit::new(range(0, 5), "y"),
        TextEdit::new(range(2, 2), "z"),
    ];
    assert_eq!(
        apply_edits(source, &inside),
        Err(EditError::Overlap {
            first: range(0, 5),
            second: range(2, 2),
        })
    );
}

#[test]
fn apply_edits_rejects_a_range_past_the_end() {
    let edited = b"a = 10";
    let stale = [TextEdit::new(range(4, 7), "2")];
    let error = apply_edits(edited, &stale).unwrap_err();
    assert_eq!(
        error,
        EditError::OutOfBounds {
            range: range(4, 7),
            len: 6
        }
    );
    assert_eq!(
        error.to_string(),
        "the edit at 4..7 goes past the end of the source (6 bytes)"
    );
}

#[test]
fn a_token_that_cannot_be_a_value_is_an_error() {
    let tree = parse(b"a = ]");
    let errors: Vec<_> = tree.errors().iter().map(|e| (e.kind, e.range)).collect();
    assert_eq!(
        errors,
        [
            (SyntaxErrorKind::MissingValue, range(2, 3)),
            (SyntaxErrorKind::UnmatchedCloseBracket, range(4, 5)),
        ]
    );
    let field = tree.ast().get(b"a").unwrap();
    assert!(field.value().is_none());

    let tree = parse(b"a = !");
    assert_eq!(error_kinds(tree.tree()), [SyntaxErrorKind::MissingValue]);

    for source in [&b"a = ]"[..], b"a = !"] {
        let error = parse_tape(source).into_result().unwrap_err();
        assert_eq!(
            (error.kind, error.range),
            (SyntaxErrorKind::MissingValue, range(2, 3))
        );
    }

    // An operator after an operator is the value.
    let tree = parse(b"a = =");
    assert_eq!(tree.errors(), []);
    let value = tree.ast().get(b"a").unwrap().value().unwrap();
    assert_eq!(value.syntax().kind(), SyntaxKind::Operator);
}

#[test]
fn an_escaped_quote_is_text() {
    let source = b"name = \"a\\\"\"\nnext = yes";
    let tree = parse(source);
    assert_eq!(tree.errors(), []);
    let value = tree.ast().get(b"name").unwrap().value().unwrap();
    let token = value.as_scalar_token().unwrap();
    assert_eq!(token.as_bytes(), b"a\\\"");
    assert_eq!(value.decode(crate::Utf8Encoding::new()).unwrap(), "a\"");

    let tape = parse_tape(source).into_result().unwrap();
    assert_eq!(
        tape.tokens()[1],
        crate::TextToken::Quoted(crate::Scalar::new(b"a\\\""))
    );

    assert_eq!(strip_quotes(b"\"a\\\\\""), b"a\\\\");
    assert_eq!(strip_quotes(b"\"a\""), b"a");
    assert_eq!(strip_quotes(b"\"a"), b"a");
    assert_eq!(strip_quotes(b"\""), b"");
    assert_eq!(strip_quotes(b"\"\""), b"");
}

#[test]
fn format_adds_no_newline_to_a_string_that_continues_to_the_end() {
    let tree = parse(b"name = \"Paris");
    assert_eq!(error_kinds(tree.tree()), [SyntaxErrorKind::UnclosedQuote]);
    assert_eq!(tree.format(&FormatOptions::default()), b"name = \"Paris");
}

#[test]
fn interpolation_body_keeps_nested_brackets() {
    let text = |source: &[u8]| {
        let tree = parse(source);
        let value = tree.ast().get(b"name").unwrap().value().unwrap();
        let Value::Interpolation(interpolation) = value else {
            panic!("expected an interpolation");
        };
        let body: Vec<u8> = interpolation
            .body()
            .flat_map(|element| element.text().to_vec())
            .collect();
        let close = interpolation.close_bracket().map(|t| t.text_range());
        (body, close, interpolation.is_closed_in_source())
    };
    assert_eq!(
        text(b"name = [outer[inner]]"),
        (b"outer[inner]".to_vec(), Some(range(20, 21)), true)
    );
    assert_eq!(
        text(b"name = [outer[inner]"),
        (b"outer[inner]".to_vec(), None, false)
    );
}

#[test]
fn code_keyword_is_only_the_statement_keyword() {
    let tree = parse(b"code = [[ code ]]");
    let value = tree.ast().get(b"code").unwrap().value().unwrap();
    let Value::Code(code) = value else {
        panic!("expected a code payload");
    };
    assert!(code.keyword().is_none());
    let values: Vec<_> = code.values().map(|v| v.syntax().text().to_vec()).collect();
    assert_eq!(values, [b"code".to_vec()]);

    let tree = parse(b"code [[ a = 1 ] b ]]");
    let Some(Item::Code(code)) = tree.ast().items().next() else {
        panic!("expected a code statement");
    };
    assert_eq!(code.keyword().unwrap().text_range(), range(0, 4));
    let items: Vec<_> = code
        .items()
        .map(|item| item.syntax().text().to_vec())
        .collect();
    assert_eq!(items, [b"a = 1".to_vec(), b"]".to_vec(), b"b".to_vec()]);
}

#[test]
fn nested_directives_stop_at_the_depth_limit() {
    let mut source = b"@:register_variable x = ".repeat(100_000);
    source.extend_from_slice(b"1");

    // Each chain of 257 directives ends with one error, and the item loop
    // starts the next chain: 100,000 = 389 * 257 + 27.
    let tree = parse(&source);
    assert_eq!(tree.errors().len(), 389);
    assert!(
        tree.errors()
            .iter()
            .all(|e| e.kind == SyntaxErrorKind::DirectiveDepthExceeded)
    );
    assert_eq!(tree.errors()[0].range, range(256 * 24, 256 * 24 + 19));
    assert_eq!(tree.reconstruct(), source);

    let tape = parse_tape(&source);
    assert_eq!(tape.errors(), tree.errors());
}

#[test]
fn parameter_items_keep_the_brackets_of_the_body() {
    for (source, key) in [
        (&b"[[p] [[a b]] x = 1 ]"[..], &b"[[p]"[..]),
        (b"[[!p] [[a b]] x = 1 ]", b"[[!p]"),
    ] {
        let tree = parse(source);
        assert_eq!(tree.errors(), [], "{}", String::from_utf8_lossy(key));
        let item = tree.ast().items().next().unwrap();
        let items: Vec<_> = match item {
            Item::Parameter(p) => p.items().map(|i| i.syntax().kind()).collect(),
            Item::UndefinedParameter(p) => p.items().map(|i| i.syntax().kind()).collect(),
            _ => panic!("expected a parameter"),
        };
        assert_eq!(
            items,
            [
                SyntaxKind::OpenBracket,
                SyntaxKind::Interpolation,
                SyntaxKind::CloseBracket,
                SyntaxKind::Field,
            ]
        );
    }
}

#[test]
fn code_is_closed_with_trivia_between_the_brackets() {
    let closed = |source: &[u8]| {
        let tree = parse(source);
        let Some(Item::Code(code)) = tree.ast().items().next() else {
            panic!("expected a code statement");
        };
        (error_kinds(tree.tree()), code.is_closed_in_source())
    };
    assert_eq!(closed(b"code [[ a ]]"), (vec![], true));
    assert_eq!(closed(b"code [[ a ] ]"), (vec![], true));
    assert_eq!(closed(b"code [[ a ] # c\n]"), (vec![], true));
    assert_eq!(
        closed(b"code [[ a ]"),
        (vec![SyntaxErrorKind::CodeMissingSecondBracket], false)
    );
}

#[test]
fn directive_value_can_be_an_operator() {
    let tree = parse(b"@:register_variable comparison = <=");
    assert_eq!(tree.errors(), []);
    let Some(Item::ReaderDirective(directive)) = tree.ast().items().next() else {
        panic!("expected a directive");
    };
    let value = directive.value().unwrap();
    let Value::Operator(token) = value else {
        panic!("expected an operator value");
    };
    assert_eq!(token.text_range(), range(33, 35));
    assert_eq!(token.op(), Some(Operator::LessThanEqual));

    let tree = parse(b"@:register_variable size = 2");
    let Some(Item::ReaderDirective(directive)) = tree.ast().items().next() else {
        panic!("expected a directive");
    };
    assert_eq!(directive.value().unwrap().to_u64(), Some(2));
}

/// Each construct allows `MAX_DEPTH` levels, and the error starts at the first
/// construct past the limit.
#[test]
fn each_construct_allows_max_depth_levels() {
    type Make = fn(usize) -> Vec<u8>;
    let cases: [(Make, SyntaxErrorKind, std::ops::Range<u32>); 6] = [
        (
            |n| [b"a=".to_vec(), vec![b'{'; n], vec![b'}'; n]].concat(),
            SyntaxErrorKind::BlockDepthExceeded,
            258..259,
        ),
        (
            |n| {
                [
                    b"a=@[".to_vec(),
                    vec![b'('; n],
                    b"1".to_vec(),
                    vec![b')'; n],
                    b"]".to_vec(),
                ]
                .concat()
            },
            SyntaxErrorKind::CalcDepthExceeded,
            2..4,
        ),
        (
            |n| [b"a=@[".to_vec(), vec![b'-'; n], b"1]".to_vec()].concat(),
            SyntaxErrorKind::CalcDepthExceeded,
            2..4,
        ),
        (
            |n| [b"code [[".repeat(n), b"]]".repeat(n)].concat(),
            SyntaxErrorKind::CodeDepthExceeded,
            1797..1798,
        ),
        (
            |n| [b"[[x] ".repeat(n), b"]".repeat(n)].concat(),
            SyntaxErrorKind::ParameterDepthExceeded,
            1280..1281,
        ),
        (
            |n| [b"@:register_variable x = ".repeat(n), b"1".to_vec()].concat(),
            SyntaxErrorKind::DirectiveDepthExceeded,
            6144..6163,
        ),
    ];
    for (make, kind, error_range) in cases {
        let deepest = make(MAX_DEPTH as usize);
        assert_eq!(parse(&deepest).errors(), []);

        let too_deep = make(MAX_DEPTH as usize + 1);
        let tree = parse(&too_deep);
        let expected = [SyntaxError {
            kind,
            range: range(error_range.start, error_range.end),
            missing_delimiter: None,
        }];
        assert_eq!(tree.errors(), expected);
        assert_eq!(parse_tape(&too_deep).errors(), expected);
        assert_eq!(tree.reconstruct(), too_deep);
    }
}

/// A `(` gets a zero-width `)` at the end of the source only when the source
/// ends inside its expression. When the expression stops at a different
/// token, the error is at the `(`, as it is when a `]` follows.
#[test]
fn calc_paren_that_stops_before_a_token_has_no_recovery() {
    let tree = parse(b"a=@[(1 2");
    assert_eq!(
        tree.errors(),
        [
            SyntaxError {
                kind: SyntaxErrorKind::UnclosedCalcParen,
                range: range(4, 5),
                missing_delimiter: None,
            },
            SyntaxError {
                kind: SyntaxErrorKind::UnclosedCalc,
                range: range(8, 8),
                missing_delimiter: Some(MissingDelimiter {
                    construct: UnclosedConstruct::Calc,
                    opening_range: range(2, 4),
                    missing: SyntaxKind::MissingCalcClose,
                    order: 0,
                    text: "]",
                    safe: true,
                }),
            },
        ]
    );
    assert_eq!(closed(&tree), b"a=@[(1 2]");
    assert_eq!(parse_tape(b"a=@[(1 2").errors(), tree.errors());

    let tree = parse(b"a=@[(1 + 2");
    assert_eq!(
        error_kinds(tree.tree()),
        [
            SyntaxErrorKind::UnclosedCalcParen,
            SyntaxErrorKind::UnclosedCalc
        ]
    );
    assert_eq!(closed(&tree), b"a=@[(1 + 2)]");
}

/// A scalar that an operator follows is the key of the next field. An
/// operator with no key is an error, except directly after a block.
#[test]
fn stray_operators_are_errors() {
    let error = |kind, start, end| SyntaxError {
        kind,
        range: range(start, end),
        missing_delimiter: None,
    };
    let source = b"b =\nc = 2";
    let tree = parse(source);
    assert_eq!(tree.errors(), [error(SyntaxErrorKind::MissingValue, 2, 3)]);
    let root = tree.ast();
    assert_eq!(root.get(b"b").unwrap().value(), None);
    let c = root.get(b"c").and_then(|f| f.value());
    assert_eq!(c.and_then(|v| v.as_scalar_token()).unwrap().text(), b"2");
    assert_eq!(parse_tape(source).errors(), tree.errors());

    let cases: [(&[u8], SyntaxError); 4] = [
        (b"a = 1 = 2", error(SyntaxErrorKind::MissingValue, 2, 3)),
        (b"x={ a=b = c }", error(SyntaxErrorKind::MissingValue, 5, 6)),
        (b"= 2", error(SyntaxErrorKind::UnexpectedOperator, 0, 1)),
        (b"x={ = }", error(SyntaxErrorKind::UnexpectedOperator, 4, 5)),
    ];
    for (source, expected) in cases {
        let tree = parse(source);
        assert_eq!(tree.errors(), std::slice::from_ref(&expected), "{source:?}");
        assert_eq!(parse_tape(source).errors(), [expected], "{source:?}");
        assert_eq!(tree.reconstruct(), source);
    }

    let tree = parse(b"= 2");
    let bogus = tree.root().child_nodes().next().unwrap();
    assert_eq!(bogus.kind(), SyntaxKind::Bogus);

    // A block before the operator is the key of a field.
    for source in [&b"{ a=b }={ c }"[..], b"x={ { a=b } # key\n={ c } }"] {
        assert_eq!(parse(source).errors(), [], "{source:?}");
        assert_eq!(parse_tape(source).errors(), [], "{source:?}");
    }
}

/// A string that spans a line break and ends with an operator most likely
/// lacks its closing quote at the end of its first line.
#[test]
fn quote_that_hides_fields_is_an_error() {
    // The quote in the comment pairs the quotes again, so the string does
    // not run to the end of the source.
    let source = b"title = \"My Mod\nname = \"Jean\" # 6\" tall\nversion = \"1.0\"\n";
    let tree = parse(source);
    let expected = [SyntaxError {
        kind: SyntaxErrorKind::ProbablyUnclosedQuote,
        range: range(8, 9),
        missing_delimiter: None,
    }];
    assert_eq!(tree.errors(), expected);
    assert!(tree.ast().get(b"name").is_none());
    assert!(tree.root().has_error());
    assert_eq!(parse_tape(source).errors(), expected);

    // The unquoted scalar `1.0"` takes the last quote, so no other error
    // shows the lost quote.
    let tree = parse(b"title = \"My Mod\nversion = \"1.0\"\n");
    assert_eq!(
        error_kinds(tree.tree()),
        [SyntaxErrorKind::ProbablyUnclosedQuote]
    );

    // EU4 files keep script in a quoted string.
    let source = b"effect_angry = \"\n\tadd_government_power = {\n\t\tvalue = -1\n\t}\"\n";
    assert_eq!(parse(source).errors(), []);
    assert_eq!(parse_tape(source).errors(), []);

    assert!(quote_hides_fields(b"\"a\nb = \""));
    assert!(quote_hides_fields(b"\"a\nb >\t\""));
    assert!(!quote_hides_fields(b"\"a b = \""));
    assert!(!quote_hides_fields(b"\"a\nb = c\""));
    assert!(!quote_hides_fields(b"\"a\nb ="));
}
