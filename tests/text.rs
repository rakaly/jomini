use jomini::{Operator, Scalar, TextEvent, TextParser};

#[test]
fn simple_open() {
    let mut parser = TextParser::new(b"{");
    assert_eq!((parser.line(), parser.column()), (0, 0));
    assert_eq!(parser.next(), Some(TextEvent::Open));
    assert_eq!((parser.line(), parser.column()), (0, 1));
    assert_eq!(parser.next(), None);
    assert_eq!((parser.line(), parser.column()), (0, 1));
}

#[test]
fn simple_open_whitespace() {
    let mut parser = TextParser::new(b" \t \r\n { \r\n\t ");
    assert_eq!((parser.line(), parser.column()), (0, 0));
    assert_eq!(parser.next(), Some(TextEvent::Open));
    assert_eq!((parser.line(), parser.column()), (1, 2));
    assert_eq!(parser.next(), None);
    assert_eq!((parser.line(), parser.column()), (2, 2));
}

#[test]
fn open_close() {
    let mut parser = TextParser::new(b"{}{}{}{}{}{}{}{}");

    for _ in 0..8 {
        assert_eq!(parser.next(), Some(TextEvent::Open));
        assert_eq!(parser.next(), Some(TextEvent::End));
    }

    assert_eq!(parser.next(), None);
}

#[test]
fn one_scalar() {
    let mut parser = TextParser::new(b"abc");
    assert_eq!((parser.line(), parser.column()), (0, 0));
    assert_eq!(parser.next(), Some(TextEvent::Scalar(Scalar::new(b"abc"))));
    assert_eq!((parser.line(), parser.column()), (0, 3));
    assert_eq!(parser.next(), None);
    assert_eq!((parser.line(), parser.column()), (0, 3));
}

#[test]
fn two_scalar() {
    let mut parser = TextParser::new(b"abc def");
    assert_eq!((parser.line(), parser.column()), (0, 0));
    assert_eq!(parser.next(), Some(TextEvent::Scalar(Scalar::new(b"abc"))));
    assert_eq!((parser.line(), parser.column()), (0, 3));
    assert_eq!(parser.next(), Some(TextEvent::Scalar(Scalar::new(b"def"))));
    assert_eq!((parser.line(), parser.column()), (0, 7));
    assert_eq!(parser.next(), None);
    assert_eq!((parser.line(), parser.column()), (0, 7));
}

#[test]
fn simple_object() {
    let mut parser = TextParser::new(b"abc=def");
    assert_eq!(parser.next(), Some(TextEvent::Scalar(Scalar::new(b"abc"))));
    assert_eq!(parser.next(), Some(TextEvent::Operator(Operator::Equal)));
    assert_eq!(parser.next(), Some(TextEvent::Scalar(Scalar::new(b"def"))));
}

#[test]
fn test_quotes() {
    let mut parser = TextParser::new(b"\"abc\"");
    assert_eq!(parser.next(), Some(TextEvent::Quoted(Scalar::new(b"abc"))));
    assert_eq!((parser.line(), parser.column()), (0, 5));
    assert_eq!(parser.next(), None);
}

#[test]
fn test_multiline_quotes() {
    let mut parser = TextParser::new(b"\"a\n\nb\t\nc\"");
    assert_eq!(
        parser.next(),
        Some(TextEvent::Quoted(Scalar::new(b"a\n\nb\t\nc")))
    );
    assert_eq!((parser.line(), parser.column()), (3, 2));
    assert_eq!(parser.next(), None);
}

#[test]
fn test_comment() {
    let mut parser = TextParser::new(b"#I am a comment");
    assert_eq!(
        parser.next(),
        Some(TextEvent::Comment(Scalar::new(b"I am a comment")))
    );
    assert_eq!((parser.line(), parser.column()), (0, 15));
    assert_eq!(parser.next(), None);
}

#[test]
fn test_comment_carriage_return() {
    let mut parser = TextParser::new(b"#I am a comment\r");
    assert_eq!(
        parser.next(),
        Some(TextEvent::Comment(Scalar::new(b"I am a comment")))
    );
    assert_eq!((parser.line(), parser.column()), (0, 16));
    assert_eq!(parser.next(), None);
}

#[test]
fn test_comment_carriage_return_newline() {
    let mut parser = TextParser::new(b"#I am a comment\r\nabc");
    assert_eq!(
        parser.next(),
        Some(TextEvent::Comment(Scalar::new(b"I am a comment")))
    );
    assert_eq!((parser.line(), parser.column()), (1, 0));
    assert_eq!(parser.next(), Some(TextEvent::Scalar(Scalar::new(b"abc"))));
    assert_eq!((parser.line(), parser.column()), (1, 3));
}

#[test]
fn test_comment_newline() {
    let mut parser = TextParser::new(b"#I am a comment\nabc");
    assert_eq!(
        parser.next(),
        Some(TextEvent::Comment(Scalar::new(b"I am a comment")))
    );
    assert_eq!((parser.line(), parser.column()), (1, 0));
    assert_eq!(parser.next(), Some(TextEvent::Scalar(Scalar::new(b"abc"))));
    assert_eq!((parser.line(), parser.column()), (1, 3));
}

#[test]
fn test_blank_quotes() {
    let mut parser = TextParser::new(b"\"\"");
    assert_eq!(parser.next(), Some(TextEvent::Quoted(Scalar::new(b""))));
    assert_eq!((parser.line(), parser.column()), (0, 2));
    assert_eq!(parser.next(), None);
}
