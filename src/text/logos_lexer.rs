use logos::{Lexer, Logos};
use smallvec::SmallVec;
use crate::{Error, ErrorKind};

type ExpressionList = SmallVec<[ExpressionToken; 16]>;

/// A lossless token that captures all syntax elements including trivia
#[derive(Logos, Debug, PartialEq, Clone)]
pub enum LosslessToken {
    // Operators from existing parser
    /// Exact equality operator `==`
    #[token(b"==")]
    Exact,
    /// Less than or equal operator `<=`
    #[token(b"<=")]
    LessThanEqual,
    /// Greater than or equal operator `>=`
    #[token(b">=")]
    GreaterThanEqual,
    /// Not equal operator `!=`
    #[token(b"!=")]
    NotEqual,
    /// Exists operator `?=`
    #[token(b"?=")]
    Exists,
    /// Equal operator `=`
    #[token(b"=")]
    Equal,
    /// Less than operator `<`
    #[token(b"<")]
    LessThan,
    /// Greater than operator `>`
    #[token(b">")]
    GreaterThan,
    
    // Structural tokens
    /// Left brace `{`
    #[token(b"{")]
    LBrace,
    /// Right brace `}`
    #[token(b"}")]
    RBrace,
    /// Left bracket `[`
    #[token(b"[")]
    LBracket,
    /// Right bracket `]`
    #[token(b"]")]
    RBracket,
    
    // String literals - handle escape sequences properly
    /// Quoted string literal
    #[regex(br#""([^"\\]|\\.)*""#)]
    Quoted,
    
    // Expression tokens for @[...] syntax 
    /// Expression in @[...] syntax
    #[token(b"@[", parse_expression)]
    Expression(ExpressionList),
    
    // Variable references starting with @
    /// Variable reference starting with @
    #[regex(br"@[a-zA-Z_][a-zA-Z0-9_]*")]
    Variable,
    
    /// Undefined parameter marker ! (for [[!var_name] syntax)
    #[token(b"!")]
    UndefinedParameter,
    
    // Unquoted tokens - most common case
    /// Unquoted identifier or value
    #[regex(br"[a-zA-Z0-9_\-.:/|]+")]
    Unquoted,
    
    // Trivia tokens for lossless parsing
    /// Whitespace (spaces, tabs, newlines, semicolons)
    #[regex(br"[ \t\r\n;]+")]
    Whitespace,
    
    /// Comment starting with #
    #[regex(br"#[^\r\n]*")]
    Comment,
}

/// Tokens for expressions inside @[...] constructs
#[derive(Debug, Copy, Clone, PartialEq, Logos)]
pub enum ExpressionToken {
    /// Subtraction operator `-`
    #[token(b"-")]
    Subtract,
    /// Addition operator `+`
    #[token(b"+")]
    Add,
    /// Multiplication operator `*`
    #[token(b"*")]
    Multiply,
    /// Division operator `/`
    #[token(b"/")]
    Divide,
    /// Left parenthesis `(`
    #[token(b"(")]
    LParen,
    /// Right parenthesis `)`
    #[token(b")")]
    RParen,
    /// Right bracket `]` - marks end of expression
    #[token(b"]")]
    RBracket,
    
    /// Integer literal
    #[regex(br"-?[0-9]+")]
    Integer,
    
    /// Float literal (with optional 'f' suffix)
    #[regex(br"-?[0-9]*\.[0-9]+f?")]
    Float,
    
    /// Identifier (includes variables with @ prefix)
    #[regex(br"@?[a-zA-Z_][a-zA-Z0-9_]*")]
    Identifier,
    
    /// Whitespace within expressions
    #[regex(br"[ \t]+")]
    Whitespace,
}


/// Parse expression tokens between @[ and ]
fn parse_expression(lex: &mut Lexer<LosslessToken>) -> Result<ExpressionList, ()> {
    let remaining = lex.remainder();
    let mut expr_lexer = ExpressionToken::lexer(remaining);
    let mut tokens = SmallVec::new();
    
    // Parse tokens until we hit RBracket (which marks the end)
    while let Some(token) = expr_lexer.next() {
        match token {
            Ok(ExpressionToken::RBracket) => {
                // Found the end - advance main lexer past the ]
                lex.bump(expr_lexer.span().end);
                return Ok(tokens);
            }
            Ok(t) => tokens.push(t),
            Err(_) => {
                // Skip invalid tokens - could be improved
                continue;
            }
        }
    }
    
    // If we get here, we didn't find a closing bracket
    Err(())
}

/// Lossless lexer for jomini text format
pub struct LosslessLexer<'a> {
    lexer: Lexer<'a, LosslessToken>,
    input: &'a [u8],
    original_input: &'a [u8],
    bom_offset: usize,
}

impl<'a> LosslessLexer<'a> {
    /// Create a new lossless lexer from input bytes
    pub fn new(input: &'a [u8]) -> Self {
        // Strip UTF-8 BOM if present
        let (stripped_input, bom_offset) = if input.starts_with(&[0xef, 0xbb, 0xbf]) {
            (&input[3..], 3)
        } else {
            (input, 0)
        };
        
        Self {
            lexer: LosslessToken::lexer(stripped_input),
            input: stripped_input,
            original_input: input,
            bom_offset,
        }
    }
    
    /// Get the next token from the input
    pub fn next_token(&mut self) -> Option<Result<LosslessToken, Error>> {
        match self.lexer.next() {
            Some(Ok(token)) => Some(Ok(token)),
            Some(Err(_)) => {
                // Handle error - could be invalid token
                let span = self.lexer.span();
                Some(Err(Error::new(ErrorKind::InvalidSyntax { 
                    msg: "Invalid token".to_string(),
                    offset: span.start 
                })))
            }
            None => None,
        }
    }
    
    /// Get current position in the input (adjusted for BOM)
    pub fn position(&self) -> usize {
        self.lexer.span().start + self.bom_offset
    }
    
    /// Get the current span being processed (adjusted for BOM)
    pub fn span(&self) -> std::ops::Range<usize> {
        let span = self.lexer.span();
        (span.start + self.bom_offset)..(span.end + self.bom_offset)
    }
    
    /// Get remaining input
    pub fn remainder(&self) -> &'a [u8] {
        self.lexer.remainder()
    }
    
    /// Get the text for a token at the current span
    pub fn token_text(&self) -> &'a [u8] {
        let span = self.lexer.span();
        &self.input[span]
    }
    
    /// Get the text for a specific span in the original input
    pub fn span_text(&self, span: std::ops::Range<usize>) -> &'a [u8] {
        &self.original_input[span]
    }
}

/// Iterator implementation for the lossless lexer
impl<'a> Iterator for LosslessLexer<'a> {
    type Item = Result<LosslessToken, Error>;
    
    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

/// Token with position information for lossless reconstruction
#[derive(Debug, Clone, PartialEq)]
pub struct TokenWithSpan {
    /// The token type
    pub token: LosslessToken,
    /// The byte span in the original input where this token occurs
    pub span: std::ops::Range<usize>,
}

/// Collect all tokens with their spans for lossless processing
pub fn collect_tokens_with_spans(input: &[u8]) -> Result<Vec<TokenWithSpan>, Error> {
    let mut lexer = LosslessLexer::new(input);
    let mut tokens = Vec::new();
    
    while let Some(result) = lexer.next_token() {
        match result {
            Ok(token) => {
                let span = lexer.span();
                tokens.push(TokenWithSpan { token, span });
            }
            Err(e) => return Err(e),
        }
    }
    
    Ok(tokens)
}

/// Verify lossless property by reconstructing input from tokens with spans
pub fn reconstruct_input(original_input: &[u8], tokens: &[TokenWithSpan]) -> Vec<u8> {
    let mut result = Vec::new();
    
    for token_with_span in tokens {
        let text = &original_input[token_with_span.span.clone()];
        result.extend_from_slice(text);
    }
    
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

    #[rstest]
    #[case(b"foo = bar", vec![
        LosslessToken::Unquoted,
        LosslessToken::Whitespace,
        LosslessToken::Equal,
        LosslessToken::Whitespace,
        LosslessToken::Unquoted,
    ])]
    fn test_basic_tokens(#[case] input: &[u8], #[case] expected_tokens: Vec<LosslessToken>) {
        let tokens_with_spans = collect_tokens_with_spans(input).unwrap();
        let tokens: Vec<_> = tokens_with_spans.iter().map(|t| t.token.clone()).collect();
        
        assert_eq!(tokens, expected_tokens);
        
        // Test lossless property
        let reconstructed = reconstruct_input(input, &tokens_with_spans);
        assert_eq!(reconstructed, input);
    }
    
    #[rstest]
    #[case(b"a==b c<=d e>=f g!=h i?=j k<l m>n", vec![
        LosslessToken::Unquoted, LosslessToken::Exact, LosslessToken::Unquoted,
        LosslessToken::Whitespace,
        LosslessToken::Unquoted, LosslessToken::LessThanEqual, LosslessToken::Unquoted,
        LosslessToken::Whitespace,
        LosslessToken::Unquoted, LosslessToken::GreaterThanEqual, LosslessToken::Unquoted,
        LosslessToken::Whitespace,
        LosslessToken::Unquoted, LosslessToken::NotEqual, LosslessToken::Unquoted,
        LosslessToken::Whitespace,
        LosslessToken::Unquoted, LosslessToken::Exists, LosslessToken::Unquoted,
        LosslessToken::Whitespace,
        LosslessToken::Unquoted, LosslessToken::LessThan, LosslessToken::Unquoted,
        LosslessToken::Whitespace,
        LosslessToken::Unquoted, LosslessToken::GreaterThan, LosslessToken::Unquoted,
    ])]
    fn test_operators(#[case] input: &[u8], #[case] expected_tokens: Vec<LosslessToken>) {
        let tokens_with_spans = collect_tokens_with_spans(input).unwrap();
        let tokens: Vec<_> = tokens_with_spans.iter().map(|t| t.token.clone()).collect();
        
        assert_eq!(tokens, expected_tokens);
        
        // Test lossless property
        let reconstructed = reconstruct_input(input, &tokens_with_spans);
        assert_eq!(reconstructed, input);
    }

    // Helper function to test lossless property for any input
    fn test_lossless(input: &[u8]) {
        let tokens_with_spans = collect_tokens_with_spans(input).unwrap();
        let reconstructed = reconstruct_input(input, &tokens_with_spans);
        assert_eq!(reconstructed, input, "Failed lossless test for: {:?}", 
                   std::str::from_utf8(input).unwrap_or("invalid utf8"));
    }

    #[rstest]
    #[case(br#"name = "hello \"world\"""#, vec![
        LosslessToken::Unquoted, LosslessToken::Whitespace, LosslessToken::Equal, 
        LosslessToken::Whitespace, LosslessToken::Quoted
    ])]
    fn test_quoted_strings(#[case] input: &[u8], #[case] expected_tokens: Vec<LosslessToken>) {
        let tokens_with_spans = collect_tokens_with_spans(input).unwrap();
        let tokens: Vec<_> = tokens_with_spans.iter().map(|t| t.token.clone()).collect();
        
        assert_eq!(tokens, expected_tokens);
        test_lossless(input);
    }

    #[rstest]
    #[case(b"# This is a comment\nfoo = bar\t# Another comment\n", vec![
        LosslessToken::Comment, LosslessToken::Whitespace,
        LosslessToken::Unquoted, LosslessToken::Whitespace, LosslessToken::Equal, LosslessToken::Whitespace,
        LosslessToken::Unquoted, LosslessToken::Whitespace, LosslessToken::Comment, LosslessToken::Whitespace
    ])]
    fn test_comments_and_whitespace(#[case] input: &[u8], #[case] expected_tokens: Vec<LosslessToken>) {
        let tokens_with_spans = collect_tokens_with_spans(input).unwrap();
        let tokens: Vec<_> = tokens_with_spans.iter().map(|t| t.token.clone()).collect();
        
        assert_eq!(tokens, expected_tokens);
        test_lossless(input);
    }

    #[rstest]
    #[case(b"@planet_standard_scale = @default_window_name", vec![
        LosslessToken::Variable, LosslessToken::Whitespace, LosslessToken::Equal, 
        LosslessToken::Whitespace, LosslessToken::Variable
    ])]
    fn test_variables(#[case] input: &[u8], #[case] expected_tokens: Vec<LosslessToken>) {
        let tokens_with_spans = collect_tokens_with_spans(input).unwrap();
        let tokens: Vec<_> = tokens_with_spans.iter().map(|t| t.token.clone()).collect();
        
        assert_eq!(tokens, expected_tokens);
        test_lossless(input);
    }

    #[test]
    fn test_expressions() {
        let input = b"position = { @[1-leopard_x] @leopard_y }";
        test_lossless(input);
        
        // Verify we have an Expression token
        let tokens_with_spans = collect_tokens_with_spans(input).unwrap();
        let has_expression = tokens_with_spans.iter().any(|t| matches!(t.token, LosslessToken::Expression(_)));
        assert!(has_expression, "Should contain an Expression token");
    }

    #[test]
    fn test_complex_expressions() {
        let input = b"my_calc = @[(-half-half)*half]";
        test_lossless(input);
        
        // Verify we have an Expression token
        let tokens_with_spans = collect_tokens_with_spans(input).unwrap();
        let has_expression = tokens_with_spans.iter().any(|t| matches!(t.token, LosslessToken::Expression(_)));
        assert!(has_expression, "Should contain an Expression token");
    }

    #[test]
    fn test_bom_stripping() {
        let input = b"\xef\xbb\xbf# UTF-8 BOM test\nfoo = bar";
        let tokens_with_spans = collect_tokens_with_spans(input).unwrap();
        let reconstructed = reconstruct_input(input, &tokens_with_spans);
        
        // BOM is stripped during lexing, so reconstructed input won't include BOM
        // This is expected behavior since we removed LosslessToken::Bom variant
        let expected_without_bom = b"# UTF-8 BOM test\nfoo = bar";
        assert_eq!(reconstructed, expected_without_bom, "Reconstructed should match input without BOM");
        
        // The first token should be Comment (BOM was stripped before lexing)
        assert_eq!(tokens_with_spans[0].token, LosslessToken::Comment);
        assert_eq!(tokens_with_spans[0].span, 3..19); // BOM offset handled in span
        
        // Verify lexer properly handles BOM without producing separate token
        let input_without_bom = b"# UTF-8 BOM test\nfoo = bar";
        let tokens_without_bom = collect_tokens_with_spans(input_without_bom).unwrap();
        
        // Both inputs should produce the same tokens (just different spans)
        assert_eq!(tokens_with_spans.len(), tokens_without_bom.len());
        for (with_bom, without_bom) in tokens_with_spans.iter().zip(tokens_without_bom.iter()) {
            assert_eq!(with_bom.token, without_bom.token);
        }
    }

    #[test]
    fn test_lossless_token_size() {
        // Test that LosslessToken has a reasonable size
        // This is important for memory efficiency since we create many tokens
        let size = std::mem::size_of::<LosslessToken>();
        
        // Should be reasonable size - exact size depends on SmallVec internal layout
        // but should be significantly smaller than a Vec<ExpressionToken> would be
        println!("LosslessToken size: {} bytes", size);
        
        // Ensure it's not unreasonably large (less than 64 bytes)
        assert!(size <= 64, "LosslessToken size ({} bytes) should be <= 64 bytes", size);
        
        // Also test ExpressionToken size
        let expr_size = std::mem::size_of::<ExpressionToken>();
        println!("ExpressionToken size: {} bytes", expr_size);
        
        // ExpressionToken should be small since it's Copy
        assert!(expr_size <= 8, "ExpressionToken size ({} bytes) should be <= 8 bytes", expr_size);
    }

    #[rstest]
    #[case(b"[[scaled_skill] code here ] [[!var_name] other code ]")]
    #[case(b"stats={{id=0 type=general} {id=1 type=admiral}}")]
    fn test_lossless_samples(#[case] input: &[u8]) {
        test_lossless(input);
    }

    #[rstest]
    #[case(b"foo = bar")]
    #[case(b"open={1 2}")]
    #[case(b"field1=-100.535")]
    #[case(br#""foo"="bar" "3"="1444.11.11""#)]
    #[case(br#"custom_name="THE !@#$%^&*( '\"LEGION\"')""#)]
    #[case(b"foo{bar=qux}")]
    #[case(b"foo=abc#def\nbar=qux")]
    #[case(b"flavor_tur.8=yes")]
    #[case(b"dashed-identifier=yes")]
    #[case(b"province_id = event_target:agenda_province")]
    #[case(b"mult = value:job_weights_research_modifier|JOB|head_researcher|")]
    #[case(b"@planet_standard_scale = 11")]
    #[case(b"window_name = @default_window_name")]
    #[case(b"value=\"win\"; a=b")]
    #[case(b"foo = 0.3;")]
    #[case(b"a = 1; b = 2;; c = 3;")]
    #[case(b";;;key = value;;;")]
    #[case(b"age > 16")]
    fn test_comprehensive_syntax(#[case] input: &[u8]) {
        test_lossless(input);
    }
}