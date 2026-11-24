//! Run `pdx json` on small plain text saves and check its output, stderr, and
//! exit code.

use std::process::Command;

fn json(test: &str, data: &[u8]) -> (String, String, Option<i32>) {
    let dir = std::env::temp_dir().join(format!("pdx-json-{}-{test}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("a.eu4");
    std::fs::write(&path, data).unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_pdx"))
        .arg("json")
        .arg(&path)
        .output()
        .unwrap();
    std::fs::remove_dir_all(&dir).unwrap();
    (
        String::from_utf8(output.stdout).unwrap(),
        String::from_utf8(output.stderr).unwrap(),
        output.status.code(),
    )
}

#[test]
fn writes_the_document_after_the_header() {
    let (stdout, stderr, code) = json("valid", b"EU4txt\na=1 b={ {} 2 } c=3 { }");
    assert_eq!(stdout, r#"{"a":1,"b":[[],2],"c":3}"#);
    assert_eq!(stderr, "");
    assert_eq!(code, Some(0));
}

#[test]
fn reports_a_syntax_error_and_writes_the_output() {
    let (stdout, stderr, code) = json("invalid", b"EU4txt\na=1 b={ c=2");
    assert_eq!(stdout, r#"{"a":1,"b":{"c":2}}"#);
    assert!(
        stderr.starts_with("the document has a syntax error: "),
        "{stderr}"
    );
    assert_eq!(code, Some(1));
}
