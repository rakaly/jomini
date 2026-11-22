//! Run `pdx fmt` on small files and check its output, stderr, and exit code.

use std::path::PathBuf;
use std::process::Command;

struct Run {
    stdout: Vec<u8>,
    stderr: String,
    code: Option<i32>,
}

/// Write `data` to a file named `name` in a directory of this test, and run
/// `pdx fmt` on it. Return the path of the file too, as stderr has it.
fn fmt(test: &str, name: &str, data: &[u8]) -> (Run, String) {
    let dir: PathBuf = std::env::temp_dir().join(format!("pdx-fmt-{}-{test}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join(name);
    std::fs::write(&path, data).unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_pdx"))
        .arg("fmt")
        .arg(&path)
        .output()
        .unwrap();
    std::fs::remove_dir_all(&dir).unwrap();
    let run = Run {
        stdout: output.stdout,
        stderr: String::from_utf8(output.stderr).unwrap(),
        code: output.status.code(),
    };
    (run, path.display().to_string())
}

#[test]
fn keeps_a_game_txt_header() {
    let (run, _) = fmt(
        "eu4txt",
        "a.eu4",
        b"EU4txt\r\ndate=1444.11.11\nplayer=\"FRA\"",
    );
    assert_eq!(
        run.stdout,
        b"EU4txt\r\ndate = 1444.11.11\nplayer = \"FRA\"\n"
    );
    assert_eq!(run.stderr, "");
    assert_eq!(run.code, Some(0));
}

#[test]
fn keeps_a_text_save_header() {
    let source = b"SAV01004da228250004e772\nmetadata={\ndate=1337.4.1 }";
    let (run, _) = fmt("sav", "a.ck3", source);
    let expected = b"SAV01004da228250004e772\nmetadata = {\n\tdate = 1337.4.1\n}\n";
    assert_eq!(run.stdout, expected);
    assert_eq!(run.stderr, "");
    assert_eq!(run.code, Some(0));
}

#[test]
fn reports_syntax_errors_and_writes_the_output() {
    let (run, path) = fmt(
        "errors",
        "a.eu4",
        b"EU4txt\na = { b = 1 } }\nc = {\n\td = 2\n",
    );
    assert_eq!(
        run.stdout,
        b"EU4txt\na = {\n\tb = 1\n}\n}\nc = {\n\td = 2\n"
    );
    assert_eq!(
        run.stderr,
        format!("{path}:2:15: unmatched '}}'\n{path}:5:1: unclosed '{{' (opened at 3:5)\n")
    );
    assert_eq!(run.code, Some(1));
}

#[test]
fn counts_columns_after_a_bom() {
    let (run, path) = fmt("bom", "a.txt", b"\xEF\xBB\xBFa = }");
    assert_eq!(run.stdout, b"\xEF\xBB\xBFa =\n}\n");
    assert_eq!(
        run.stderr,
        format!("{path}:1:3: operator has no value\n{path}:1:5: unmatched '}}'\n")
    );
    assert_eq!(run.code, Some(1));
}

#[test]
fn refuses_binary_and_compressed_files() {
    let cases: [(&str, &[u8], &str); 3] = [
        ("zip", b"PK\x03\x04\x14\x00", "a zip file"),
        (
            "binary-save",
            b"SAV01014da228250004e772\n\x01\x00\x03\x00",
            "a save with a Binary header",
        ),
        ("nul", b"EU4bin\x01\x00\x03\x00", "a NUL byte at offset 7"),
    ];
    for (test, data, message) in cases {
        let (run, _) = fmt(test, "a.eu4", data);
        assert_eq!(run.stdout, b"", "{test}");
        let message = format!("{message} is not plain text; melt it first");
        assert_eq!(run.stderr, format!("Error: {message}\n"), "{test}");
        assert_eq!(run.code, Some(1), "{test}");
    }
}

#[test]
fn counts_the_errors_after_the_first_twenty() {
    for (count, rest) in [(21, "1 more syntax error"), (22, "2 more syntax errors")] {
        let (run, path) = fmt("many", "a.txt", "}\n".repeat(count).as_bytes());
        let mut expected: String = (1..=20)
            .map(|line| format!("{path}:{line}:1: unmatched '}}'\n"))
            .collect();
        expected.push_str(&format!("{path}: {rest}\n"));
        assert_eq!(run.stderr, expected);
        assert_eq!(run.code, Some(1));
    }
}
