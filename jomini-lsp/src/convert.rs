//! Conversions between jomini's `lint` types and `lsp_types`.
//!
//! Everything here is a pure function of owned data plus a [`LineIndex`] (to turn
//! byte ranges into LSP line/character ranges), so it has no dependency on a live
//! parse tree.

use crate::line_index::LineIndex;
use jomini::text::lint::{Diagnostic, Severity};
use lsp_types::{DiagnosticSeverity, NumberOrString, Range, SymbolKind};

/// A jomini half-open byte range → an LSP range, positioned with the target
/// document's [`LineIndex`].
pub fn to_range(li: &LineIndex, range: (u32, u32)) -> Range {
    Range {
        start: li.position(range.0),
        end: li.position(range.1),
    }
}

/// Map a lint [`Severity`] onto LSP's four-level scale. jomini has two extra
/// rungs (`Untidy`, `Fatal`); they fold onto the nearest LSP level.
pub fn to_severity(sev: Severity) -> DiagnosticSeverity {
    match sev {
        Severity::Tip => DiagnosticSeverity::HINT,
        Severity::Untidy => DiagnosticSeverity::INFORMATION,
        Severity::Warning => DiagnosticSeverity::WARNING,
        Severity::Error | Severity::Fatal => DiagnosticSeverity::ERROR,
    }
}

/// A lint [`Diagnostic`] → an LSP diagnostic, positioned via `li` (the
/// [`LineIndex`] of the file the diagnostic belongs to). The stable lint id
/// becomes the `code`; the `help` note — which LSP has no dedicated field for —
/// is appended to the message.
pub fn to_lsp_diagnostic(li: &LineIndex, d: &Diagnostic) -> lsp_types::Diagnostic {
    let mut message = d.message.clone();
    if let Some(help) = &d.help {
        message.push('\n');
        message.push_str(help);
    }
    lsp_types::Diagnostic {
        range: to_range(li, d.range),
        severity: Some(to_severity(d.severity)),
        code: Some(NumberOrString::String(d.id.to_string())),
        source: Some("jomini".to_string()),
        message,
        ..Default::default()
    }
}

/// The LSP symbol kind for a lint entity kind. Definitions are name-keyed game
/// entities (buildings, etc.); `OBJECT` is the neutral choice. A richer per-game
/// schema could map specific kinds (e.g. event → `EVENT`).
pub fn symbol_kind(_kind: &str) -> SymbolKind {
    SymbolKind::OBJECT
}
