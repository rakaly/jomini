//! Expose data within Paradox save file envelopes.
//!
//! # Supported Games
//!
//! - Europa Universalis 5 (EU5)
//! - Victoria 3 (Vic3)
//! - Crusader Kings 3 (CK3)
//! - Imperator: Rome
//!
//! Notable games NOT using this format:
//! - Europa Universalis 4 (EU4)
//! - Hearts of Iron 4 (HOI4)
//!
//! This module provides uniform interfaces for working with both compressed
//! (ZIP-based) and uncompressed save files. It automatically detects the format
//! and handles decompression transparently, exposing the raw gamestate and
//! metadata in a type-safe way.
//!
//! Modern Paradox games use a structured save file format consisting of three
//! parts:
//!
//! 1. Header - A 24 byte header containing format version, encoding type, and
//!    metadata length
//! 2. Metadata - Game session information (date, version)
//! 3. Gamestate - The actual save data (countries, provinces, characters, etc.)
//!
//! Save files can be either:
//! - Uncompressed - Header, metadata, and gamestate stored sequentially in
//!   plaintext or binary format
//! - Compressed - Header followed by an optional metadata and then a ZIP
//!   archive. If the metadata is found within the zip, it takes precedence.
//!
//! # Architecture Overview
//!
//! The envelope module provides a type-safe interface for accessing save file
//! data without needing to understand the underlying compression format. All
//! operations work uniformly whether the file is compressed or uncompressed.
//!
//! ```text
//! JominiFile
//! ├── Handles both compressed (ZIP) and uncompressed saves
//! ├── Provides gamestate() → SaveContentKind
//! └── Provides meta() → SaveMetadataKind
//!
//! SaveContentKind (dynamic encoding dispatch)
//! ├── Text(SaveContent<TextEncoding, R>)
//! └── Binary(SaveContent<BinaryEncoding, R>)
//!
//! SaveMetadataKind (dynamic encoding dispatch)
//! ├── Text(SaveMetadata<TextEncoding, R>)
//! └── Binary(SaveMetadata<BinaryEncoding, R>)
//! ```

mod errors;
mod file;
mod header;

pub use errors::*;
pub use file::*;
pub use header::*;
pub use rawzip::ReaderAt;
