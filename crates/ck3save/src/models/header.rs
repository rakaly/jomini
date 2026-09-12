use crate::Ck3Date;
use serde::Deserialize;

#[derive(Debug, Deserialize)]
pub struct Header {
    pub meta_data: Metadata,
}

#[derive(Debug, Deserialize)]
pub struct Metadata {
    pub version: String,
    pub meta_date: Ck3Date,
}
