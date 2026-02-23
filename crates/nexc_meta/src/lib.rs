use std::collections::BTreeMap;
use std::fs;
use std::path::Path;

use nexc_diag::{Diagnostic, Severity};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExportedSymbol {
    pub name: String,
    pub kind: String,
    pub signature: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Signature {
    pub name: String,
    pub text: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NexMeta {
    pub module: String,
    pub compiler_version: String,
    pub signatures: Vec<Signature>,
    pub exported_symbols: Vec<ExportedSymbol>,
    pub layout_hashes: BTreeMap<String, String>,
    pub dependency_hashes: Vec<String>,
    pub source_hash: String,
    pub warnings: u32,
}

pub fn write_meta(meta: &NexMeta, path: &Path) -> Result<(), Diagnostic> {
    let payload = serde_json::to_string_pretty(meta).map_err(|err| Diagnostic {
        id: "meta_serialize".to_string(),
        severity: Severity::Error,
        span: None,
        file: Some(path.to_path_buf()),
        message: err.to_string(),
        notes: Vec::new(),
        suggestions: Vec::new(),
    })?;
    fs::write(path, payload).map_err(|err| Diagnostic {
        id: "meta_write".to_string(),
        severity: Severity::Error,
        span: None,
        file: Some(path.to_path_buf()),
        message: err.to_string(),
        notes: Vec::new(),
        suggestions: Vec::new(),
    })?;
    Ok(())
}

pub fn read_meta(path: &Path) -> Result<NexMeta, Diagnostic> {
    let text = fs::read_to_string(path).map_err(|err| Diagnostic {
        id: "meta_read".to_string(),
        severity: Severity::Error,
        span: None,
        file: Some(path.to_path_buf()),
        message: err.to_string(),
        notes: Vec::new(),
        suggestions: Vec::new(),
    })?;
    serde_json::from_str(&text).map_err(|err| Diagnostic {
        id: "meta_parse".to_string(),
        severity: Severity::Error,
        span: None,
        file: Some(path.to_path_buf()),
        message: err.to_string(),
        notes: Vec::new(),
        suggestions: Vec::new(),
    })
}

pub fn read_meta_if_exists(path: &Path) -> Result<Option<NexMeta>, Diagnostic> {
    if path.exists() {
        Ok(Some(read_meta(path)?))
    } else {
        Ok(None)
    }
}

