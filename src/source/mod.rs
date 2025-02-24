use std::{path::Path, rc::Rc};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FilePosition {
    pub filepath: Option<Rc<Path>>,
    pub line: usize,
    pub col: usize,
}

impl FilePosition {
    pub const fn new() -> Self {
        FilePosition {
            line: 1,
            col: 1,
            filepath: None,
        }
    }
    pub fn with_line(mut self, line: usize) -> Self {
        self.line = line;
        self
    }
    pub fn with_col(mut self, col: usize) -> Self {
        self.col = col;
        self
    }
    pub fn with_path(mut self, path: impl Into<Rc<Path>>) -> Self {
        self.filepath = Some(path.into());
        self
    }
    pub fn without_path(mut self) -> Self {
        self.filepath = None;
        self
    }
}

impl Default for FilePosition {
    fn default() -> Self {
        FilePosition::new()
    }
}
