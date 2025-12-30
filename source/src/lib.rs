use std::{cell::OnceCell, num::NonZero, ops::Range, rc::Rc};

mod lines;
pub use lines::*;

/// A cheaply [Clone]able container for source code.
///
/// Allows for efficiently mapping from byte offsets to line and column values.
#[derive(Clone, Debug)]
pub struct SourceFile(Rc<SourceStorage>);

impl SourceFile {
    #[must_use]
    pub fn new(path: String, text: String) -> Self {
        let storage = SourceStorage::new(path, text);
        Self(Rc::new(storage))
    }
    #[must_use]
    pub fn path(&self) -> &str {
        &self.0.path
    }
    #[must_use]
    pub fn text(&self) -> &str {
        &self.0.text
    }
    #[must_use]
    pub fn span_to_pos(&self, span: &Span) -> [HumanFilePos; 2] {
        [span.start, span.end.saturating_sub(1)].map(|o| self.offset_to_pos(o))
    }
    /// Panics if the provided offset is out of bounds of the text.
    #[must_use]
    pub fn offset_to_pos(&self, offset: usize) -> HumanFilePos {
        assert!(
            offset <= self.text().len(),
            "Cannot calculate an offset outside the file"
        );
        let starts = self.0.line_starts();
        let line = starts
            .binary_search(&offset)
            .unwrap_or_else(|l| l.saturating_sub(1));
        let line_start = starts.get(line).copied().unwrap_or(0);
        let column_bytes = u32::try_from(offset.checked_sub(line_start).unwrap()).unwrap();
        let column = self
            .text()
            .get(line_start..offset)
            // Count UTF-8 chars for columns
            // Fall back to byte columns if UTF-8 access fails
            .map_or(column_bytes, |s| u32::try_from(s.chars().count()).unwrap());
        // NOTE: Let's assume line and column counts > 2^32-1 won't be encountered *that* often.
        let line = NonZero::new(u32::try_from(line + 1).unwrap()).unwrap();
        let column = NonZero::new(column + 1).unwrap();
        HumanFilePos {
            line,
            column_utf8: column,
            column_bytes,
        }
    }
    #[must_use]
    pub fn lines(&self) -> LineIterator<'_> {
        let starts = self.0.line_starts();
        LineIterator {
            text: self.text(),
            line_starts: starts,
            remaining: 0..starts.len() + 1,
        }
    }
}

pub type Span = Range<usize>;

/// A human-readable position in a file
/// line and column both start at 1
#[derive(Debug, Clone, Copy)]
pub struct HumanFilePos {
    line: NonZero<u32>,
    column_utf8: NonZero<u32>,
    column_bytes: u32,
}

impl HumanFilePos {
    #[must_use]
    pub const fn line(&self) -> usize {
        self.line.get() as usize
    }
    #[must_use]
    pub const fn line_0idx(&self) -> usize {
        self.line.get() as usize - 1
    }
    #[must_use]
    pub const fn col(&self) -> usize {
        self.column_utf8.get() as usize
    }
    #[must_use]
    pub const fn col_bytes(&self) -> usize {
        self.column_bytes as usize
    }
}

#[derive(Debug)]
struct SourceStorage {
    text: String,
    path: String,
    line_starts: OnceCell<Box<[usize]>>,
}

impl SourceStorage {
    #[must_use]
    pub const fn new(path: String, text: String) -> Self {
        Self {
            text,
            path,
            line_starts: OnceCell::new(),
        }
    }
    fn line_starts(&self) -> &[usize] {
        self.line_starts.get_or_init(|| {
            let newlines = self
                .text
                .bytes()
                .enumerate()
                .filter(|&(_, b)| b == b'\n')
                .map(|(idx, _)| idx + 1);
            core::iter::once(0).chain(newlines).collect()
        })
    }
}
