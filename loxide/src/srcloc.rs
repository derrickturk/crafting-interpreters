/// A source location (line and character position)
#[derive(Copy, Clone, Debug)]
pub struct SrcLoc {
    pub line: usize,
    pub pos: usize,
}
