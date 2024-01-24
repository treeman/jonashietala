use crate::paths::AbsPath;

pub enum RawMarkup {
    Markdown(String),
    Djot(String),
}

impl RawMarkup {
    pub fn from_file(path: AbsPath) -> Result<Self> {}
}
