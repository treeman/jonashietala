use jotdown::Event;

pub struct StripElements<'a, I: Iterator<Item = Event<'a>>> {
    parent: I,
}

impl<'a, I: Iterator<Item = Event<'a>>> StripElements<'a, I> {
    pub fn new(parent: I) -> Self {
        Self { parent }
    }
}

impl<'a, I: Iterator<Item = Event<'a>>> Iterator for StripElements<'a, I> {
    type Item = Event<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let e = self.parent.next()?;
        if skip(&e) {
            self.next()
        } else {
            Some(e)
        }
    }
}

fn skip(e: &Event<'_>) -> bool {
    !matches!(
        e,
        Event::Str(_)
            | Event::LeftSingleQuote
            | Event::RightSingleQuote
            | Event::LeftDoubleQuote
            | Event::RightDoubleQuote
            | Event::Ellipsis
            | Event::EnDash
            | Event::EmDash
            | Event::Escape
    )
}

#[cfg(test)]
mod tests {
    use super::super::djot_to_html_stripped;
    use eyre::Result;

    #[test]
    fn test_djot_to_html_stripped() -> Result<()> {
        let input = "# [melange-nvim][]: An amazing colorscheme";
        let output = djot_to_html_stripped(input)?;
        assert_eq!(output.0, "melange-nvim: An amazing colorscheme\n");

        Ok(())
    }
}
