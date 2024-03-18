use crate::markup::syntax_highlight::*;
use colored::Colorize;
use itertools::{Itertools, MultiPeek};
use jotdown::{html, Attributes, Container, Event, Render};
use tracing::warn;

pub struct CodeBlockSyntaxHighlight<'a, I: Iterator<Item = Event<'a>>> {
    parent: I,
    event_queue: Vec<Event<'a>>,
}

impl<'a, I: Iterator<Item = Event<'a>>> CodeBlockSyntaxHighlight<'a, I> {
    pub fn new(parent: I) -> Self {
        Self {
            parent,
            event_queue: vec![],
        }
    }
}

impl<'a, I: Iterator<Item = Event<'a>>> Iterator for CodeBlockSyntaxHighlight<'a, I> {
    type Item = Event<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(event) = self.event_queue.pop() {
            return Some(event);
        }

        let (lang, _attrs) = match self.parent.next()? {
            Event::Start(Container::CodeBlock { language }, attrs) => (language, attrs),
            other => return Some(other),
        };

        let lang = parse_code_spec(&lang);

        let mut code = String::new();
        // Next should eat the End event as well.
        while let Some(Event::Str(text)) = self.parent.next() {
            code.push_str(&text);
        }

        let mut res = String::new();

        push_code_block(&mut res, lang, &code);

        let html = Container::RawBlock { format: "html" };
        self.event_queue.push(Event::End(html.clone()));
        self.event_queue.push(Event::Str(res.into()));
        Some(Event::Start(html, Attributes::new()))
    }
}

pub struct InlineCodeSyntaxHighlight<'a, I: Iterator<Item = Event<'a>>> {
    parent: MultiPeek<I>,
    event_queue: Vec<Event<'a>>,
}

impl<'a, I: Iterator<Item = Event<'a>>> InlineCodeSyntaxHighlight<'a, I> {
    pub fn new(parent: I) -> Self {
        Self {
            parent: parent.multipeek(),
            event_queue: vec![],
        }
    }
}

impl<'a, I: Iterator<Item = Event<'a>>> Iterator for InlineCodeSyntaxHighlight<'a, I> {
    type Item = Event<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(event) = self.event_queue.pop() {
            return Some(event);
        }

        let start = match self.parent.next()? {
            start @ Event::Start(Container::Verbatim, _) => start,
            other => return Some(other),
        };

        // We need to peek directly after the verbatim.
        loop {
            if let Some(Event::End(Container::Verbatim)) = self.parent.peek() {
                break;
            }
        }

        match self.parent.peek() {
            Some(Event::Str(txt)) => {
                if !INLINE_CODE_SPEC.is_match(txt) {
                    return Some(start);
                }
            }
            _ => return Some(start),
        }

        // Now lets eat it up!
        let mut code = String::new();
        loop {
            match self.parent.next()? {
                Event::End(Container::Verbatim) => break,
                Event::Str(text) => code.push_str(&text),
                x => {
                    warn!("Unexpected event: {x:?}");
                    return Some(start);
                }
            }
        }

        let txt = match self.parent.next()? {
            Event::Str(txt) => txt,
            other => panic!("Should have peeked a surefire match, got: {other:?}"),
        };

        let (lang, trailing) = match inline_code_spec(&txt) {
            Some(x) => x,
            None => panic!(
                "Inline code spec not matched: {}",
                format!("`{code}`{txt}").red()
            ),
        };

        let mut res = String::new();
        push_code_inline(&mut res, &lang, &code);

        let html = Container::RawBlock { format: "html" };

        if !trailing.is_empty() {
            self.event_queue.push(Event::Str(trailing.into()));
        }

        self.event_queue.push(Event::End(html.clone()));
        self.event_queue.push(Event::Str(res.into()));
        Some(Event::Start(html, Attributes::new()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use eyre::Result;
    use jotdown::{html, Parser, Render};

    fn convert(s: &str) -> Result<String> {
        let parser = Parser::new(s);
        let transformed = CodeBlockSyntaxHighlight::new(parser);
        let transformed = InlineCodeSyntaxHighlight::new(transformed);
        let mut body = String::new();
        html::Renderer::default().push(transformed, &mut body)?;
        Ok(body.trim().into())
    }

    #[test]
    fn test_highlight_code_block() -> Result<()> {
        let s = r"
```rust
let x = 2;
```";
        let res = convert(s)?;
        assert!(res.starts_with(r#"<div class="code-wrapper"><div class="lang rust" data-lang="rust"></div><pre><code class="highlight rust">"#));
        assert!(res.ends_with("</code></pre></div>"));
        Ok(())
    }

    #[test]
    fn test_highlight_empty_code_block() -> Result<()> {
        let s = r"
```rust
```";
        let res = convert(s)?;
        assert!(res.starts_with(r#"<div class="code-wrapper"><div class="lang rust" data-lang="rust"></div><pre><code class="highlight rust">"#));
        assert!(res.ends_with("</code></pre></div>"));
        Ok(())
    }

    #[test]
    fn test_highlight_extension() -> Result<()> {
        let s = r"
```ml
let square x = x * x
```";
        let res = convert(s)?;
        assert!(res.starts_with(r#"<div class="code-wrapper"><div class="lang ml" data-lang="ml"></div><pre><code class="highlight ml">"#));
        assert!(res.ends_with("</code></pre></div>"));
        Ok(())
    }

    #[test]
    fn test_highlight_inline_code() -> Result<()> {
        // Text
        // ..
        // Code
        // Text("rust end")
        let s = r"Inline `let x = 2;`rust end";
        let res = convert(s)?;
        assert!(res.starts_with(
            r#"<p>Inline 
<code class="highlight rust">"#
        ));
        assert!(res.ends_with(r#"</code> end</p>"#));
        Ok(())
    }

    #[test]
    fn test_highlight_inline_code_no_escape() -> Result<()> {
        // Text
        // ..
        // Code
        // Text("rust end")
        let s = r"`x->y`c";
        let res = convert(s)?;
        assert_eq!(res, "<p>\n<code class=\"highlight c\"><span class=\"source c\">x<span class=\"punctuation accessor c\">-&gt;</span>y</span></code></p>");
        Ok(())
    }

    #[test]
    fn test_highlight_code_block_no_escape() -> Result<()> {
        // Text
        // ..
        // Code
        // Text("rust end")
        let s = r"```c
x->y
```";
        let res = convert(s)?;
        assert_eq!(res, "<div class=\"code-wrapper\"><div class=\"lang c\" data-lang=\"c\"></div><pre><code class=\"highlight c\"><span class=\"source c\">x<span class=\"punctuation accessor c\">-&gt;</span>y\n</span></code></pre></div>");
        Ok(())
    }

    #[test]
    fn test_highlight_inline_code_no_lang() -> Result<()> {
        // Text
        // ..
        // Code
        // Text(" no lang")
        let s = r"Inline `xxx` no lang";
        let res = convert(s)?;
        assert_eq!(res, "<p>Inline <code>xxx</code> no lang</p>");
        Ok(())
    }
}
