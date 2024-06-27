use crate::markup::syntax_highlight::*;
use colored::Colorize;
use pulldown_cmark::{CodeBlockKind, Event, Tag};
use std::iter::Peekable;

pub struct CodeBlockSyntaxHighlight<'a, I: Iterator<Item = Event<'a>>> {
    parent: I,
}

impl<'a, I: Iterator<Item = Event<'a>>> CodeBlockSyntaxHighlight<'a, I> {
    pub fn new(parent: I) -> Self {
        Self { parent }
    }
}

impl<'a, I: Iterator<Item = Event<'a>>> Iterator for CodeBlockSyntaxHighlight<'a, I> {
    type Item = Event<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let lang = match self.parent.next()? {
            Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(lang))) => lang,
            other => return Some(other),
        };

        let lang = parse_code_spec(&lang);

        let mut code = String::new();
        // Next should eat the End event as well.
        while let Some(Event::Text(text)) = self.parent.next() {
            code.push_str(&text);
        }

        let mut res = String::new();

        CodeBlock {
            code: code.as_str(),
            lang: lang.as_deref(),
            path: None,
        }
        .push(&mut res);

        Some(Event::Html(res.into()))
    }
}

pub struct InlineCodeSyntaxHighlight<'a, I: Iterator<Item = Event<'a>>> {
    parent: Peekable<I>,
    trailing: Option<Event<'a>>,
}

impl<'a, I: Iterator<Item = Event<'a>>> InlineCodeSyntaxHighlight<'a, I> {
    pub fn new(parent: I) -> Self {
        Self {
            parent: parent.peekable(),
            trailing: None,
        }
    }
}

impl<'a, I: Iterator<Item = Event<'a>>> Iterator for InlineCodeSyntaxHighlight<'a, I> {
    type Item = Event<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.trailing.is_some() {
            return self.trailing.take();
        }

        let code = match self.parent.next()? {
            Event::Code(code) => code,
            other => return Some(other),
        };

        match self.parent.peek() {
            Some(Event::Text(txt)) => {
                if !INLINE_CODE_SPEC.is_match(txt) {
                    return Some(Event::Code(code));
                }
            }
            _ => return Some(Event::Code(code)),
        }

        let txt = match self.parent.next()? {
            Event::Text(txt) => txt,
            other => panic!("Should have peeked a surefire match, got: {other:?}"),
        };

        let (lang, trailing) = match inline_code_spec(&txt) {
            Some(x) => x,
            None => panic!(
                "Inline code spec not matched: {}",
                format!("`{code}`{txt}").red()
            ),
        };

        if !trailing.is_empty() {
            self.trailing = Some(Event::Text(trailing.into()));
        }

        let mut res = String::new();
        push_code_inline(&mut res, &lang, &code);
        Some(Event::Html(res.into()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pulldown_cmark::{html, Options, Parser};

    fn convert(s: &str) -> String {
        let parser = Parser::new_ext(s, Options::all());
        let transformed = CodeBlockSyntaxHighlight::new(parser);
        let transformed = InlineCodeSyntaxHighlight::new(transformed);
        let mut body = String::new();
        html::push_html(&mut body, transformed);
        body
    }

    #[test]
    fn test_highlight_code_block() {
        let s = r"
```rust
let x = 2;
```";
        let res = convert(s);
        assert!(res.starts_with(r#"<div class="code-wrapper"><div class="descr" data-descr="rust"></div><pre><code class="highlight rust">"#));
        assert!(res.trim_end().ends_with("</code></pre></div>"));
    }

    #[test]
    fn test_highlight_extension() {
        let s = r"
```ml
let square x = x * x
```";
        let res = convert(s);
        assert!(res.starts_with(r#"<div class="code-wrapper"><div class="descr" data-descr="ml"></div><pre><code class="highlight ml">"#));
        assert!(res.trim_end().ends_with("</code></pre></div>"));
    }

    #[test]
    fn test_highlight_inline_code() {
        // Text
        // ..
        // Code
        // Text("rust end")
        let s = r"Inline `let x = 2;`rust end";
        let res = convert(s);
        assert!(res.starts_with(r#"<p>Inline <code class="highlight rust">"#));
        assert!(res.trim_end().ends_with(r#"</code> end</p>"#));
    }

    #[test]
    fn test_highlight_inline_code_no_lang() {
        // Text
        // ..
        // Code
        // Text(" no lang")
        let s = r"Inline `xxx` no lang";
        let res = convert(s);
        assert_eq!(res.trim_end(), "<p>Inline <code>xxx</code> no lang</p>");
    }
}
