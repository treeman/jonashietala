use crate::markup::syntax_highlight::*;
use itertools::{Itertools, MultiPeek};
use jotdown::{Attributes, Container, Event};

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

        let (lang, attrs) = match self.parent.next()? {
            Event::Start(Container::CodeBlock { language }, attrs) => (language, attrs),
            other => return Some(other),
        };

        let lang = parse_code_spec(lang);
        let path = attrs.get("path").map(|x| x.to_string());
        let highlight_lines = attrs.get("hl").map(|x| {
            parse_line_highlight_spec(x.to_string().as_str())
                .expect("Error parsing `hl` code block attribute")
        });
        let linenum_start = attrs.get("linenum").map(|x| {
            x.to_string()
                .parse::<u32>()
                .expect("Error parsing `linenum` not a number")
        });

        let mut code = String::new();
        // Next should eat the End event as well.
        while let Some(Event::Str(text)) = self.parent.next() {
            code.push_str(&text);
        }

        let mut res = String::new();

        Code::Block {
            code: code.as_str(),
            lang: lang.as_deref(),
            path: path.as_deref(),
            linenum_start,
            highlight_lines,
        }
        .push(&mut res);

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

        let attrs = match self.parent.next()? {
            Event::Start(Container::Verbatim, attrs) => attrs,
            other => return Some(other),
        };

        let lang = match attrs.get("lang") {
            Some(lang) => lang.to_string(),
            None => return Some(Event::Start(Container::Verbatim, attrs)),
        };

        // Now lets eat it up!
        let mut code = String::new();
        loop {
            match self.parent.next()? {
                Event::End(Container::Verbatim) => break,
                Event::Str(text) => code.push_str(&text),
                x => {
                    panic!("Unexpected event: {x:?}");
                }
            }
        }

        let mut res = String::new();
        Code::Inline {
            code: &code,
            lang: Some(&lang),
        }
        .push(&mut res);

        let html = Container::RawBlock { format: "html" };

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
        assert!(res.starts_with(r#"<div class="code-wrapper"><div class="descr" data-descr="rust"></div><pre><code class="highlight rust">"#));
        assert!(res.ends_with("</code></pre></div>"));
        Ok(())
    }

    #[test]
    fn test_highlight_empty_code_block() -> Result<()> {
        let s = r"
```rust
```";
        let res = convert(s)?;
        assert!(res.starts_with(r#"<div class="code-wrapper"><div class="descr" data-descr="rust"></div><pre><code class="highlight rust">"#));
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
        assert!(res.starts_with(r#"<div class="code-wrapper"><div class="descr" data-descr="ml"></div><pre><code class="highlight ml">"#));
        assert!(res.ends_with("</code></pre></div>"));
        Ok(())
    }

    #[test]
    fn test_highlight_inline_code() -> Result<()> {
        // Text
        // ..
        // Code
        // Text("rust end")
        let s = r"Inline `let x = 2;`{lang=rust} end";
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
        let s = r"`x->y`{lang=c}";
        let res = convert(s)?;
        assert_eq!(res, "<p>\n<code class=\"highlight c\">x<span class=\"punctuation accessor c\">-&gt;</span>y</code></p>");
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
        assert_eq!(
            res,
            r#"<div class="code-wrapper"><div class="descr" data-descr="c"></div><pre><code class="highlight c"><div class="line">x<span class="punctuation accessor c">-&gt;</span>y
</div></code></pre></div>"#
        );
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

    #[test]
    fn test_code_path_attr_lang() -> Result<()> {
        let s = r#"
{path="file.rs"}
```rust
let x = 2;
```"#;
        let res = convert(s)?;
        assert_eq!(
            res,
            r#"<div class="code-wrapper"><div class="descr" data-descr="file.rs"></div><pre><code class="highlight rust"><div class="line"><span class="storage type rust">let</span> x <span class="keyword operator rust">=</span> <span class="constant numeric integer decimal rust">2</span><span class="punctuation terminator rust">;</span>
</div></code></pre></div>"#
        );
        Ok(())
    }

    #[test]
    fn test_code_path_attr_no_lang() -> Result<()> {
        let s = r#"
{path="file"}
```
Text
```"#;
        let res = convert(s)?;
        assert_eq!(
            res,
            r#"<div class="code-wrapper"><div class="descr" data-descr="file"></div><pre><code><div class="line">Text
</div></code></pre></div>"#
        );
        Ok(())
    }

    #[test]
    fn test_code_highlight_line_text() -> Result<()> {
        let s = r#"
    {hl="2,4..5"}
    ```
    1
    2
    3
    4
    5
    6
    ```"#;
        let res = convert(s)?;
        assert_eq!(
            res,
            r#"<div class="code-wrapper"><pre><code><div class="line">1
</div><div class="line hl">2
</div><div class="line">3
</div><div class="line hl">4
</div><div class="line hl">5
</div><div class="line">6
</div></code></pre></div>"#
        );
        Ok(())
    }
}
