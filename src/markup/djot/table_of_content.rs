use crate::markup::djot::djot_to_html_stripped;
use crate::markup::markup_lookup::{Element, MarkupLookup};
use lazy_static::lazy_static;
use regex::{Captures, Regex};
use std::borrow::Cow;

pub fn insert_toc<'a>(body: &'a str, lookup: &MarkupLookup) -> Cow<'a, str> {
    // Use a regex instead of relying on the parser because it might get offset in the output,
    // and we need to do a pass first to collect all headings before we insert the toc.
    TOC_SYMBOL.replace(body, |_caps: &Captures| {
        let mut res = r#"<div class="toc">
  <details>
    <summary>Contents</summary>
    <nav>
      <ol>
"#
        .to_string();

        for (_, x) in lookup.char_pos_to_element.iter() {
            if let Element::Heading(ref heading) = x.element {
                let class = match heading.level {
                    1 => "heading-1",
                    2 => "heading-2",
                    _ => continue,
                };
                res.push_str(&format!(
                    r##"<li><a href="#{}" class="{}">{}</a></li>
"##,
                    heading.id,
                    class,
                    djot_to_html_stripped(&format!("# {}", heading.content))
                        .expect("Failed to render Djot")
                        .0
                ));
            }
        }

        res.push_str(
            r#"      </ol>
    </nav>
  </details>
</div>"#,
        );
        res
    })
}

lazy_static! {
    pub static ref TOC_SYMBOL: Regex = Regex::new(r"(<p>:table-of-content:</p>)").unwrap();
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::markup::djot::drop_offset::DropOffset;
    use crate::markup::djot::lookup_register::LookupRegister;
    use crate::markup::ParseContext;
    use eyre::Result;
    use jotdown::{html, Parser, Render};
    use std::cell::RefCell;
    use std::rc::Rc;

    fn convert(s: &str) -> Result<String> {
        let lookup = Rc::new(RefCell::new(MarkupLookup::new(s, 0)));
        let parser = Parser::new(s).into_offset_iter();
        let transformed = LookupRegister::new(parser, s, lookup.clone(), ParseContext::default());
        let transformed = DropOffset::new(transformed);
        let mut body = String::new();
        html::Renderer::default().push(transformed, &mut body)?;

        let lookup = Rc::try_unwrap(lookup)
            .expect("Should be able to unwrap lookup")
            .into_inner();

        body = insert_toc(&body, &lookup).to_string();

        Ok(body)
    }

    #[test]
    fn test_toc() -> Result<()> {
        let s = r#"
:table-of-content:

# First

## Nested

# With a [link][]

## [Def]: link like a definition
"#;
        let s = convert(s)?;
        println!("{}", s);

        assert_eq!(
            s,
            r##"
<div class="toc">
  <details>
    <summary>Contents</summary>
    <nav>
      <ol>
<li><a href="#First" class="heading-1">First
</a></li>
<li><a href="#Nested" class="heading-2">Nested
</a></li>
<li><a href="#With-a-link" class="heading-1">With a link
</a></li>
<li><a href="#Def-link-like-a-definition" class="heading-2">[Def]: link like a definition
</a></li>
      </ol>
    </nav>
  </details>
</div>
<section id="First">
<h1>First</h1>
<section id="Nested">
<h2>Nested</h2>
</section>
</section>
<section id="With-a-link">
<h1>With a <a>link</a></h1>
<section id="Def-link-like-a-definition">
<h2>[Def]: link like a definition</h2>
</section>
</section>
"##
        );

        Ok(())
    }
}
