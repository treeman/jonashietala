use chrono::NaiveDate;
use eyre::{eyre, Result};
use jotdown::{Attributes, Container, Event};
use lazy_static::lazy_static;
use regex::Regex;
use std::borrow::Cow;

pub fn move_changelog(body: &str) -> Cow<'_, str> {
    if let Some(caps) = CHANGELOG.captures(body) {
        let mut res = body.to_string();
        res.replace_range(caps.get(0).unwrap().range(), "");
        res.push_str(&caps[1]);
        res.push('\n');
        Cow::Owned(res)
    } else {
        Cow::Borrowed(body)
    }
}

lazy_static! {
    static ref CHANGELOG: Regex =
        Regex::new(r#"(?s)<!-- CHANGELOG_START -->(.+)<!-- CHANGELOG_END -->"#).unwrap();
}

// TODO
// 1. Should store these and output them last in post
// 2. Need to format them in a nice way.
pub fn convert_changelog<'a, I>(mut content: I) -> Result<Vec<Event<'a>>>
where
    I: Iterator<Item = Event<'a>>,
{
    assert!(matches!(
        content.next(),
        Some(Event::Start(Container::DescriptionList, _))
    ));

    let mut res = Vec::new();

    let html = Container::RawBlock { format: "html" };

    res.push(Event::Start(html.clone(), Attributes::new()));
    res.push(Event::Str(
        r#"<!-- CHANGELOG_START --><section class="changelog"><hr /><ul class="items">"#.into(),
    ));
    res.push(Event::End(html.clone()));

    while let Some(token) = content.next() {
        match token {
            Event::Start(Container::DescriptionTerm, _) => {}
            Event::End(Container::DescriptionList) => break,
            token => return Err(eyre!("Unknown token: {:?}", token)),
        };

        lazy_static! {
            static ref DATE: Regex = Regex::new(r"^(\d{4})-(\d{2})-(\d{2})$").unwrap();
        }

        let when = match content.next() {
            Some(Event::Str(s)) => match DATE.captures(s.as_ref()) {
                Some(captures) => NaiveDate::from_ymd_opt(
                    captures[1].parse().unwrap(),
                    captures[2].parse().unwrap(),
                    captures[3].parse().unwrap(),
                )
                .unwrap(),
                None => return Err(eyre!("Failed to match changelog date: `{s}`")),
            },
            token => {
                return Err(eyre!("Expected string , got: {:?}", token));
            }
        };

        for token in content.by_ref() {
            if matches!(token, Event::Start(Container::DescriptionDetails, _)) {
                break;
            }
        }

        let human_format = when.format("%B %e, %Y").to_string();
        let human_dt = html_escape::encode_text(&human_format);
        let dt_format = when.format("%Y-%m-%dT00:00:00Z").to_string();
        let dt = html_escape::encode_text(&dt_format);

        res.push(Event::Start(html.clone(), Attributes::new()));
        res.push(Event::Str(
            format!(
                r#"<li class="item">
      <time datetime="{dt}" title="{human_dt}">{human_dt}</time>
      <div class="description">
          "#,
            )
            .into(),
        ));
        res.push(Event::End(html.clone()));

        for token in content.by_ref() {
            match token {
                Event::End(Container::DescriptionDetails) => break,
                x => res.push(x),
            }
        }

        // Close `description` and `item`
        res.push(Event::Start(html.clone(), Attributes::new()));
        res.push(Event::Str("</div></li>".into()));
        res.push(Event::End(html.clone()));
    }

    // Close `items` and `changelog`
    res.push(Event::Start(html.clone(), Attributes::new()));
    res.push(Event::Str("</ul></section><!-- CHANGELOG_END -->".into()));
    res.push(Event::End(html.clone()));

    Ok(res)
}

#[cfg(test)]
mod tests {
    use super::*;
    use eyre::Result;

    #[test]
    fn test_move_changelog() -> Result<()> {
        let s = r#"
<section id="Head">
<h1>Head</h1>
<p>Text</p>
<!-- CHANGELOG_START --><section class="changelog">
My changelog
</section><!-- CHANGELOG_END -->
<p>After</p>
</section>
"#;

        let res = move_changelog(s);
        assert_eq!(
            res,
            r#"
<section id="Head">
<h1>Head</h1>
<p>Text</p>

<p>After</p>
</section>
<section class="changelog">
My changelog
</section>
"#
        );

        Ok(())
    }
}
