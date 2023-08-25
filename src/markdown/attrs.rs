use super::html;
use super::pd_html;
use super::strip_one_paragraph;
use eyre::{eyre, Result};
use lazy_static::lazy_static;
use pulldown_cmark::Event;
use regex::Regex;
use std::borrow::Cow;
use std::collections::HashMap;

pub fn parse_ending_attrs(s: &str) -> Result<Option<(&str, Attrs)>> {
    let bracket = match s.rfind('{') {
        Some(x) => x,
        None => return Ok(None),
    };
    if bracket == 0 {
        match parse_attrs(s)? {
            Some(attrs) => Ok(Some(("", attrs))),
            None => Ok(None),
        }
    } else {
        let prefix = s[..bracket - 1].trim_end();
        match parse_attrs(&s[bracket..])? {
            Some(attrs) => Ok(Some((prefix, attrs))),
            None => Ok(None),
        }
    }
}

pub fn parse_attrs(s: &str) -> Result<Option<Attrs>> {
    let caps = match ATTRS.captures(s.trim()) {
        Some(x) => x,
        None => return Ok(None),
    };
    let mut attrs = Attrs::new();

    for attr in caps[1].split(", ") {
        let attr = attr.trim();
        if attr.is_empty() {
            continue;
        } else if let Some(attr) = attr.strip_prefix('.') {
            attrs.class = Some(attr.to_string());
        } else if let Some(attr) = attr.strip_prefix(':') {
            attrs.parser = Some(AttrParser::from_str(attr)?);
        } else if let Some((key, value)) = attr.split_once('=') {
            attrs.key_value.insert(key.into(), value.into());
        } else {
            return Err(eyre!("Bad attribute `{attr}`"));
        };
    }

    if attrs.is_empty() {
        return Err(eyre!("Empty attribute"));
    }

    Ok(Some(attrs))
}

#[derive(Debug, PartialEq, Eq)]
pub struct Attrs {
    pub key_value: HashMap<String, String>,
    pub class: Option<String>,
    pub parser: Option<AttrParser>,
}

impl Attrs {
    pub fn new() -> Self {
        Self {
            key_value: HashMap::new(),
            class: None,
            parser: None,
        }
    }

    #[allow(dead_code)]
    pub fn with_keys<S: ToString>(xs: &[(S, S)]) -> Self {
        Attrs {
            key_value: xs
                .iter()
                .map(|(key, value)| (key.to_string(), value.to_string()))
                .collect(),
            class: None,
            parser: None,
        }
    }

    #[allow(dead_code)]
    pub fn with_class<S: ToString>(class: S) -> Self {
        Attrs {
            key_value: HashMap::new(),
            class: Some(class.to_string()),
            parser: None,
        }
    }

    #[allow(dead_code)]
    pub fn with_parser<S: AsRef<str>>(parser: S) -> Result<Self> {
        Ok(Attrs {
            key_value: HashMap::new(),
            class: None,
            parser: Some(AttrParser::from_str(parser.as_ref())?),
        })
    }

    pub fn is_empty(&self) -> bool {
        self.key_value.is_empty() && self.class.is_none() && self.parser.is_none()
    }

    pub fn push_open_tag(&self, s: &mut String, tag: &str) {
        html::push_open_tag(s, tag, self.class.as_ref());
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum AttrParser {
    Epigraph,
    Notice,
}

impl AttrParser {
    fn from_str(s: &str) -> Result<Self> {
        match s {
            "notice" => Ok(AttrParser::Notice),
            "epigraph" => Ok(AttrParser::Epigraph),
            _ => Err(eyre!("Unsupported attribute parser `{s}`")),
        }
    }

    pub fn transform(&self, events: Vec<Event>, _key_value: HashMap<String, String>) -> String {
        match self {
            AttrParser::Notice => transform_notice(events),
            AttrParser::Epigraph => transform_epigraph(events),
        }
    }
}

fn transform_notice(events: Vec<Event>) -> String {
    tag_content(events, "aside")
}

fn transform_epigraph(events: Vec<Event>) -> String {
    surround_content(
        events,
        r#"<div class="epigraph"><blockquote>"#,
        "</blockquote></div>",
    )
}

fn tag_content(events: Vec<Event>, tag: &str) -> String {
    surround_content(events, &format!("<{tag}>"), &format!("</{tag}>"))
}

fn surround_content(events: Vec<Event>, prefix: &str, suffix: &str) -> String {
    let mut content = String::new();
    pd_html::push_html(&mut content, events.into_iter());
    let content = strip_one_paragraph(Cow::from(content));

    format!("{prefix}{content}{suffix}")
}

lazy_static! {
    static ref ATTRS: Regex = Regex::new(
        r#"(?x)
        ^\{\s*([^}]+?)\s*\}$"#
    )
    .unwrap();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compact() {
        assert_eq!(
            parse_attrs("{width=100%}").unwrap(),
            Some(Attrs::with_keys(&[("width", "100%")]))
        );
    }

    #[test]
    fn test_regular() {
        assert_eq!(
            parse_attrs("{ width=100% }").unwrap(),
            Some(Attrs::with_keys(&[("width", "100%")]))
        );
    }

    #[test]
    fn test_surrounding_space() {
        assert_eq!(
            parse_attrs(" { width=100% }  ").unwrap(),
            Some(Attrs::with_keys(&[("width", "100%")]))
        );
    }

    #[test]
    fn test_multiple() {
        assert_eq!(
            parse_attrs("{ width=100px, height=200px }").unwrap(),
            Some(Attrs::with_keys(&[("width", "100px"), ("height", "200px")]))
        );
    }

    #[test]
    fn test_parse_end() {
        assert_eq!(
            parse_ending_attrs("xyz { width=100 }").unwrap(),
            Some(("xyz", Attrs::with_keys(&[("width", "100")])))
        );
    }

    #[test]
    fn test_parse_end_complete() {
        assert_eq!(
            parse_ending_attrs("{ width=100 }").unwrap(),
            Some(("", Attrs::with_keys(&[("width", "100")])))
        );
    }

    #[test]
    fn test_parse_class() {
        assert_eq!(
            parse_attrs("{ .one }").unwrap(),
            Some(Attrs::with_class("one"))
        );
    }

    #[test]
    fn test_parse_parsers() {
        assert_eq!(
            parse_attrs("{ :notice }").unwrap(),
            Some(Attrs::with_parser("notice").unwrap())
        );
    }

    #[test]
    fn test_errors() {
        assert!(parse_attrs("{ :xxx }").is_err());
        assert!(parse_attrs("{ }").is_err());
        assert!(parse_attrs("{ arg }").is_err());
    }

    #[test]
    fn test_skip() {
        assert!(parse_attrs("x { arg=1 }").unwrap().is_none());
        assert!(parse_attrs("{ arg=1").unwrap().is_none());
        assert!(parse_attrs("{}").unwrap().is_none());
    }
}
