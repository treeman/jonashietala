use eyre::eyre;
use eyre::Result;
use hotwatch::Event;
use lazy_static::lazy_static;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use serde::Serialize;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Debug;
use std::fs;
use std::path::PathBuf;
use tera::{Context, Tera};
use tracing::{debug, error, info, warn};
use url::Url;

use crate::content::load_drafts;
use crate::content::load_series;
use crate::content::DraftRef;
use crate::content::PostRef;
use crate::content::SeriesArchiveItem;
use crate::content::SeriesItem;
use crate::content::SeriesRef;
use crate::feed::SiteFeed;
use crate::item::Item;
use crate::paths::AbsPath;
use crate::paths::FilePath;
use crate::paths::RelPath;
use crate::{
    content::{
        load_posts, load_standalones, post_archives, tags_archives, ArchiveItem, DraftArchiveItem,
        DraftItem, HomepageItem, PostItem, ProjectsItem, Sass, StandaloneItem, Tag, TagListItem,
    },
    item::RenderContext,
    site_url::SiteUrl,
    util::{self, load_templates},
};

lazy_static! {
    pub static ref BASE_SITE_URL: Url = Url::parse("https://jonashietala.se").unwrap();
}

#[derive(Debug)]
pub struct SiteOptions {
    pub input_dir: AbsPath,
    pub output_dir: AbsPath,
    pub clear_output_dir: bool,
    pub include_drafts: bool,
}

pub struct SiteContent {
    pub homepage: HomepageItem,
    pub projects: ProjectsItem,

    pub posts: BTreeMap<PostRef, PostItem>,
    pub series: BTreeMap<SeriesRef, SeriesItem>,
    pub drafts: Option<BTreeMap<DraftRef, DraftItem>>,

    pub standalones: HashSet<StandaloneItem>,
}

impl SiteContent {
    fn load(opts: &SiteOptions) -> Result<Self> {
        let mut posts = load_posts(opts.input_dir.join("posts"))?;
        let series = load_series(opts.input_dir.join("series"), &mut posts)?;
        let standalones = load_standalones(opts.input_dir.join("static"))?;

        let drafts = if opts.include_drafts {
            let drafts = load_drafts(opts.input_dir.join("drafts"))?;
            Some(drafts)
        } else {
            None
        };

        println!(
            "posts: {} series: {} standalones: {} drafts: {:?}",
            posts.len(),
            series.len(),
            standalones.len(),
            drafts.as_ref().map(|x| x.len())
        );

        let homepage = HomepageItem::new(&opts.input_dir, &posts)?;
        let projects = ProjectsItem::new(&opts.input_dir)?;

        Ok(Self {
            posts,
            series,
            standalones,
            drafts,
            homepage,
            projects,
        })
    }

    pub fn get_post(&self, post_ref: &PostRef) -> Option<&PostItem> {
        self.posts.get(post_ref)
    }

    pub fn get_series(&self, series_ref: &SeriesRef) -> Option<&SeriesItem> {
        self.series.get(series_ref)
    }
}

pub struct SiteLookup {
    pub tags: HashMap<Tag, Vec<PostRef>>,
}

impl SiteLookup {
    fn from_content(content: &SiteContent) -> Self {
        let mut tags: HashMap<Tag, Vec<PostRef>> = HashMap::new();
        for (post_ref, post) in &content.posts {
            for tag in &post.tags {
                tags.entry(tag.clone())
                    .or_insert_with(Vec::new)
                    .push(post_ref.clone());
            }
        }
        Self { tags }
    }
}

pub struct Site {
    pub content: SiteContent,
    pub lookup: SiteLookup,

    pub templates: Tera,

    // Generation options
    pub opts: SiteOptions,
    // Cached rendering context
    context: Context,
}

#[derive(Default)]
struct SiteRenderOpts<'a> {
    // FIXME update all posts in series when title is changed or a new post is added
    all_posts: bool,
    all_standalones: bool,
    all_drafts: bool,
    draft_archive: bool,
    post_archives: bool,
    tags_archives: bool,
    // FIXME update archive when post is changed
    series_archive: bool,
    tags_list: bool,
    homepage: bool,
    projects: bool,
    series: bool,
    sass: bool,
    copy_files: bool,
    feed: bool,

    extra_items: Vec<&'a dyn Item>,
}

impl SiteRenderOpts<'_> {
    fn all() -> Self {
        Self {
            all_posts: true,
            all_standalones: true,
            all_drafts: true,
            draft_archive: true,
            post_archives: true,
            tags_archives: true,
            series_archive: true,
            tags_list: true,
            homepage: true,
            projects: true,
            series: true,
            sass: true,
            copy_files: true,
            feed: true,
            extra_items: vec![],
        }
    }
}

struct SiteRenderExtra<'a> {
    post_archives: Option<Vec<ArchiveItem>>,
    series_archive: Option<SeriesArchiveItem>,
    tags_archives: Option<Vec<ArchiveItem>>,
    tags_list: Option<TagListItem<'a>>,
    draft_archive: Option<DraftArchiveItem<'a>>,
}

impl<'a> SiteRenderExtra<'a> {
    fn new(opts: &SiteRenderOpts, site: &'a Site) -> SiteRenderExtra<'a> {
        let post_archives = if opts.post_archives {
            Some(post_archives(&site.content.posts))
        } else {
            None
        };
        let tags_archives = if opts.tags_archives {
            Some(tags_archives(&site.lookup.tags))
        } else {
            None
        };
        let series_archive = if opts.series_archive {
            Some(SeriesArchiveItem::new(&site.content.series))
        } else {
            None
        };
        let tags_list = if opts.tags_list {
            Some(TagListItem::new(&site.lookup.tags))
        } else {
            None
        };
        let draft_archive = if opts.draft_archive {
            site.draft_archive()
        } else {
            None
        };

        SiteRenderExtra {
            post_archives,
            series_archive,
            tags_archives,
            tags_list,
            draft_archive,
        }
    }
}

impl Site {
    pub fn load_content(opts: SiteOptions) -> Result<Self> {
        let content = SiteContent::load(&opts)?;
        Self::with_content(content, opts)
    }

    pub fn with_content(content: SiteContent, opts: SiteOptions) -> Result<Self> {
        let lookup = SiteLookup::from_content(&content);
        let templates = load_templates("templates/*.html")?;
        let context = Context::from_serialize(SiteContext::new(opts.include_drafts)).unwrap();

        Ok(Self {
            opts,
            templates,
            content,
            lookup,
            context,
        })
    }

    fn draft_archive(&self) -> Option<DraftArchiveItem> {
        self.content.drafts.as_ref().map(|drafts| DraftArchiveItem {
            drafts: drafts.values().collect(),
            url: SiteUrl::parse("/drafts").unwrap(),
            title: "Drafts".to_string(),
        })
    }

    fn copy_archive(&self) -> Result<()> {
        util::copy_file(
            &self.opts.output_dir.join("blog/index.html"),
            &self.opts.output_dir.join("archive/index.html"),
        )
    }

    pub fn render_all(&self) -> Result<()> {
        if self.opts.clear_output_dir && self.opts.output_dir.exists() {
            debug!("Removing {}", self.opts.clear_output_dir);
            fs::remove_dir_all(&self.opts.output_dir)?;
        }
        self.render(SiteRenderOpts::all())
    }

    fn render(&self, opts: SiteRenderOpts<'_>) -> Result<()> {
        let extra = SiteRenderExtra::new(&opts, self);

        let mut items = opts.extra_items;
        if opts.all_posts {
            info!("Rebuilding all posts");
            for post in self.content.posts.values() {
                items.push(post);
            }
        }
        if opts.all_standalones {
            info!("Rebuilding all standalones");
            for standalone in &self.content.standalones {
                items.push(standalone);
            }
        }
        if opts.all_drafts {
            info!("Rebuilding all drafts");
            if let Some(ref drafts) = self.content.drafts {
                for draft in drafts.values() {
                    items.push(draft);
                }
            }
        }
        if opts.homepage {
            info!("Rebuilding homepage");
            items.push(&self.content.homepage);
        }
        if opts.projects {
            info!("Rebuilding projects");
            items.push(&self.content.projects);
        }
        if opts.series {
            info!("Rebuilding series");
            for serie in self.content.series.values() {
                items.push(serie);
            }
        }
        if let Some(ref post_archives) = extra.post_archives {
            info!("Rebuilding post archives");
            for i in post_archives {
                items.push(i);
            }
        }
        if let Some(ref series_archive) = extra.series_archive {
            info!("Rebuilding series archives");
            items.push(series_archive);
        }
        if let Some(ref tags_archives) = extra.tags_archives {
            info!("Rebuilding tags archives");
            for i in tags_archives {
                items.push(i);
            }
        }
        if let Some(ref tags_list) = extra.tags_list {
            info!("Rebuilding tags list");
            items.push(tags_list);
        }
        if let Some(ref draft_archive) = extra.draft_archive {
            info!("Rebuilding draft archive");
            items.push(draft_archive);
        }

        let sass = Sass;
        if opts.sass {
            info!("Rebuilding css");
            items.push(&sass);
        }

        let feed = SiteFeed;
        if opts.feed {
            info!("Rebuilding feed");
            items.push(&feed);
        }
        self.render_items(&items)?;

        if opts.post_archives {
            self.copy_archive()?;
        }
        if opts.copy_files {
            util::copy_files_keep_dirs("fonts/*", &self.opts.input_dir, &self.opts.output_dir)?;
            util::copy_files_keep_dirs("images/**/*", &self.opts.input_dir, &self.opts.output_dir)?;
            util::copy_files_to(
                self.opts.input_dir.join("static/*.txt").as_str(),
                &self.opts.output_dir,
            )?;
        }

        Ok(())
    }

    fn render_items(&self, items: &[&dyn Item]) -> Result<()> {
        items
            .par_iter()
            .try_for_each(|item| self.render_item(*item))
    }

    fn render_item<T: Item + ?Sized>(&self, item: &T) -> Result<()> {
        item.render(&RenderContext {
            parent_context: &self.context,
            tera: &self.templates,
            output_dir: &self.opts.output_dir,
            content: &self.content,
        })
    }

    pub fn file_changed(&mut self, event: Event) -> Result<()> {
        // FIXME paths don't work anymore...
        match event {
            Event::Write(path) => {
                self.write_event(path)?;
            }
            Event::Create(path) => {
                self.create_event(path)?;
            }
            Event::Rename(from, to) => {
                self.rename_event(from, to)?;
            }
            Event::Remove(path) => {
                self.remove_event(path)?;
            }
            // Event::Remove(path) => {}
            // Rename post
            // Move draft -> post
            // Move post -> draft
            // Rename css
            // Remove css files
            Event::Rescan => {
                self.rebuild_all()?;
            }
            Event::Error(err, path) => {
                return Err(eyre!("{err} {path:?}"));
            }
            _ => {}
        }
        Ok(())
    }

    fn write_event(&mut self, path: PathBuf) -> Result<()> {
        let path = self.file_path(path)?;

        if path.rel_path.0.extension() == Some("rs") {
            error!("Source file changed {path}, please rebuild");
        } else if path.rel_path.starts_with("css/") {
            self.rebuild_css()?;
        } else if path.rel_path.starts_with("posts/") {
            self.rebuild_post(path.abs_path())?;
        } else if path.rel_path.starts_with("static/") {
            self.rebuild_standalone(path.abs_path())?;
        } else if path.rel_path.starts_with("drafts/") {
            self.rebuild_draft(path.abs_path())?;
        } else if path.rel_path.starts_with("series/") {
            self.rebuild_series(path.abs_path())?;
        } else if path.rel_path.starts_with("templates/") {
            self.rebuild_template(path.abs_path())?;
        } else if path.rel_path.starts_with("fonts/") || path.rel_path.starts_with("images/") {
            self.rebuild_copy(path)?;
        } else if path.rel_path == "about.markdown" {
            self.rebuild_homepage()?;
        } else if path.rel_path == "projects.markdown" || path.rel_path.starts_with("projects/") {
            self.rebuild_projects(path.abs_path())?;
        } else if unknown_change_msg(&path.rel_path) {
            warn!("Unknown write: {path}");
        }

        Ok(())
    }

    fn create_event(&mut self, path: PathBuf) -> Result<()> {
        let path = self.file_path(path)?;

        if path.rel_path.starts_with("css/") {
            self.rebuild_css()?;
        // } else if path.starts_with("posts/") {
        //     self.rebuild_post(path)?;
        // } else if path.starts_with("static/") {
        //     self.rebuild_standalone(path)?;
        // } else if path.starts_with("drafts/") {
        //     self.rebuild_draft(path)?;
        // } else if path.starts_with("templates/") {
        //     self.rebuild_template(path)?;
        } else if path.rel_path.starts_with("fonts/") || path.rel_path.starts_with("images/") {
            self.rebuild_copy(path)?;
        } else if unknown_change_msg(&path.rel_path) {
            warn!("Unknown create: {path}");
        }

        Ok(())
    }

    fn rename_event(&mut self, from: PathBuf, to: PathBuf) -> Result<()> {
        let from = self.file_path(from)?;
        let to = self.file_path(to)?;
        warn!("Unsupported rename: {from} {to}");
        Ok(())
    }

    fn remove_event(&mut self, path: PathBuf) -> Result<()> {
        let path = self.file_path(path)?;
        warn!("Unsupported remove: {path}");
        Ok(())
    }

    fn rebuild_css(&self) -> Result<()> {
        info!("Rebuilding css");
        self.render_item(&Sass {})
    }

    fn rebuild_post(&mut self, path: AbsPath) -> Result<()> {
        info!("Post changed: {path}");
        let updated = PostItem::from_file(path.clone())?;
        let post_ref = updated.post_ref();

        // FIXME this misses if we add a series to a post

        let old = self
            .content
            .posts
            .insert(post_ref.clone(), updated)
            .ok_or_else(|| eyre!("Nonexistent post: {}", path))?;

        let updated = self.content.posts.get(&post_ref).unwrap();

        let title_changed = updated.title != old.title;
        let tags_changed = updated.tags != old.tags;
        let series_changed = updated.series != None || updated.series != old.series;
        let recommended_changed = updated.recommended != old.recommended;

        self.render(SiteRenderOpts {
            // It's excessive to rerender ALL posts, just next/prev + in series should be enough.
            all_posts: title_changed || series_changed,
            // It's excessive to rerender ALL tags, just affected tags should be enough.
            tags_archives: title_changed || tags_changed,
            tags_list: tags_changed,
            post_archives: title_changed,
            series_archive: title_changed || series_changed,
            // It's excessive to rerender ALL series, just affected should be enough.
            series: title_changed || series_changed,
            homepage: title_changed || recommended_changed,
            extra_items: vec![updated],
            feed: true,
            ..Default::default()
        })
    }

    fn rebuild_standalone(&mut self, path: AbsPath) -> Result<()> {
        info!("Standalone changed: {path}");
        let updated = StandaloneItem::from_file(path)?;
        self.render_item(&updated)?;

        self.content.standalones.insert(updated);

        Ok(())
    }

    fn rebuild_draft(&mut self, path: AbsPath) -> Result<()> {
        if !self.opts.include_drafts {
            return Ok(());
        }

        info!("Draft changed: {path}");
        let updated = DraftItem::from_file(path.clone())?;

        let drafts = &mut self.content.drafts.as_mut().unwrap();

        let draft_ref = updated.draft_ref();

        let old = drafts
            .insert(draft_ref.clone(), updated)
            .ok_or_else(|| eyre!("Nonexistent draft: {}", path))?;

        let drafts = self.content.drafts.as_ref().unwrap();
        let updated = drafts.get(&draft_ref).unwrap();

        self.render(SiteRenderOpts {
            draft_archive: updated.title != old.title,
            extra_items: vec![updated],
            ..Default::default()
        })
    }

    fn rebuild_series(&mut self, path: AbsPath) -> Result<()> {
        info!("Series changed: {path}");
        let mut updated = SeriesItem::from_file(path.clone())?;

        // We need to loop as we can't build a SeriesRef without having the last updated field.
        let old_ref = self
            .content
            .series
            .iter()
            .find(|x| x.0.id == updated.id)
            .map(|x| x.0.clone())
            .ok_or_else(|| eyre!("Nonexistent series: {}", path))?;

        let old = self
            .content
            .series
            .remove(&old_ref)
            .ok_or_else(|| eyre!("Nonexistent series: {}", path))?;

        updated.posts = old.posts;

        // We need series here for posts to render.
        let series_ref = updated.series_ref();
        self.content.series.insert(series_ref.clone(), updated);
        let updated = self.content.series.get(&series_ref).unwrap();

        let title_changed = updated.title != old.title;

        self.render(SiteRenderOpts {
            // It's excessive to rerender ALL posts, just next/prev + in series should be enough.
            all_posts: title_changed,
            series_archive: true,
            extra_items: vec![updated],
            ..Default::default()
        })
    }

    fn rebuild_homepage(&mut self) -> Result<()> {
        self.content.homepage = HomepageItem::new(&self.opts.input_dir, &self.content.posts)?;

        self.render(SiteRenderOpts {
            projects: true,
            ..Default::default()
        })
    }

    fn rebuild_projects(&mut self, path: AbsPath) -> Result<()> {
        info!("Projects changed: {path}");

        self.content.projects = ProjectsItem::new(&self.opts.input_dir)?;

        self.render(SiteRenderOpts {
            projects: true,
            ..Default::default()
        })
    }

    fn rebuild_template(&mut self, path: AbsPath) -> Result<()> {
        info!("Template changed: {path}");

        let template = if let Some(name) = path.file_name() {
            name
        } else {
            return Ok(());
        };

        self.templates.full_reload()?;

        let opts = match template {
            "site.html" => SiteRenderOpts {
                all_posts: true,
                all_standalones: true,
                all_drafts: true,
                draft_archive: true,
                post_archives: true,
                tags_archives: true,
                tags_list: true,
                homepage: true,
                projects: true,
                ..Default::default()
            },
            "archive.html" => SiteRenderOpts {
                draft_archive: true,
                post_archives: true,
                tags_archives: true,
                ..Default::default()
            },
            "post.html" => SiteRenderOpts {
                all_posts: true,
                all_drafts: true,
                ..Default::default()
            },
            "static.html" => SiteRenderOpts {
                all_standalones: true,
                ..Default::default()
            },
            "tags.html" => SiteRenderOpts {
                tags_list: true,
                ..Default::default()
            },
            "homepage.html" => SiteRenderOpts {
                homepage: true,
                ..Default::default()
            },
            "projects.html" => SiteRenderOpts {
                projects: true,
                ..Default::default()
            },
            "series.html" => SiteRenderOpts {
                series: true,
                ..Default::default()
            },
            "series_archive.html" => SiteRenderOpts {
                series_archive: true,
                ..Default::default()
            },
            _ => {
                error!("Unknown template {template}");
                SiteRenderOpts::all()
            }
        };
        self.render(opts)
    }

    fn rebuild_copy(&mut self, path: FilePath) -> Result<()> {
        info!("Copy changed: {path}");
        util::copy_file(
            &path.abs_path(),
            &self.opts.output_dir.join(&path.rel_path.0),
        )
    }

    fn rebuild_all(&mut self) -> Result<()> {
        self.templates.full_reload()?;
        self.content = SiteContent::load(&self.opts)?;
        self.lookup = SiteLookup::from_content(&self.content);
        self.render_all()
    }

    fn file_path(&self, path: PathBuf) -> Result<FilePath> {
        FilePath::from_std_path(&self.opts.input_dir, path)
    }
}

fn unknown_change_msg(path: &RelPath) -> bool {
    #[allow(clippy::if_same_then_else)]
    if path.starts_with("target") {
        false
    } else if path.starts_with("test-site/") {
        false
    } else if path.starts_with("itemref-derive/") {
        false
    } else if path == "Cargo.toml" {
        false
    } else if path == "Cargo.lock" {
        false
    } else if path == "TODO.md" {
        false
    } else if path == "README.md" {
        false
    } else {
        !path.starts_with(".")
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct SiteContext {
    mail: &'static str,
    meta_keywords: Vec<String>,
    include_drafts: bool,
}

impl SiteContext {
    pub fn new(include_drafts: bool) -> Self {
        Self {
            mail: "mail@jonashietala.se",
            meta_keywords: vec![],
            include_drafts,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::item::TeraItem;
    use crate::tests::*;
    use crate::util::{parse_html_files, ParsedFiles};
    use camino::Utf8Path;
    use camino::Utf8PathBuf;
    use colored::Colorize;

    #[test]
    fn test_render_and_check_site() -> Result<()> {
        let (_output_dir, output_path) = AbsPath::new_tempdir()?;

        // let temp_dir = tempfile::tempdir().unwrap();
        // let output_dir = Utf8PathBuf::from_path_buf(temp_dir.path().to_owned()).unwrap();
        let site = Site::load_content(SiteOptions {
            output_dir: output_path.clone(),
            input_dir: AbsPath::current_dir().unwrap(),
            clear_output_dir: false,
            include_drafts: true,
        })?;
        site.render_all()?;

        assert!(!site.content.posts.is_empty());
        for post in site.content.posts.values() {
            let output_file = post.output_file(&output_path);
            assert!(output_file.exists());
        }
        if let Some(ref drafts) = site.content.drafts {
            assert!(!drafts.is_empty());
            for draft in drafts.values() {
                let output_file = draft.output_file(&output_path);
                assert!(output_file.exists());
            }
        }

        let rel_path = |path| {
            let mut res = output_path.0.clone();
            res.push(path);
            res
        };

        // FIXME test that draft link + files are included/not included depending on site build options

        assert!(output_path.exists());
        assert!(rel_path("blog/2009/07/21/the_first_worst_post/index.html").exists());
        assert!(rel_path("blog/index.html").exists());
        assert!(rel_path("blog/2022/index.html").exists());
        assert!(rel_path("blog/2022/01/index.html").exists());
        assert!(rel_path("blog/2022/01/10/2021_in_review/index.html").exists());
        assert!(rel_path("blog/tags/index.html").exists());
        assert!(rel_path("blog/tags/why_cryptocurrencies/index.html").exists());
        assert!(rel_path("archive/index.html").exists());
        assert!(rel_path("series/index.html").exists());
        assert!(rel_path("series/t-34/index.html").exists());
        assert!(rel_path("css/main.css").exists());
        assert!(rel_path("404/index.html").exists());
        assert!(rel_path("index.html").exists());
        if site.content.drafts.is_some() {
            assert!(rel_path("drafts/index.html").exists());
        }
        assert!(rel_path("encrmsg01.txt").exists());
        assert!(rel_path("feed.xml").exists());

        let files = parse_html_files(&output_path)?;
        let file_errors = check_files(&files, &output_path);
        let mut file_error_count = 0;
        for (path, errors) in file_errors {
            // Drafts shouldn't generate hard errors, but output warnings if we run
            // the test with -- --nocapture or if the test fails for other reasons.
            let is_draft = path.as_str().contains("/drafts/");
            if !is_draft {
                file_error_count += errors.len();
            }

            if !errors.is_empty() {
                if is_draft {
                    print!("{}", "Warnings".yellow());
                } else {
                    print!("{}", "Errors".red());
                }
                println!(" while checking {}", path.as_str().magenta());
                for error in &errors {
                    println!("  {}", error);
                }
            }
        }
        assert_eq!(file_error_count, 0);

        Ok(())
    }

    #[test]
    fn test_site_file_changed() -> Result<()> {
        // use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};
        // tracing_subscriber::registry()
        //     .with(tracing_subscriber::EnvFilter::new(
        //         "jonashietala_se=debug,tower_http=debug",
        //     ))
        //     .with(tracing_subscriber::fmt::layer())
        //     .init();

        let mut test_site = TestSiteBuilder {
            include_drafts: true,
        }
        .build()?;

        assert!(test_site
            .find_post("2022-01-31-test_post.markdown")
            .unwrap()
            .raw_content
            .contains("☃︎"));

        assert!(test_site
            .find_post("2022-01-31-test_post.markdown")
            .unwrap()
            .raw_content
            .contains("☃︎"));

        test_site.change_file("posts/2022-01-31-test_post.markdown", "☃︎", "💩")?;

        assert!(test_site
            .find_post("2022-01-31-test_post.markdown")
            .unwrap()
            .raw_content
            .contains('💩'));

        let myseries = test_site.find_series("myseries.markdown").unwrap();
        assert_eq!(myseries.posts.len(), 2);
        let myseries_content = test_site.output_content("series/myseries/index.html")?;
        assert!(myseries_content.contains("Feb post 1"));
        assert!(myseries_content.contains("Feb post 2"));

        // FIXME check archives

        test_site.change_file(
            "posts/2022-02-01-feb_post.markdown",
            "Feb post 1",
            "First series post",
        )?;

        let myseries_content = test_site.output_content("series/myseries/index.html")?;
        assert!(!myseries_content.contains("Feb post 1"));
        assert!(myseries_content.contains("First series post"));
        assert!(myseries_content.contains("Feb post 2"));

        // FIXME check archives

        // Check adding/removing/writing an image checks

        // let post1_content = test_site.output_content("blog/2022/02/01/feb_post/index.html")?;
        // assert!(post1_content.contains("part 1"));
        // assert!(post1_content.contains("My series"));
        //
        // let post2_content = test_site.output_content("blog/2022/02/02/feb_post2/index.html")?;
        // assert!(post2_content.contains("part 2"));
        // assert!(post2_content.contains("My series"));

        // Test that post titles exist in other post series too
        // Test that post title exists in archive

        Ok(())
    }

    fn check_files<'a>(
        files: &'a ParsedFiles,
        output_dir: &Utf8Path,
    ) -> HashMap<Utf8PathBuf, Vec<GeneratedFileError<'a>>> {
        files
            .values()
            .filter_map(|file| {
                let errors = check_file(file, files, output_dir);
                if errors.is_empty() {
                    None
                } else {
                    Some((file.path.clone(), errors))
                }
            })
            .collect()
    }
}
