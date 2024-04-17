use camino::Utf8PathBuf;
use eyre::eyre;
use eyre::Result;
use flume::Sender;
use hotwatch::notify::event::ModifyKind;
use hotwatch::notify::event::RenameMode;
use hotwatch::Event;
use hotwatch::EventKind;
use lazy_static::lazy_static;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use serde::Serialize;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Debug;
use std::fs;
use std::path::PathBuf;
use tera::{Context, Tera};
use tracing::{debug, error, info, warn};
use url::Url;

use crate::content::load_series;
use crate::content::set_post_prev_next;
use crate::content::PostRef;
use crate::content::SeriesArchiveItem;
use crate::content::SeriesItem;
use crate::content::SeriesRef;
use crate::feed::SiteFeed;
use crate::item::Item;
use crate::markup::markup_lookup::MarkupLookup;
use crate::paths::AbsPath;
use crate::paths::FilePath;
use crate::paths::RelPath;
use crate::server::messages::{Diagnostic, NeovimResponse, WebEvent};
use crate::{
    content::{
        load_posts, load_standalones, post_archives, tags_archives, ArchiveItem, HomepageItem,
        JsItem, PostItem, ProjectsItem, SassItem, StandaloneItem, Tag, TagListItem,
    },
    item::RenderContext,
    site_url::SiteUrl,
    util::{self, load_templates},
};

lazy_static! {
    pub static ref BASE_SITE_URL: Url = Url::parse("https://www.jonashietala.se").unwrap();
}

#[derive(Debug)]
pub struct SiteOptions {
    pub input_dir: AbsPath,
    pub output_dir: AbsPath,
    pub clear_output_dir: bool,
    pub include_drafts: bool,
    pub generate_feed: bool,
    pub include_js: bool,
    pub generate_markup_lookup: bool,
}

pub struct SiteContent {
    pub homepage: HomepageItem,
    pub projects: ProjectsItem,

    pub posts: BTreeMap<PostRef, PostItem>,
    pub series: BTreeMap<SeriesRef, SeriesItem>,
    pub drafts: Option<BTreeSet<PostRef>>,

    pub standalones: HashSet<StandaloneItem>,
}

impl SiteContent {
    fn load(opts: &SiteOptions) -> Result<Self> {
        let post_dirs = if opts.include_drafts {
            vec!["posts", "drafts"]
        } else {
            vec!["posts"]
        }
        .into_iter()
        .map(|x| opts.input_dir.join(x))
        .collect::<Vec<_>>();

        let mut posts = load_posts(&post_dirs, opts.generate_markup_lookup)?;
        let series = load_series(opts.input_dir.join("series"), &mut posts)?;
        let standalones = load_standalones(opts.input_dir.join("static"))?;

        let drafts = if opts.include_drafts {
            Some(
                posts
                    .iter()
                    .filter_map(|(post_ref, item)| {
                        if item.is_draft {
                            Some(post_ref.clone())
                        } else {
                            None
                        }
                    })
                    .collect(),
            )
        } else {
            None
        };

        println!(
            "posts: {} series: {} standalones: {} drafts: {}",
            posts.len(),
            series.len(),
            standalones.len(),
            drafts.as_ref().map(|x: &BTreeSet<_>| x.len()).unwrap_or(0)
        );

        let projects = ProjectsItem::new(&opts.input_dir)?;
        let homepage = HomepageItem::new(&posts, &series, &projects.projects, &projects.games)?;

        Ok(Self {
            posts,
            series,
            standalones,
            drafts,
            homepage,
            projects,
        })
    }

    // Find post
    pub fn find_post<'a, P>(&'a self, predicate: P) -> Option<&'a PostItem>
    where
        P: FnMut(&&PostItem) -> bool,
    {
        self.posts.values().find(predicate)
    }

    pub fn find_post_by_file_name<'a>(&'a self, file_name: &str) -> Option<&'a PostItem> {
        self.find_post(|post| post.path.file_name() == Some(file_name))
    }

    pub fn find_post_by_url<'a>(&'a self, url: &str) -> Option<&'a PostItem> {
        self.find_post(|post| post.url.href() == url)
    }

    pub fn find_post_lookup_by_file_name<'a>(&'a self, path: &str) -> Option<&'a MarkupLookup> {
        let path = Utf8PathBuf::from(path);

        self.find_post_by_file_name(path.file_name()?)
            .and_then(|post| post.markup_lookup.as_ref())
    }

    pub fn find_post_lookup_by_url<'a>(&'a self, url: &str) -> Option<&'a MarkupLookup> {
        self.find_post_by_url(url)
            .and_then(|post| post.markup_lookup.as_ref())
    }

    // Find series by file name
    pub fn find_series_by_file_name<'a>(&'a self, file_name: &str) -> Option<&'a SeriesItem> {
        self.series
            .values()
            .find(|series| series.path.file_name() == Some(file_name))
    }

    // Find series by series id
    pub fn find_series_by_id<'a>(&'a self, id: &str) -> Option<&'a SeriesItem> {
        self.series.values().find(|series| series.id == id)
    }

    pub fn insert_post(&mut self, post: PostItem) -> Option<PostItem> {
        let post_ref = post.post_ref();
        if post.is_draft {
            self.drafts.as_mut().map(|drafts| {
                drafts.insert(post_ref.clone());
            });
        }
        let prev_post = self.posts.insert(post_ref.clone(), post);
        set_post_prev_next(&mut self.posts);
        self.update_homepage();
        prev_post
    }

    pub fn update_homepage(&mut self) {
        self.homepage.update_posts(&self.posts)
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

    web_notifier: Option<Sender<WebEvent>>,
    nvim_notifier: Option<Sender<NeovimResponse>>,
}

#[derive(Default, Debug)]
struct SiteRenderOpts<'a> {
    all_posts: bool,
    all_standalones: bool,
    draft_archive: bool,
    post_archives: bool,
    tags_archives: bool,
    series_archive: bool,
    tags_list: bool,
    homepage: bool,
    projects: bool,
    series: bool,
    sass: bool,
    js: bool,
    copy_files: bool,
    feed: bool,

    extra_render: Vec<&'a dyn Item>,
}

impl SiteRenderOpts<'_> {
    fn all() -> Self {
        Self {
            all_posts: true,
            all_standalones: true,
            draft_archive: true,
            post_archives: true,
            tags_archives: true,
            series_archive: true,
            tags_list: true,
            homepage: true,
            projects: true,
            series: true,
            sass: true,
            js: true,
            copy_files: true,
            feed: true,
            extra_render: vec![],
        }
    }

    fn post_created<'a>(post: &'a PostItem) -> SiteRenderOpts<'a> {
        let has_series = post.series.is_some();
        let has_tags = !post.tags.is_empty();

        SiteRenderOpts {
            all_posts: true,
            tags_archives: has_tags,
            tags_list: has_tags,
            post_archives: true,
            draft_archive: post.is_draft,
            series_archive: has_series,
            series: has_series,
            homepage: true,
            extra_render: vec![post],
            feed: true,
            ..Default::default()
        }
    }

    fn post_updated<'a>(old: &PostItem, new: &'a PostItem) -> SiteRenderOpts<'a> {
        let title_changed = new.title != old.title;
        let tags_changed = new.tags != old.tags;
        let series_changed = new.series != old.series;
        let recommended_changed = new.recommended != old.recommended;
        let is_draft = old.is_draft || new.is_draft;

        SiteRenderOpts {
            // NOTE
            // It's excessive to re-render ALL posts, just next/prev + in series should be enough.
            // It's excessive to re-render ALL tags, just affected tags should be enough.
            // It's excessive to re-render ALL series, just affected should be enough.
            all_posts: title_changed || series_changed,
            tags_archives: title_changed || tags_changed,
            tags_list: tags_changed,
            post_archives: title_changed,
            draft_archive: is_draft && title_changed,
            series_archive: title_changed || series_changed,
            series: title_changed || series_changed,
            homepage: title_changed || recommended_changed || tags_changed || series_changed,
            extra_render: vec![new],
            feed: true,
            ..Default::default()
        }
    }
}

struct SiteRenderExtra<'a> {
    post_archives: Option<Vec<ArchiveItem>>,
    series_archive: Option<SeriesArchiveItem>,
    tags_archives: Option<Vec<ArchiveItem>>,
    tags_list: Option<TagListItem<'a>>,
    draft_archive: Option<ArchiveItem>,
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

enum PathEvent {
    SourceFile,
    Css,
    Js,
    Post,
    Static,
    Draft,
    Series,
    Template,
    Font,
    Image,
    #[allow(dead_code)]
    Homepage,
    Project,
    Unknown,
    Ignore,
}

impl PathEvent {
    pub fn from_path(path: &FilePath) -> Self {
        if path.rel_path.0.extension() == Some("rs") {
            Self::SourceFile
        } else if path.rel_path.starts_with("css/") {
            Self::Css
        } else if path.rel_path.starts_with("js/") {
            Self::Js
        } else if path.rel_path.starts_with("posts/") {
            Self::Post
        } else if path.rel_path.starts_with("static/") {
            Self::Static
        } else if path.rel_path.starts_with("drafts/") {
            Self::Draft
        } else if path.rel_path.starts_with("series/") {
            Self::Series
        } else if path.rel_path.starts_with("templates/") {
            Self::Template
        } else if path.rel_path.starts_with("fonts/") || path.rel_path.starts_with("images/") {
            Self::Font
        } else if path.rel_path.starts_with("images/") {
            Self::Image
        } else if path.rel_path == "projects.markdown" || path.rel_path.starts_with("projects/") {
            Self::Project
        } else if unknown_change_msg(&path.rel_path) {
            Self::Unknown
        } else {
            Self::Ignore
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
        let context =
            Context::from_serialize(SiteContext::new(opts.include_drafts, opts.include_js))
                .unwrap();

        Ok(Self {
            opts,
            templates,
            content,
            lookup,
            context,
            web_notifier: None,
            nvim_notifier: None,
        })
    }

    fn draft_archive(&self) -> Option<ArchiveItem> {
        self.content.drafts.as_ref().map(|drafts| ArchiveItem {
            posts: drafts.iter().cloned().collect(),
            url: SiteUrl::parse("/drafts").unwrap(),
            title: "Drafts".to_string(),
            tag_filter: None,
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
        let ctx = self.render_ctx();

        let extra = SiteRenderExtra::new(&opts, self);

        let mut items = opts.extra_render;
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

        let sass = SassItem;
        if opts.sass {
            info!("Rebuilding css");
            items.push(&sass);
        }

        let js = JsItem;
        if self.opts.include_js && opts.js {
            info!("Rebuilding js");
            items.push(&js);
        }

        let feed = SiteFeed;
        if self.opts.generate_feed && opts.feed {
            info!("Rebuilding feed");
            items.push(&feed);
        }
        render_items(&items, &ctx)?;

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

        debug!("Render completed");

        self.notify_change(&items)?;

        Ok(())
    }

    fn render_ctx(&self) -> RenderContext {
        RenderContext {
            parent_context: &self.context,
            tera: &self.templates,
            output_dir: &self.opts.output_dir,
            content: &self.content,
        }
    }

    fn render_item<T: Item + ?Sized>(&self, item: &T) -> Result<()> {
        item.render(&self.render_ctx())
    }

    pub fn file_changed(&mut self, mut event: Event) -> Result<()> {
        debug!("Event: {:?}", event);
        match event.kind {
            EventKind::Create(_) => {
                self.create_event(event.paths.pop().unwrap())?;
            }
            EventKind::Modify(ModifyKind::Name(RenameMode::Both)) => {
                // This is rename
                let from = event.paths[0].clone();
                let to = event.paths[1].clone();
                self.rename_event(from, to)?;
            }
            EventKind::Modify(ModifyKind::Name(RenameMode::To)) => {
                self.create_event(event.paths.pop().unwrap())?;
            }
            EventKind::Modify(ModifyKind::Name(_)) => {
                // Skip, generated when modifying files
            }
            EventKind::Modify(ModifyKind::Data(_)) => {
                self.write_event(event.paths.pop().unwrap())?;
            }
            EventKind::Modify(_) => {
                // Skip duplicate events
            }
            EventKind::Remove(_) => {
                self.remove_event(event.paths.pop().unwrap())?;
            }
            EventKind::Access(_) => {}
            _ => {
                debug!("Unknown event: {:?}", event);
            }
        }
        Ok(())
    }

    fn write_event(&mut self, path: PathBuf) -> Result<()> {
        let path = self.file_path(path)?;

        match PathEvent::from_path(&path) {
            PathEvent::SourceFile => error!("Source file changed `{path}`, please rebuild"),
            PathEvent::Css => self.rebuild_css()?,
            PathEvent::Js => self.rebuild_js()?,
            PathEvent::Post => self.rebuild_post(path.abs_path())?,
            PathEvent::Static => self.rebuild_standalone(path.abs_path())?,
            PathEvent::Draft => self.rebuild_draft(path.abs_path())?,
            PathEvent::Series => self.rebuild_series(path.abs_path())?,
            PathEvent::Template => self.rebuild_template(path.abs_path())?,
            PathEvent::Font | PathEvent::Image => self.rebuild_copy(path)?,
            PathEvent::Homepage => self.rebuild_homepage()?,
            PathEvent::Project => self.rebuild_projects(path.abs_path())?,
            PathEvent::Unknown => warn!("Unknown write: {path}"),
            PathEvent::Ignore => (),
        }

        Ok(())
    }

    fn create_event(&mut self, path: PathBuf) -> Result<()> {
        let path = self.file_path(path)?;

        match PathEvent::from_path(&path) {
            PathEvent::SourceFile => error!("Source file changed `{path}`, please rebuild"),
            PathEvent::Css => self.rebuild_css()?,
            PathEvent::Js => self.rebuild_js()?,
            PathEvent::Static => self.rebuild_standalone(path.abs_path())?,
            PathEvent::Draft => self.rebuild_draft(path.abs_path())?,
            PathEvent::Font | PathEvent::Image => self.rebuild_copy(path)?,
            PathEvent::Homepage => self.rebuild_homepage()?,
            PathEvent::Project => self.rebuild_projects(path.abs_path())?,
            PathEvent::Post => {
                let existing = self
                    .content
                    .find_post_by_file_name(path.rel_path.0.file_name().unwrap());
                if existing.is_none() {
                    self.rebuild_all()?
                } else {
                    debug!("post `{}` already exists", path);
                }
            }
            PathEvent::Series => {
                let existing = self
                    .content
                    .find_series_by_file_name(path.rel_path.0.file_name().unwrap());
                if existing.is_none() {
                    self.rebuild_all()?
                } else {
                    debug!("series `{}` already exists", path);
                }
            }
            PathEvent::Template => self.rebuild_template(path.abs_path())?,
            PathEvent::Ignore => (),
            PathEvent::Unknown => warn!("Unknown create: {path}"),
        }

        Ok(())
    }

    fn rename_event(&mut self, from: PathBuf, to: PathBuf) -> Result<()> {
        let from = self.file_path(from)?;
        let to = self.file_path(to)?;

        // Could be made more efficient, but this is easier and good for consistency.
        // Rebuild all is still quite fast and this is uncommon, so it's fine for now...
        match (PathEvent::from_path(&from), PathEvent::from_path(&to)) {
            (PathEvent::SourceFile, _) | (_, PathEvent::SourceFile) => {
                error!("Source file removed `{from} -> {to}`, please rebuild")
            }
            (PathEvent::Unknown, _) | (_, PathEvent::Unknown) => {
                warn!("Unknown rename: {from} -> {to}")
            }
            (PathEvent::Ignore, PathEvent::Ignore) => (),
            _ => self.rebuild_all()?,
        }

        Ok(())
    }

    fn remove_event(&mut self, path: PathBuf) -> Result<()> {
        let path = self.file_path(path)?;
        match PathEvent::from_path(&path) {
            PathEvent::SourceFile => error!("Source file removed `{path}`, please rebuild"),
            PathEvent::Css => self.rebuild_css()?,
            PathEvent::Font | PathEvent::Image => self.remove_output(path)?,
            PathEvent::Homepage => self.rebuild_homepage()?,
            PathEvent::Project => self.rebuild_projects(path.abs_path())?,
            PathEvent::Unknown => warn!("Unknown remove: {path}"),
            PathEvent::Ignore => (),
            // Not efficient, but it's much easier to get consistency.
            // Rebuild all is still quite fast and this is uncommon, so it's fine for now...
            _ => self.rebuild_all()?,
        }

        Ok(())
    }

    fn rebuild_css(&self) -> Result<()> {
        info!("Rebuilding css");
        self.render_item(&SassItem {})
    }

    fn rebuild_js(&self) -> Result<()> {
        if self.opts.include_js {
            info!("Rebuilding js");
            self.render_item(&JsItem {})
        } else {
            debug!("Skip js");
            Ok(())
        }
    }

    fn rebuild_post(&mut self, path: AbsPath) -> Result<()> {
        info!("Post changed: {path}");
        let mut updated = PostItem::from_file(path.clone(), self.opts.generate_markup_lookup)?;

        if let Some(series) = updated
            .series_id
            .as_deref()
            .and_then(|id| self.content.find_series_by_id(id))
        {
            updated.series = Some(series.series_ref());
        }

        let post_ref = updated.post_ref();
        let prev_post = self.content.insert_post(updated);

        let updated = self.content.posts.get(&post_ref).unwrap();

        let render_opts = match prev_post {
            Some(old) => SiteRenderOpts::post_updated(&old, &updated),
            None => SiteRenderOpts::post_created(&updated),
        };

        self.render(render_opts)
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

        self.rebuild_post(path)
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
        self.update_homepage_item()?;
        let updated = self.content.series.get(&series_ref).unwrap();

        let title_changed = updated.title != old.title;
        let note_changed = updated.post_note != old.post_note;

        self.render(SiteRenderOpts {
            homepage: true,
            all_posts: title_changed || note_changed,
            series_archive: true,
            extra_render: vec![updated],
            ..Default::default()
        })
    }

    fn rebuild_homepage(&mut self) -> Result<()> {
        self.update_homepage_item()?;

        self.render(SiteRenderOpts {
            homepage: true,
            ..Default::default()
        })
    }

    fn rebuild_projects(&mut self, path: AbsPath) -> Result<()> {
        info!("Projects changed: {path}");

        self.content.projects = ProjectsItem::new(&self.opts.input_dir)?;
        self.update_homepage_item()?;

        self.render(SiteRenderOpts {
            projects: true,
            homepage: true,
            ..Default::default()
        })
    }

    fn update_homepage_item(&mut self) -> Result<()> {
        self.content.homepage = HomepageItem::new(
            &self.content.posts,
            &self.content.series,
            &self.content.projects.projects,
            &self.content.projects.games,
        )?;

        Ok(())
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

    fn remove_output(&mut self, path: FilePath) -> Result<()> {
        info!("File removed: {path}");
        std::fs::remove_file(&self.opts.output_dir.join(&path.rel_path.0))?;
        Ok(())
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

    pub fn file_path(&self, path: PathBuf) -> Result<FilePath> {
        FilePath::from_std_path(&self.opts.input_dir, path)
    }

    pub fn set_notifiers(
        &mut self,
        web_notifier: Sender<WebEvent>,
        nvim_notifier: Sender<NeovimResponse>,
    ) {
        self.web_notifier = Some(web_notifier);
        self.nvim_notifier = Some(nvim_notifier);
    }

    fn notify_change(&self, items: &[&dyn Item]) -> Result<()> {
        if let Some(ref tx) = self.nvim_notifier {
            let diagnostics = self.collect_diagnostics(items);
            if !diagnostics.is_empty() {
                tx.send(NeovimResponse::Diagnostics { diagnostics })?;
            }
        }

        if let Some(ref tx) = self.web_notifier {
            tx.send(WebEvent::Refresh)?;
        }

        Ok(())
    }

    fn collect_diagnostics(&self, items: &[&dyn Item]) -> HashMap<String, Vec<Diagnostic>> {
        self.collect_paths_diagnostics(items.into_iter().filter_map(|item| item.source_file()))
    }

    pub fn collect_paths_diagnostics<'a, I: Iterator<Item = &'a AbsPath>>(
        &self,
        paths: I,
    ) -> HashMap<String, Vec<Diagnostic>> {
        paths
            .filter_map(|path| {
                self.collect_path_diagnostics(path)
                    .map(|diagnostics| (path.to_string(), diagnostics))
            })
            .collect()
    }

    fn collect_path_diagnostics(&self, path: &AbsPath) -> Option<Vec<Diagnostic>> {
        let lookup = self.content.find_post_lookup_by_file_name(path.as_str())?;

        Some(
            lookup
                .broken_links
                .iter()
                .map(|link| {
                    let start = lookup.char_pos_to_row_col(link.range.start);
                    let end = lookup.char_pos_to_row_col(link.range.end);
                    Diagnostic {
                        linenum: start.0,
                        column: start.1,
                        end_linenum: end.0,
                        end_column: end.1,
                        message: format!("Link to non-existent link definition `{}`", link.tag),
                    }
                })
                .collect(),
        )
    }
}

fn render_items(items: &[&dyn Item], ctx: &RenderContext) -> Result<()> {
    items.par_iter().try_for_each(|item| item.render(ctx))
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
    include_js: bool,
}

impl SiteContext {
    pub fn new(include_drafts: bool, include_js: bool) -> Self {
        Self {
            mail: "mail@jonashietala.se",
            meta_keywords: vec![],
            include_drafts,
            include_js,
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

    #[allow(dead_code)]
    fn enable_trace() {
        use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};
        tracing_subscriber::registry()
            .with(tracing_subscriber::EnvFilter::new(
                "jonashietala_se=debug,tower_http=debug",
            ))
            .with(tracing_subscriber::fmt::layer())
            .init();
    }

    #[test]
    fn test_render_and_check_site() -> Result<()> {
        let (_output_dir, output_path) = AbsPath::new_tempdir()?;

        // Do this to not delete the output directory if test fails
        // let _x = _output_dir.into_path();

        let site = Site::load_content(SiteOptions {
            output_dir: output_path.clone(),
            input_dir: AbsPath::current_dir().unwrap(),
            clear_output_dir: false,
            include_drafts: true,
            generate_feed: true,
            include_js: false,
            generate_markup_lookup: false,
        })?;
        site.render_all()?;

        assert!(!site.content.posts.is_empty());
        for post in site.content.posts.values() {
            let output_file = post.output_file(&output_path);
            assert!(output_file.exists());
        }

        let rel_path = |path| {
            let mut res = output_path.0.clone();
            res.push(path);
            res
        };

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
    fn test_hide_drafts() -> Result<()> {
        let test_site = TestSiteBuilder {
            include_drafts: false,
            generate_markup_lookup: false,
        }
        .build()?;

        assert!(!test_site.output_path("drafts/index.html").exists());
        assert!(!test_site.output_path("drafts/a_draft/index.html").exists());

        assert!(!test_site
            .read_file_to_string("index.html")?
            .contains("Drafts"));

        assert!(!test_site
            .find_post("2022-01-31-test_post.dj")
            .unwrap()
            .markup
            .content()
            .contains("Drafts"));

        Ok(())
    }

    #[test]
    fn test_site_file_create() -> Result<()> {
        let mut test_site = TestSiteBuilder {
            include_drafts: true,
            generate_markup_lookup: false,
        }
        .build()?;

        assert!(test_site
            .find_post("posts/2023-01-31-new_post.markdown")
            .is_none());

        test_site.create_file(
            "posts/2023-01-31-new_post.markdown",
            r#"
---
title: "New post title"
tags: Tag1
---

My created post
"#,
        )?;

        assert!(test_site
            .find_post("2023-01-31-new_post.markdown")
            .unwrap()
            .markup
            .content()
            .contains("My created post"));

        let homepage = test_site.output_content("index.html")?;
        assert!(homepage.contains("New post title"));

        test_site.create_file(
            "drafts/my_draft.markdown",
            r#"
---
title: "New draft title"
tags: Tag1
---

My created draft
"#,
        )?;

        assert!(test_site
            .output_content("drafts/index.html")?
            .contains("New draft title"));
        assert!(test_site
            .output_content("drafts/my_draft/index.html")?
            .contains("My created draft"));

        test_site.create_file(
            "static/my_static.markdown",
            r#"
---
title: "Some static page"
---

My created static
"#,
        )?;

        let draft = test_site.output_content("my_static/index.html")?;
        assert!(draft.contains("My created static"));

        Ok(())
    }

    #[test]
    fn test_post_removed() -> Result<()> {
        let mut test_site = TestSiteBuilder {
            include_drafts: false,
            generate_markup_lookup: false,
        }
        .build()?;

        assert!(test_site
            .output_content("blog/2022/01/31/test_post/index.html")
            .is_ok());

        // FIXME this may cause other tests to break
        test_site.remove_file("posts/2022-01-31-test_post.dj")?;

        assert!(test_site
            .output_content("blog/2022/01/31/test_post/index.html")
            .is_err());

        Ok(())
    }

    #[test]
    fn test_draft_promoted() -> Result<()> {
        let mut test_site = TestSiteBuilder {
            include_drafts: true,
            generate_markup_lookup: false,
        }
        .build()?;

        assert!(test_site
            .output_content("drafts/a_draft/index.html")?
            .contains("My draft text"));

        // FIXME this may cause other tests to break
        test_site.rename_file(
            "drafts/a_draft.markdown",
            "posts/2023-01-31-now_post.markdown",
        )?;

        assert!(test_site
            .output_content("drafts/a_draft/index.html")
            .is_err());

        assert!(test_site
            .output_content("blog/2023/01/31/now_post/index.html")?
            .contains("My draft text"));

        Ok(())
    }

    #[test]
    fn test_post_demoted() -> Result<()> {
        let mut test_site = TestSiteBuilder {
            include_drafts: true,
            generate_markup_lookup: false,
        }
        .build()?;

        assert!(test_site
            .output_content("blog/2022/01/31/test_post/index.html")?
            .contains("☃︎"));

        // FIXME this may cause other tests to break
        test_site.rename_file(
            "posts/2022-01-31-test_post.markdown",
            "drafts/new_draft.markdown",
        )?;

        assert!(test_site
            .output_content("blog/2022/01/31/test_post/index.html")
            .is_err());

        assert!(test_site
            .output_content("drafts/new_draft/index.html")?
            .contains("☃︎"));

        Ok(())
    }

    #[test]
    fn test_post_content_change() -> Result<()> {
        let mut test_site = TestSiteBuilder {
            include_drafts: false,
            generate_markup_lookup: false,
        }
        .build()?;

        assert!(test_site
            .find_post("2022-01-31-test_post.dj")
            .unwrap()
            .markup
            .content()
            .contains("☃︎"));

        test_site.change_file("posts/2022-01-31-test_post.dj", "☃︎", "💩")?;

        assert!(test_site
            .find_post("2022-01-31-test_post.dj")
            .unwrap()
            .markup
            .content()
            .contains('💩'));

        Ok(())
    }

    #[test]
    fn test_draft_content_change() -> Result<()> {
        let mut test_site = TestSiteBuilder {
            include_drafts: true,
            generate_markup_lookup: false,
        }
        .build()?;

        assert!(test_site
            .output_content("drafts/a_draft/index.html")?
            .contains("My draft text"));

        test_site.change_file("drafts/a_draft.markdown", "My draft text", "DRAFT TEXT")?;

        assert!(test_site
            .output_content("drafts/a_draft/index.html")?
            .contains("DRAFT TEXT"));

        Ok(())
    }

    #[test]
    fn test_post_title_change() -> Result<()> {
        let mut test_site = TestSiteBuilder {
            include_drafts: false,
            generate_markup_lookup: false,
        }
        .build()?;

        let myseries = test_site.find_series("myseries.markdown").unwrap();
        assert_eq!(myseries.posts.len(), 2);
        let myseries_content = test_site.output_content("series/myseries/index.html")?;
        assert!(myseries_content.contains("Feb post 1"));
        assert!(myseries_content.contains("Feb post 2"));

        test_site.change_file(
            "posts/2022-02-01-feb_post.dj",
            "Feb post 1",
            "First series post",
        )?;

        assert!(test_site
            .output_content("archive/index.html")?
            .contains("First series post"));

        let myseries_content = test_site.output_content("series/myseries/index.html")?;
        assert!(!myseries_content.contains("Feb post 1"));
        assert!(myseries_content.contains("First series post"));
        assert!(myseries_content.contains("Feb post 2"));

        Ok(())
    }

    #[test]
    fn test_series_title_change() -> Result<()> {
        let mut test_site = TestSiteBuilder {
            include_drafts: true,
            generate_markup_lookup: false,
        }
        .build()?;

        assert!(test_site
            .output_content("series/index.html")?
            .contains("My series"));
        assert!(test_site
            .output_content("blog/2022/02/01/feb_post/index.html")?
            .contains("My series"));
        assert!(test_site
            .output_content("blog/2022/02/02/feb_post2/index.html")?
            .contains("My series"));

        test_site.change_file("series/myseries.markdown", "My series", "New series title")?;

        assert!(test_site
            .output_content("series/index.html")?
            .contains("New series title"));
        assert!(test_site
            .output_content("blog/2022/02/01/feb_post/index.html")?
            .contains("New series title"));
        assert!(test_site
            .output_content("blog/2022/02/02/feb_post2/index.html")?
            .contains("New series title"));

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
