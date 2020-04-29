{-# LANGUAGE OverloadedStrings #-}
-- Workaround for new ghc features.
-- I'm not smart enough to figure out how to modify the code for this.
{-# LANGUAGE FlexibleContexts #-}

import Contexts
import Config
import Compilers
import Routes
import Render
import Pygments

import Control.Applicative ((<$>))
import Data.Monoid (mappend, mconcat, (<>))
import Data.Char (toLower)
import Data.List (intercalate, isSuffixOf)
import Data.List.Utils (replace)
import Data.Ord (comparing)
import System.FilePath (dropExtension, splitFileName, joinPath)
import Data.Maybe (fromMaybe)
import Control.Applicative (Alternative (..))

import Hakyll hiding (pandocCompiler)
import Text.Sass.Options
import Text.Pandoc


-- Recommended posts on home page
recommended :: [Pattern]
recommended = [ "posts/2015-07-22-5_years_at_the_university.markdown"
              , "posts/2014-10-06-ida_summer_of_code_2014_summary.markdown"
              , "posts/2014-07-13-summer_job_at_configura.markdown"
              , "posts/2013-01-20-i_robot.markdown"
              , "posts/2010-06-01-game_design_analysis_world_of_goo.markdown"
              ]

postsGlob = "posts/*.markdown"


main :: IO ()
main = do

    streams <- pygmentsServer

    hakyllWith config $ do
        match ("images/**" .||. "favicon.ico" .||. "fonts/**") $ do
            route   idRoute
            compile copyFileCompiler

        match "css/*.scss" $ do
            compile getResourceBody

        -- Enable hot reload when changing an imported stylesheet
        scssDependencies <- makePatternDependency "css/*.scss"
        rulesExtraDependencies [scssDependencies] $ do
            create ["css/main.css"] $ do
                route   idRoute
                compile sassCompiler

        tags <- buildTags postsGlob (fromCapture "tags/*")

        match "static/*.markdown" $ do
            route   staticRoute
            compile $ pandocCompiler streams
                >>= loadAndApplyTemplate "templates/static.html" siteCtx
                >>= loadAndApplyTemplate "templates/site.html" siteCtx
                >>= deIndexUrls

        match "static/*.txt" $ do
            route   txtStaticRoute
            compile getResourceString

        match "static/*.html" $ do
            route   staticRoute
            compile copyFileCompiler

        match postsGlob $ do
            let ctx = (postCtx tags)

            route   postRoute
            compile $ do
                feedCompiler
                    >>= loadAndApplyTemplate "templates/post.html" ctx
                    >>= saveSnapshot "feed"
                postCompiler streams
                    >>= saveSnapshot "demoted_content"
                    >>= loadAndApplyTemplate "templates/post.html" ctx
                    >>= loadAndApplyTemplate "templates/site.html" ctx
                    >>= deIndexUrls

        match "drafts/*.markdown" $ do
            let ctx = (draftCtx tags)

            route   draftRoute
            compile $ postCompiler streams
                >>= loadAndApplyTemplate "templates/post.html" ctx
                >>= loadAndApplyTemplate "templates/site.html" ctx
                >>= deIndexUrls

        create ["blog/index.html"] $ do
            route idRoute
            compile $ do
                let ctx = constField "title" "My Weblog" <> siteCtx

                loadAllSnapshots "posts/*.markdown" "demoted_content"
                    >>= recentFirst
                    >>= loadAndApplyTemplateList "templates/post.html" (postCtx tags)
                    >>= makeItem
                    >>= loadAndApplyTemplate "templates/posts.html" ctx
                    >>= loadAndApplyTemplate "templates/site.html" ctx
                    >>= deIndexUrls

        create ["archive/index.html"] $ do
            let title = "The Archives"
            let pattern = "posts/*.markdown"

            route   idRoute
            compile $ archiveCompiler title tags pattern "templates/archive.html"

        create ["drafts/index.html"] $ do
            let title = "All drafts"
            let pattern = "drafts/*.markdown"

            route   idRoute
            compile $ draftArchiveCompiler title tags pattern "templates/post-list.html"

        -- Pages for individual tags
        tagsRules tags $ \tag pattern -> do
            let title = "Posts tagged: " ++ tag

            route   tagRoute
            compile $ archiveCompiler title tags pattern "templates/post-list.html"

        -- All tags list
        create ["blog/tags/index.html"] $ do
            let ctx = tagsCtx (sortedTagHtmlListRenderer tags)

            route idRoute
            compile $ makeItem ""
                >>= loadAndApplyTemplate "templates/tags.html" ctx
                >>= loadAndApplyTemplate "templates/site.html" ctx
                >>= deIndexUrls

        -- Projects page
        match "projects/*.markdown" $ do
            compile $ pandocCompiler streams
                >>= saveSnapshot "content"

        match "projects/games/*.markdown" $ do
            compile $ pandocCompiler streams
                >>= saveSnapshot "content"

        gamesDependencies <- makePatternDependency "projects/games/*.markdown"
        projectDependencies <- makePatternDependency "projects/*.markdown"
        rulesExtraDependencies [gamesDependencies, projectDependencies] $ do
            match "projects.markdown" $ do
                route   $ customRoute (const "projects/index.html")
                compile $ do
                    games <- renderGamesList "projects/games/*.markdown"
                    projects <- renderProjects "projects/*.markdown"
                    let ctx = projectsCtx projects games

                    pandocCompiler streams
                        >>= loadAndApplyTemplate "templates/projects.html" ctx
                        >>= loadAndApplyTemplate "templates/site.html" ctx
                        >>= deIndexUrls

        -- Main page
        match "about.markdown" $ do
            route   $ customRoute (const "index.html")
            compile $ do
                posts <- renderPostList tags "posts/*" $ fmap (take 5) . recentFirst
                recommended <- renderPostList tags (foldr1 (.||.) recommended) $ recentFirst

                pandocCompiler streams
                    >>= loadAndApplyTemplate "templates/homepage.html"
                                            (homepageCtx posts recommended)
                    >>= loadAndApplyTemplate "templates/site.html"
                                            (postCtx tags)
                    >>= deIndexUrls

        match "templates/*" $ compile templateCompiler

        create ["feed.xml"] $ do
            route idRoute
            compile $ do
                let feedCtx = (postCtx tags) <> bodyField "description"

                posts <- fmap (take 30) . recentFirst =<<
                    loadAllSnapshots "posts/*.markdown" "feed"
                renderAtom myFeedConfiguration feedCtx posts

