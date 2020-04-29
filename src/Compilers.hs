{-# LANGUAGE OverloadedStrings #-}

module Compilers where

import Contexts
import Render
import Routes
import Pygments

import Hakyll hiding (pandocCompiler)
import Text.Pandoc
import Text.Pandoc.Walk (walk, walkM)

-- Unicode friendly regex
-- This is the only library I found which has subtitutions and unicode support
import qualified Text.Regex.PCRE.Light as RL
import qualified Text.Regex.PCRE.Heavy as RH


pandocCompiler :: Streams -> Compiler (Item String)
pandocCompiler streams = do
  pandocCompilerWithTransformM defaultHakyllReaderOptions
                               defaultHakyllWriterOptions
                               (pygments streams)

archiveCompiler :: String
                -> Tags
                -> Pattern
                -> Identifier
                -> Compiler (Item String)
archiveCompiler title tags pattern tpl = do
    list <- renderPostList tags pattern recentFirst
    let ctx = mconcat
            [ constField "posts" list
            , constField "title" title
            , siteCtx
            ]

    makeItem ""
        >>= loadAndApplyTemplate tpl ctx
        >>= loadAndApplyTemplate "templates/site.html" ctx
        >>= deIndexUrls


draftArchiveCompiler :: String
                     -> Tags
                     -> Pattern
                     -> Identifier
                     -> Compiler (Item String)
draftArchiveCompiler title tags pattern tpl = do
    list <- renderDraftList tags pattern recentFirst
    let ctx = mconcat
            [ constField "posts" list
            , constField "title" title
            , siteCtx
            ]

    makeItem ""
        >>= loadAndApplyTemplate tpl ctx
        >>= loadAndApplyTemplate "templates/site.html" ctx
        >>= deIndexUrls


feedCompiler :: Compiler (Item String)
feedCompiler = pandocCompilerWith feedReaderOptions feedWriterOptions
                >>= applyFilter youtubeFilter
                >>= return . fmap demoteHeaders

postCompiler :: Streams -> Compiler (Item String)
postCompiler streams = pandocCompiler streams
                       >>= applyFilter youtubeFilter
                       >>= return . fmap demoteHeaders

sassCompiler :: Compiler (Item String)
sassCompiler = loadBody "css/main.scss"
                >>= makeItem
                >>= withItemBody (unixFilter "sassc" args)
    where args = ["-s", "-I", "css/", "--style", "compressed"]


-- Turn off code parsing for feed
-- The only way I found how to turn off line links. It messes up feed readers badly.
feedReaderOptions :: ReaderOptions
feedReaderOptions = defaultHakyllReaderOptions
    { readerExtensions = disableExtension Ext_fenced_code_attributes pandocExtensions }

feedWriterOptions :: WriterOptions
feedWriterOptions = defaultHakyllWriterOptions
    { writerExtensions = disableExtension Ext_fenced_code_attributes pandocExtensions }


-- Find and replace bare youtube links separated by <p></p>.
youtubeFilter :: String -> String
youtubeFilter txt = RH.gsub rx replace txt
    where
      rx = RL.compile "<p>\\s*https?://www\\.youtube\\.com/watch\\?v=([A-Za-z0-9_-]+)\\s*</p>"
                      [RL.utf8]
      replace = \[g] -> "<div class=\"video-wrapper\">\
                 \<div class=\"video-container\">\
                   \<iframe src=\"//www.youtube.com/embed/"
                            ++ g ++ "\" frameborder=\"0\" allowfullscreen/>\
                 \</div>\
              \</div>";

applyFilter :: (Monad m, Functor f) => (String-> String) -> f String -> m (f String)
applyFilter transformator str = return $ (fmap $ transformator) str

