{-# LANGUAGE OverloadedStrings #-}

module Render where

import Contexts

import Hakyll
import Data.List (intercalate)
import Text.Blaze.Html (toHtml, toValue, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Ord (comparing)


postList :: Pattern
         -> ([Item String]
         -> Compiler [Item String])
         -> Compiler [Item String]
postList pattern filter = filter =<< loadAll pattern


renderPostList :: Tags
               -> Pattern
               -> ([Item String]
               -> Compiler [Item String])
               -> Compiler String
renderPostList tags pattern filter = do
     -- Not sure how to just send settings, this can be used in templates $if(...)$
    posts <- postList pattern filter
    loadAndApplyTemplateList "templates/post-item.html" (postCtx tags) posts


renderDraftList :: Tags
               -> Pattern
               -> ([Item String]
               -> Compiler [Item String])
               -> Compiler String
renderDraftList tags pattern filter = do
    drafts <- loadAll pattern
    loadAndApplyTemplateList "templates/draft-item.html" (draftCtx tags) drafts


sortedTagHtmlListRenderer :: Tags -> (a -> Compiler (String))
sortedTagHtmlListRenderer tags = (\_ -> renderTagHtmlList (sortTagsBy tagSort tags))


renderTagHtmlList :: Tags -> Compiler (String)
renderTagHtmlList = renderTags makeLink makeList
  where
    makeLink tag url count _ _ = renderHtml $ H.li $
        H.a ! A.href (toValue url) $ toHtml (tag ++ " (" ++ show count ++ ")")
    makeList tags = renderHtml $ H.ul $ H.preEscapedToHtml (intercalate "" tags)


-- Sort tags after number of posts in tag
tagSort :: (String, [Identifier]) -> (String, [Identifier]) -> Ordering
tagSort a b = comparing (length . snd) b a


renderGamesList :: Pattern -> Compiler String
renderGamesList pattern = do
    loadAllSnapshots pattern "content"
        >>= recentFirst
        >>= loadAndApplyTemplateList "templates/game-item.html" gameCtx


renderProjects :: Pattern -> Compiler String
renderProjects pattern = do
    loadAllSnapshots pattern "content"
        >>= loadAndApplyTemplateList "templates/project.html" siteCtx


loadAndApplyTemplateList :: Identifier
                         -> Context a
                         -> [Item a]
                         -> Compiler String
loadAndApplyTemplateList i c is = do
    t <- loadBody i
    applyTemplateList t c is
