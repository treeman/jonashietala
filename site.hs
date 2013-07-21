{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>))
import Data.Monoid (mappend, mconcat, (<>))
import Data.List
import Data.List.Utils
import System.FilePath  (dropExtension, splitFileName, joinPath)
import Text.Blaze.Html (toHtml, toValue, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Hakyll

mail = "mail@jonashietala.se"
name = "Jonas Hietala"
siteRoot = "http://jonashietala.se"


myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = name ++ ": All posts"
    , feedDescription = "Personal blog of " ++ name
    , feedAuthorName  = name
    , feedAuthorEmail = mail
    , feedRoot        = siteRoot
    }


main :: IO ()
main = hakyll $ do
    match ("images/*" .||. "favicon.ico" .||. "files/**") $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    match "static/*" $ do
        route   staticRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/site.html" siteCtx
            >>= deIndexUrls

        version "raw" $ do
            route   $ setExtension ".txt"
            compile   getResourceString

    match "posts/*" $ do
        route   postRoute
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= return . fmap demoteHeaders
            >>= saveSnapshot "demoted_content"
            >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
            >>= saveSnapshot "post"
            >>= loadAndApplyTemplate "templates/site.html" (postCtx tags)
            >>= deIndexUrls

        version "raw" $ do
            route   rawPostRoute
            compile getResourceString

    match "projects/*" $ do
        compile $ pandocCompiler
            >>= saveSnapshot "content"

    create ["blog/index.html"] $ do
        route idRoute

        compile $ do
            let ctx = constField "title" "My Weblog" <> siteCtx

            loadAllSnapshots ("posts/*" .&&. hasNoVersion) "demoted_content"
                >>= recentFirst
                >>= loadAndApplyTemplateList "templates/post.html" (postCtx tags)
                >>= makeItem
                >>= loadAndApplyTemplate "templates/site.html" ctx
                >>= deIndexUrls

    create ["archive/index.html"] $ do
        route   idRoute
        compile $ archiveCompiler "The Archives" tags "posts/*" "templates/archive.html"

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged: " ++ tag

        route   tagRoute
        compile $ archiveCompiler title tags pattern "templates/tags-archive.html"

    create ["projects/index.html"] $ do
        route   idRoute
        compile $ do
            let ctx = constField "title" "Projects" <> siteCtx

            loadAllSnapshots "projects/*.markdown" "content"
                -- Should be able to apply template in project?
                >>= loadAndApplyTemplateList "templates/project.html" defaultContext
                >>= makeItem
                >>= loadAndApplyTemplate "templates/site.html" ctx
                >>= deIndexUrls

    -- Main page
    match "about.markdown" $ do
        route   $ customRoute (const "index.html")
        compile $ do
            list <- renderPostList tags "posts/*" $ fmap (take 5) . recentFirst
            let ctx = constField "posts" list <>
                      --field "tags" (\_ -> renderTagHtmlList tags) <>
                      field "tags" (\_ -> renderTagList tags) <>
                      --field "tags" (\_ -> renderTagCloud 40 160 tags) <>
                      siteCtx

            pandocCompiler
                >>= loadAndApplyTemplate "templates/index.html" ctx
                >>= loadAndApplyTemplate "templates/site.html" (postCtx tags)
                >>= deIndexUrls

    match "templates/*" $ compile templateCompiler

    create ["feed.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = (postCtx tags) <> bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots ("posts/*" .&&. hasNoVersion) "post"
            renderAtom myFeedConfiguration feedCtx posts

    -- TODO complete.
    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            list <- renderPostList tags "posts/*" recentFirst
            let ctx = mconcat
                    [ constField "posts" list
                    , constField "siteRoot" siteRoot
                    , siteCtx
                    ]

            makeItem ""
                >>= loadAndApplyTemplate "templates/sitemap.xml" ctx
                >>= deIndexUrls


archiveCompiler :: String -> Tags -> Pattern -> Identifier -> Compiler (Item String)
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


siteCtx :: Context String
siteCtx = mconcat
    [ constField "mail" mail
    , constField "name" name
    , metaKeywordCtx
    , defaultContext
    ]


postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ dateField "date" "%B %e, %Y"
    , tagsField "tags" tags
    , siteCtx
   ]


-- 'metaKeywords' from tags for insertion in header. Empty if no tags are found.
metaKeywordCtx :: Context String
metaKeywordCtx = field "metaKeywords" $ \item -> do
    tags <- getMetadataField (itemIdentifier item) "tags"
    return $ maybe "" showMetaTags tags
      where
        showMetaTags t = "<meta name=\"keywords\" content=\"" ++ t ++ "\">"


postList :: Pattern -> ([Item String] -> Compiler [Item String])
         -> Compiler [Item String]
postList pattern filter = filter =<< loadAll (pattern .&&. hasNoVersion)


renderPostList :: Tags -> Pattern -> ([Item String] -> Compiler [Item String])
               -> Compiler String
renderPostList tags pattern filter = do
    posts <- postList pattern filter
    tmpl  <- loadBody "templates/post-item.html"
    applyTemplateList tmpl (postCtx tags) posts


renderTagHtmlList :: Tags -> Compiler (String)
renderTagHtmlList = renderTags makeLink makeList
  where
    makeLink tag url count _ _ = renderHtml $ H.li $
        H.a ! A.href (toValue url) $ toHtml (tag ++ " (" ++ show count ++ ")")
    makeList tags = renderHtml $ H.ul $ H.preEscapedToHtml (intercalate "" tags)


postRoute :: Routes
postRoute = replacePosts `composeRoutes`
            dateRoute `composeRoutes`
            dropIndexRoute


rawPostRoute :: Routes
rawPostRoute = replacePosts `composeRoutes`
            dateRoute `composeRoutes`
            setExtension ".txt"


staticRoute :: Routes
staticRoute = gsubRoute "static/" (const "") `composeRoutes`
              dropIndexRoute


tagRoute :: Routes
tagRoute = gsubRoute "tags/" (const "blog/tags/") `composeRoutes`
           dropIndexRoute


replacePosts :: Routes
replacePosts = gsubRoute "posts/" (const "blog/")


dateRoute :: Routes
dateRoute =
  gsubRoute "/[0-9]{4}-[0-9]{2}-[0-9]{2}-" (replace "-" "/")


-- Move to subdirectories to avoid extensions in links.
dropIndexRoute :: Routes
dropIndexRoute = customRoute $
     (++ "/index.html"). dropExtension . toFilePath


deIndexUrls :: Item String -> Compiler (Item String)
deIndexUrls item = return $ fmap (withUrls stripIndex) item


stripIndex :: String -> String
stripIndex url =
    if "index.html" `isSuffixOf` url && elem (head url) "/."
    then take (length url - 10) url
    else url


loadAndApplyTemplateList :: Identifier
                         -> Context a
                         -> [Item a]
                         -> Compiler String
loadAndApplyTemplateList i c is = do
    t <- loadBody i
    applyTemplateList t c is

