{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ((<$>))
import Data.Monoid (mappend, mconcat, (<>))
import Data.List
import Data.List.Utils
import System.FilePath  (dropExtension, splitFileName, joinPath)
import Hakyll

mail = "mail@jonashietala.se"
name = "Jonas Hietala"
siteRoot = "http://jonashietala.se"


myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Feed title TODO"
    , feedDescription = "Description TODO"
    , feedAuthorName  = name
    , feedAuthorEmail = mail
    , feedRoot        = siteRoot
    }


main :: IO ()
main = hakyll $ do
    match "images/*" $ do
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
            >>= relativizeUrls
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
            >>= relativizeUrls
            >>= deIndexUrls

        version "raw" $ do
            route   rawPostRoute
            compile getResourceString

    create ["blog/index.html"] $ do
        route idRoute

        compile $ do
            let ctx = constField "title" "My Weblog" <> siteCtx
            loadAllSnapshots ("posts/*" .&&. hasNoVersion) "demoted_content"
                >>= recentFirst
                >>= loadAndApplyTemplateList "templates/post.html" (postCtx tags)
                >>= makeItem
                >>= loadAndApplyTemplate "templates/site.html" ctx
                >>= relativizeUrls
                >>= deIndexUrls

    create ["archive/index.html"] $ do
        route   idRoute
        compile $ archiveCompiler "The Archives" tags "posts/*"

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag

        route   tagRoute
        compile $ archiveCompiler title tags pattern

    create ["projects/index.html"] $ do
        route   idRoute
        compile $ do
            let ctx = constField "title" "Projects" <> siteCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/projects.html" ctx
                >>= loadAndApplyTemplate "templates/site.html" ctx
                >>= relativizeUrls
                >>= deIndexUrls

    match "about.markdown" $ do
        route   $ customRoute (const "index.html")
        compile $ do
            list <- renderPostList tags "posts/*" $ fmap (take 3) . recentFirst
            let ctx = constField "posts" list <>
                      field "tags" (\_ -> renderTagList tags) <>
                      --field "tags" (\_ -> renderTagCloud 80 120 tags) <>
                      siteCtx

            pandocCompiler
                >>= loadAndApplyTemplate "templates/index.html" ctx
                >>= loadAndApplyTemplate "templates/site.html" (postCtx tags)
                >>= relativizeUrls
                >>= deIndexUrls

    match "templates/*" $ compile templateCompiler

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = (postCtx tags) <> bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots ("posts/*" .&&. hasNoVersion) "post"
            renderAtom myFeedConfiguration feedCtx posts

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


archiveCompiler :: String -> Tags -> Pattern -> Compiler (Item String)
archiveCompiler title tags pattern = do
    list <- renderPostList tags pattern recentFirst
    let ctx = mconcat
            [ constField "posts" list
            , constField "title" title
            , siteCtx
            ]

    makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" ctx
        >>= loadAndApplyTemplate "templates/site.html" ctx
        >>= relativizeUrls
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

