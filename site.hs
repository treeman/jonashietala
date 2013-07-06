{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend, mconcat)
import Hakyll
import Data.List
import Data.List.Utils
import System.FilePath  (dropExtension, splitFileName, joinPath)


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

    match "*.markdown" $ do
        route   dropIndexRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/site.html" siteCtx
            >>= relativizeUrls
            >>= deIndexUrls

        version "raw" $ do
            route   $ setExtension ".txt"
            compile   getResourceBody

    match "posts/*" $ do
        route   postRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/site.html" (postCtx tags)
            >>= relativizeUrls
            >>= deIndexUrls

    match "posts/*" $ version "raw" $ do
        route   rawPostRoute
        compile getResourceBody

    create ["archive/index.html"] $ do
        route   idRoute
        compile $ do
            let archiveCtx =
                    field "posts" (\_ -> postList tags "posts/*" recentFirst) `mappend`
                    constField "title" "Archives"              `mappend`
                    siteCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/site.html" archiveCtx
                >>= relativizeUrls
                >>= deIndexUrls

    create ["projects/index.html"] $ do
        route   idRoute
        compile $ do
            let projectsCtx =
                    constField "title" "Projects" `mappend`
                    siteCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/projects.html" projectsCtx
                >>= loadAndApplyTemplate "templates/site.html" projectsCtx
                >>= relativizeUrls
                >>= deIndexUrls

    match "index.html" $ do
        route   idRoute
        compile $ do
            list <- postList tags "posts/*" $ fmap (take 3) . recentFirst
            let ctx = constField "posts" list `mappend`
                      field "tags" (\_ -> renderTagList tags) `mappend`
                      --field "tags" (\_ -> renderTagCloud 80 120 tags) `mappend`
                      siteCtx

            getResourceBody
                >>= applyAsTemplate ctx
                >>= loadAndApplyTemplate "templates/site.html" (postCtx tags)
                >>= relativizeUrls
                >>= deIndexUrls

    match "templates/*" $ compile templateCompiler

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = (postCtx tags) `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots ("posts/*" .&&. hasNoVersion) "content"
            renderAtom myFeedConfiguration feedCtx posts


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


postList :: Tags -> Pattern -> ([Item String] -> Compiler [Item String]) -> Compiler String
postList tags pattern filter = do
    posts   <- filter =<< loadAll (pattern .&&. hasNoVersion)
    itemTpl <- loadBody "templates/post-item.html"
    list    <- applyTemplateList itemTpl (postCtx tags) posts
    return list


postRoute :: Routes
postRoute = replacePosts `composeRoutes`
            dateRoute `composeRoutes`
            dropIndexRoute


rawPostRoute :: Routes
rawPostRoute = replacePosts `composeRoutes`
            dateRoute `composeRoutes`
            setExtension ".txt"


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

