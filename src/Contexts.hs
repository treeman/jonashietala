{-# LANGUAGE OverloadedStrings #-}

module Contexts where

import Config

import Hakyll
import System.Process (readProcess)
import Text.Blaze.Html (toHtml, toValue, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

siteCtx :: Context String
siteCtx = mconcat
    [ defaultContext
    , constField "mail" mail
    , constField "name" name
    , metaKeywordCtx
    ]


postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ siteCtx
    , dateField "date" "%B %e, %Y"
    , dateField "ymd" "%F"
    , tagsField "linked_tags" tags
    , gitTag "git"
    ]


draftCtx :: Tags -> Context String
draftCtx tags = mconcat
    [ siteCtx
    , constField "date" "January 1, 2000"
    , constField "ymd" "2000-01-01"
    , tagsField "linked_tags" tags
    , gitTag "git"
    ]


gameCtx :: Context String
gameCtx = mconcat
    [ siteCtx
    , dateField "date" "%B %e, %Y"
    , dateField "ymd" "%F"
    ]


projectsCtx :: String -> String -> Context String
projectsCtx projects games = mconcat
    [ siteCtx
    , constField "projects" projects
    , constField "games" games
    ]


homepageCtx :: String -> String -> Context String
homepageCtx posts recommended = mconcat
    [ siteCtx
    , constField "posts" posts
    , constField "recommended" recommended
    ]


tagsCtx :: (Item String -> Compiler String) -> Context String
tagsCtx tags = mconcat
    [ siteCtx
    , field "tags" tags
    ]


-- 'metaKeywords' from tags for insertion in header. Empty if no tags are found.
metaKeywordCtx :: Context String
metaKeywordCtx = field "metaKeywords" $ \item -> do
    tags <- getMetadataField (itemIdentifier item) "tags"
    return $ maybe "" showMetaTags tags
      where
        showMetaTags t = "<meta name=\"keywords\" content=\"" ++ t ++ "\">"


gitTag :: String -> Context String
gitTag key = field key $ \item -> do
  let fp = (toFilePath $ itemIdentifier item)
      gitLog format =
        readProcess "git" [
          "log"
        , "-1"
        , "HEAD"
        , "--pretty=format:" ++ format
        , "--date=format:%b %e, %Y"
        , fp
        ] ""

  unsafeCompiler $ do
    sha     <- gitLog "%h"
    message <- gitLog "%s"
    date    <- gitLog "%ad"

    let history = github ++ "/commits/master/" ++ fp
        title = sha ++ ": " ++ message

    return $ if null sha
             then renderHtml $ do
                    H.span ! A.class_ "commit" $ do
                      "Not Committed"
             else renderHtml $ do
                    H.span ! A.class_ "commit" $ do
                      H.a ! A.class_ "hash"
                          ! A.title (toValue title)
                          ! A.href (toValue history)
                          $ toHtml sha
                      toHtml $ " " ++ date

