module Contexts where

import Config

import Hakyll

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
    ]


draftCtx :: Tags -> Context String
draftCtx tags = mconcat
    [ siteCtx
    , constField "date" "January 1, 2000"
    , constField "ymd" "2000-01-01"
    , tagsField "linked_tags" tags
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
