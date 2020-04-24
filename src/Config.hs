module Config where

import Hakyll

mail = "mail@jonashietala.se"
name = "Jonas Hietala"
siteRoot = "http://www.jonashietala.se"


myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = name ++ ": All posts"
    , feedDescription = "Personal blog of " ++ name
    , feedAuthorName  = name
    , feedAuthorEmail = mail
    , feedRoot        = siteRoot
    }


config :: Configuration
config = defaultConfiguration
    { deployCommand = "./sync --site" }
