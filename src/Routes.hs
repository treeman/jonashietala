module Routes where

import Hakyll
import Data.Char (toLower)
import Data.List (isSuffixOf)
import Data.List.Utils (replace)
import System.FilePath  (dropExtension)

postRoute :: Routes
postRoute = replacePosts `composeRoutes`
            dateRoute `composeRoutes`
            dropIndexRoute


staticRoute :: Routes
staticRoute = gsubRoute "static/" (const "") `composeRoutes`
              dropIndexRoute

draftRoute :: Routes
draftRoute = dropIndexRoute


txtStaticRoute :: Routes
txtStaticRoute = gsubRoute "static/" (const "") `composeRoutes`
                 setExtension ".txt"


tagRoute :: Routes
tagRoute = (customRoute $ (map toLower) . (replace " " "_") . (replace "?" "") . toFilePath) `composeRoutes`
           gsubRoute "tags/" (const "blog/tags/") `composeRoutes`
           dropIndexRoute


replacePosts :: Routes
replacePosts = gsubRoute "posts/" (const "blog/")


dateRoute :: Routes
dateRoute =
  gsubRoute "/[0-9]{4}-[0-9]{2}-[0-9]{2}-" (replace "-" "/")


-- Move to subdirectories to avoid extensions in links.
dropIndexRoute :: Routes
dropIndexRoute = customRoute $
     (++ "/index.html") . dropExtension . toFilePath


deIndexUrls :: Item String -> Compiler (Item String)
deIndexUrls item = return $ fmap (withUrls stripIndex) item


stripIndex :: String -> String
stripIndex url =
    if "index.html" `isSuffixOf` url && elem (head url) ['/', '.']
    then take (length url - 10) url
    else url
