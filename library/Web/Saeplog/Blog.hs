{-# LANGUAGE OverloadedStrings #-}
{- |
Module      :  Web.Saeplog.Blog
Description :  Very experimental Blog-serving facilties
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

XXX This module will most likely change a lot!
-}
module Web.Saeplog.Blog
    ( module Web.Saeplog.Blog.Crawler
    , BlogConfig(..)
    , initBlog
    , serveBlog
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Happstack.Server            (Response, ServerPartT, ok, toResponse)
import Text.Blaze.Html5            as H
import Text.Blaze.Html5.Attributes as A hiding (id)

import Data.IxSet (toList)

import Web.Saeplog.Blog.Converter
import Web.Saeplog.Blog.Crawler   (collectEntryData)
import Web.Saeplog.Blog.Types
import Web.Saeplog.Templates.Saep

-- | Dummy value for now. Might read some special configuration from the blog
-- contents repository in the future.
data BlogConfig = BlogConfig
    { additionalHeaders :: [Html]
    , tmp               :: [Html]
    }

initBlog :: (MonadIO io) => FilePath -> io (Maybe BlogConfig)
initBlog fp = do
    entries <- collectEntryData fp
    md <- liftIO $ forM (toList entries) $ \e -> do
        fc <- readFile (fullPath e)
        return (e, fc)
    let blogEntries = fmap (uncurry convertToHTML) md
    return $ Just (BlogConfig [] blogEntries)

serveBlog :: BlogConfig -> ServerPartT IO Response
serveBlog cfg = ok . toResponse $ do
    H.docType
    H.html $ do
        H.head $ do
            {-H.title (H.toHtml "WIP") -- TODO saep 2014-10-07 WIP-}
            meta ! httpEquiv "Content-Type"
                 ! content "text/html;charset=utf-8"
            link ! rel "stylesheet" ! type_ "text/css" ! href "/resources/saep.css"
            sequence_ (additionalHeaders cfg)
        H.body $ do
            topNavigationBar [ ("Home", Just "/")
                             , ("Github", Just "https://github.com/saep")
                             ]
            sequence_ $ tmp cfg

