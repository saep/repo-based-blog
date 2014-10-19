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
    ( module Web.Saeplog.Crawler
    , BlogConfig(..)
    , initBlog
    , serveBlog
    ) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.IxSet                  (toList)
import Happstack.Server            (Response, ServerPartT, ok, toResponse)
import Text.Blaze.Html5            as H
import Text.Blaze.Html5.Attributes as A hiding (id)

import Web.Saeplog.Converter
import Web.Saeplog.Crawler        (collectEntryData)
import Web.Saeplog.Templates.Saep
import Web.Saeplog.Types

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
        fc <- readFile (e^.fullPath)
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
            link ! rel "stylesheet" ! type_ "text/css"
                 ! href "/resources/saep.css"
            meta ! content "width=device-width, initial-scale=1, maximum-scale=1"
                 ! name "viewport"
            sequence_ (additionalHeaders cfg)
        H.body $ do
            topNavigationBar [ ("Home", Just "/")
                             , ("Github", Just "https://github.com/saep")
                             ]
            sequence_ $ tmp cfg

