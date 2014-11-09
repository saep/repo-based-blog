{- |
Module      : Web.RBB.Crawler
Description :  Exported Crawler API
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Web.RBB.Crawler
    ( module Web.RBB.Crawler.MetaParser
    , module Web.RBB.Crawler.Repository
    ) where

import Web.RBB.Crawler.MetaParser (Meta (..), parseMeta)
import Web.RBB.Crawler.Repository (initBlog, updateBlog)

