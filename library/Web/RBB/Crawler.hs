{- |
Module      :  RBB.Crawler
Description :  Exported Crawler API
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module RBB.Crawler
    ( module RBB.Crawler.MetaParser
    , module RBB.Crawler.Repository
    ) where

import RBB.Crawler.MetaParser (Meta (..), parseMeta)
import RBB.Crawler.Repository (initBlog, updateBlog)

