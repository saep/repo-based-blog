{- |
Module      :  Web.Saeplog
Description :  This module re-exports all necessary and some useful modules
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Web.Saeplog
    ( saeplog
    , module Web.Saeplog.Config
    , module Web.Saeplog.Blog
    , module Web.Saeplog.Templates.Default
    ) where

import Web.Saeplog.Blog              (Blog, blogEntries, withBlog, getBlogConfig)
import Web.Saeplog.Config            (BlogConfig (..))
import Web.Saeplog.Main              (saeplog)
import Web.Saeplog.Templates.Default (createDefaultBlogConfig)
