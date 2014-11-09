{-# LANGUAGE OverloadedStrings #-}
{- |
Module      : Web.RBB.Config
Description :  Basic configuration for a blog
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Web.RBB.Config
    ( BlogConfig(..)
    ) where

import Data.Text        (Text)
import Data.Time        (UTCTime)
import Text.Blaze.Html5 (Html)
import Web.RBB.Types

-- | Basic configuration of the blog.
-- The @m@ type variable is just a context in which the functions can operate
-- on. It can be as simple as the @Identity@ functor but also more complex to
-- play nice with libraries such as boomerang (which provides type-safe URLs).
-- These functions are usually called in an 'IO' context and hence the context
-- can be some 'IO' type as well.
--
data BlogConfig m = BlogConfig
    { baseURL        :: m Text
    -- ^ The base URL of the website such as <https://github.com/saep/saeplog>.
    , entryRenderer  :: BlogConfig m -> [(Entry, Html)] -> m Html
    -- ^ This field describes how the content of a blog entry is being
    -- rendered. The 'Html' content is the blog content rendered with the
    -- pandoc library. You can take a look at the implementation of the the
    -- module "RBB.Templates.Default" on how to define this function.
    --
    , timeFormatter  :: UTCTime -> Text
    -- ^ Function that converts time entries to printable 'Text'.
    , entryPath      :: FilePath
    -- ^ Path to the repository that contains the blog entries.
    --
    -- The path may as well point to a directory within a repository.
    , updateInterval :: Integer
    -- ^ Interval in minutes at which the entry repository should be queried for
    -- new content. Will default to 10 for entries smaller than 1.
    }

