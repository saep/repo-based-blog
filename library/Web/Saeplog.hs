{- |
Module      :  Web.Saeplog
Description :  This module re-exports all necessary and some useful modules
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Web.Saeplog (
    -- * Configuration
    -- $configuration
    BlogConfig (..),
    createDefaultBlogConfig,

    -- * Usage
    -- ** General
    -- $usage
    Blog,
    getBlogConfig,
    withBlog,
    blogEntries,

    -- ** XMonad style usage
    -- $example
    saeplog,

    -- * Entry querying
    -- ** Entry
    Entry,
    entryId,
    -- | Unique 'Entry' identifier. It you do not mess with the repositories
    -- history, this should be the same across restarts.
    title,
    -- | Title of the 'Entry'.
    author,
    -- | Author of the 'Entry'.
    authorEmail,
    tags,
    fileType,

    -- $ixsetquery
    Title(..),
    AuthorName(..),
    AuthorEmail(..),
    Tags(..),
    Index(..),

    -- ** FileType
    FileType(..),

    def,
    ) where

import Data.Default (Default(..))
import Web.Saeplog.Blog              (Blog, blogEntries, withBlog, getBlogConfig)
import Web.Saeplog.Config            (BlogConfig (..))
import Web.Saeplog.Main              (saeplog)
import Web.Saeplog.Types.Entry
import Web.Saeplog.Types.FileType    (FileType(..))
import Web.Saeplog.Templates.Default (createDefaultBlogConfig)

-- $configuration
-- The blog configuration is essentially done by setting the fields of the
-- 'BlogConfig' data type. A basic configuration can be created with the
-- function 'createDefaultBlogConfig' that only takes the absolutely necessary
-- parameters.

-- $usage
-- To use the blog in an web application, you have to create an abstact 'Blog'
-- value via the 'withBlog' function and pass it to functions that need access
-- to it because they call blog related functions.
--
-- > import Web.Saeplog
-- >
-- > myBlogConfig = createDefaultBlogConfig
-- >     "/path/to/repository/with/blog/entries"
-- >     "/path/to/folder/with/static/resources"
-- >
-- > main = withBlog myBlogConfig $ \blog -> do
-- >     putStrLn "Replace this with your web application code."

-- $example
--
-- This example is almost identical to the general usage except for the call of
-- the function 'saeplog'.
--
-- > import Web.Saeplog
-- > -- Other imports such as Web.Routes.Boomerang
-- >
-- > myBlogConfig = createDefaultBlogConfig
-- >     "/path/to/repository/with/blog/entries"
-- >     "/path/to/folder/with/static/resources"
-- >
-- > main = saeplog $ withBlog myBlogConfig $ \blog -> do
-- >    putStrLn "Replace this with you web application code."
--

-- $ixsetquery
-- These newtype wrappers are used by the 'IxSet' of 'Entriy' values and needed
-- for search queries.
