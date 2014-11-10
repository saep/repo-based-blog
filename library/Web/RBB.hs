{- |
Module      :  Web.RBB
Description :  This module re-exports all necessary and some useful modules
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Web.RBB (
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
    rbb,

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

import Data.Default              (Default (..))
import Web.RBB.Blog              (Blog, blogEntries, getBlogConfig, withBlog)
import Web.RBB.Config            (BlogConfig (..))
import Web.RBB.Main              (rbb)
import Web.RBB.Templates.Default (createDefaultBlogConfig)
import Web.RBB.Types.Entry
import Web.RBB.Types.FileType    (FileType (..))

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
-- > importWeb.RBB
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
-- the function 'rbb'.
--
-- > importWeb.RBB
-- > -- Other imports such as Web.Routes.Boomerang
-- >
-- > myBlogConfig = createDefaultBlogConfig
-- >     "/path/to/repository/with/blog/entries"
-- >     "/path/to/folder/with/static/resources"
-- >
-- > main = rbb $ withBlog myBlogConfig $ \blog -> do
-- >    putStrLn "Replace this with you web application code."
--

-- $ixsetquery
-- These newtype wrappers are used by the 'IxSet' of 'Entry' values and needed
-- for search queries.
--
-- > queryEntryForIdentifier :: Ixset Entry -> Maybe Entry
-- > queryEntryForIdentifier set = getOne $ set @= Index 42
--
