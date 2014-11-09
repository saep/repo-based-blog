{-# LANGUAGE OverloadedStrings #-}
{- |
Module      :  Web.Saeplog.Config
Description :  Basic configuration for a blog
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Web.Saeplog.Config
    ( BlogConfig(..)
    ) where

import           Data.Text                     (Text)
import           Data.Time                     (UTCTime)
import           Text.Blaze.Html5              (Html)
import           Web.Saeplog.Types

-- | Basic configuration of a blog entry.
data BlogConfig m = BlogConfig
    { baseURL        :: m Text
    -- ^ URL at which the blog is accessible.
    --
    --  Default: @Identity@ <http://127.0.0.1:8000/blog>
    , entryRenderer  :: BlogConfig m -> [(Entry, Html)] -> m Html
    -- ^ This entry describes how the content of a blog entry is being
    -- rendered. The 'Html' content is the blog content rendered with the
    -- pandoc library.
    --
    -- $entrySettings
    , timeFormatter  :: UTCTime -> Text
    -- ^ Function that converts time entries to printable 'Text'.
    , entryPath      :: FilePath
    -- ^ Path to the repository that contains the blog entries.
    --
    -- The path may as well point to a directory within a repository.
    , resourcesPath  :: FilePath
    -- ^ Path to a directory whose content are considered static resources.
    --
    -- Every file int hat directory is accessible, so do not put sensitive
    -- stuff in there.
    , updateInterval :: Integer
    -- ^ Interval in minutes at which the entry repository should be queried for
    -- new content. Will default to 10 for entries smaller than 1.
    }

{-| $entrySettings
As it would be relatively inconvenient to recompile the web site everytime the
cascading style sheet for the general design is changed, it is stored in an
extra file that is dynamically loaded. The 'BlogConfig' defines the two fields
'resourcesPath' and 'cssFileName' to set the sheet.

Most of the content is styled using the <http://pandoc.org pandoc> library. So
your CSS should conform to that. Additionally, every entry can contain
additional class and id tags. But that depends entirely on the function put into
this field.
-}


