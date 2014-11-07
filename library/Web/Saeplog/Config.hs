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

data BlogConfig = BlogConfig
    { baseURL        :: Text
    -- ^ URL at which the blog is accessible.
    --
    --  Default <http://127.0.0.1:8000/blog>
    , entryRenderer  :: BlogConfig -> [(Entry, Html)] -> Html
    -- ^ This entry describes how the content of a blog entry is being
    -- rendered. The 'Html' content is the blog content rendered with the
    -- pandoc library.
    --
    -- $entrySettings
    , timeFormatter  :: UTCTime -> Text
    , entryPath      :: FilePath
    -- ^ Path to the repository that contains the blog entries.
    --
    -- The path may as well point to a directory within a repository.
    , resourcesPath  :: FilePath
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


