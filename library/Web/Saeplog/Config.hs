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

import Data.Text         (Text)
import Text.Blaze.Html5  (Html)
import Web.Saeplog.Types

data BlogConfig = BlogConfig
    { entryRenderer         :: Entry -> Html
    -- ^ This entry describes how the content of a blog entry is being
    -- rendered.
    --
    -- $entrySettings
    , metainfoBoxRenderer   :: Entry -> Html
    -- ^ This function describes how a small meta info box should be rendered.
    -- This is the ideal candidate for
    , metainfoTableRenderer :: Entry -> Html
    , resourcesPath         :: Text
    , cssFileName           :: Text
    , errorMessage          :: Maybe String
    -- ^ Used by the dyre wrapper
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


