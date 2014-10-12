{- |
Module      :  Web.Saeplog.Server
Description :  Server related configuration
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

This module essentially describes two essetial components: a configuration data
type definition 'Options' and the 'start' function. Due to their genericity, it
is recommended to import them qualified.

-}
module Web.Saeplog.Server
    ( Options(..)
    , start
    , module ReExport
    ) where

import Happstack.Server as ReExport (Conf (..), nullConf)

import Web.Saeplog.Blog

import Control.Monad
import Control.Monad.IO.Class
import Happstack.Server       (Browsing (..), dir, serveDirectory, simpleHTTP)

data Options = Options
    { happstackConf            :: Conf
    -- ^ Configuration data type from the Happstack package
    , blogEntryRepository      :: Maybe FilePath
    -- ^ Path to a repository (or a path within a repository). The supported
    -- repository types are determined by the version of the filestore package
    -- <http://hackage.haskell.org/package/filestore>
    , staticResourcesDirectory :: Maybe FilePath
    }

-- | Start the blog using the given configuration 'Options'. Currently it will
-- just load all blog entries that are in the configured directory. Static
-- resources (e.g. style sheets) are available via the @/resources/@ path.
start :: (MonadIO io) => Options -> io ()
start opts = do
    blogConfig <- maybe (return Nothing) initBlog (blogEntryRepository opts)
    liftIO $ simpleHTTP (happstackConf opts) $ msum
        [ maybe mzero
                (dir "resources" . serveDirectory DisableBrowsing [])
                (staticResourcesDirectory opts)
        , maybe mzero serveBlog blogConfig
        ]
