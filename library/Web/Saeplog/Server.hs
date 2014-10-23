{-# LANGUAGE TemplateHaskell   #-}
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
    ( Options
    , happstackConf
    , blogEntryRepository
    , staticResourcesDirectory
    , start
    , blogConfig
    , module ReExport
    , module Data.Default
    ) where

import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Data.Default
import Data.Maybe
import Happstack.Server     as ReExport (Conf (..), nullConf)
import Happstack.Server
import Web.Saeplog.Blog

data Options = Options
    { _happstackConf            :: Conf
    -- ^ Configuration data type from the Happstack package
    , _blogEntryRepository      :: Maybe FilePath
    -- ^ Path to a repository (or a path within a repository). The supported
    -- repository types are determined by the version of the filestore package
    -- <http://hackage.haskell.org/package/filestore>
    , _staticResourcesDirectory :: Maybe FilePath
    , _blogConfig               :: BlogConfig
    }
makeLenses ''Options

instance Default Options where
    def = Options
        { _happstackConf = nullConf
        , _blogEntryRepository = Nothing
        , _staticResourcesDirectory = Nothing
        , _blogConfig = def
        }

-- | Start the blog using the given configuration 'Options'. Currently it will
-- just load all blog entries that are in the configured directory. Static
-- resources (e.g. style sheets) are available via the @/resources/@ path.
start :: (MonadIO io)
      => Options
      -> [ReaderT Options (ServerPartT IO ) Response]
      -> io ()
start opts routes = do
    blogcfg <- fromMaybe def `liftM`
        maybe (return Nothing) initBlog (opts^.blogEntryRepository)
    let opts' = opts & blogConfig .~ blogcfg

    liftIO $ simpleHTTP (opts^.happstackConf) $ flip runReaderT opts' $
        msum routes

