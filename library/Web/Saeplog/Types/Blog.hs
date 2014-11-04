{-# LANGUAGE TemplateHaskell #-}
{- |
Module      :  Web.Saeplog.Types.Blog
Description :  Internal Blog state
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Web.Saeplog.Types.Blog
    where

import Control.Concurrent.STM  (TVar)
import Control.Lens
import Data.FileStore          (FileStore)
import Data.IxSet              (IxSet)
import Data.Map                (Map)
import Data.Time               (UTCTime)
import Text.Blaze.Html5        (Html)
import Web.Saeplog.Types.Entry

data Blog = Blog
    { _nextEntryId         :: Integer
    , _entries             :: IxSet Entry
    , _lastEntryUpdate     :: EntryUpdate
    , _lastUpdateCheck     :: UTCTime
    , _fileStore           :: FileStore
    , _blogEntryCache      :: TVar (Map Integer Html)
    -- ^ XXX Remove TVar and instead send the rendered Html (if it was not
    -- present) to an update thread via some channel (e.g. TChan).
    , _repositoryPath      :: FilePath
    , _contentRelativePath :: FilePath
    }
makeLenses ''Blog

