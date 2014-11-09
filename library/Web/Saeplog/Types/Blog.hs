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

import Control.Concurrent.STM        (TChan)
import Control.Lens
import Data.FileStore                (FileStore)
import Data.IxSet                    (IxSet)
import Data.Map                      (Map)
import Data.Time                     (UTCTime)
import Web.Saeplog.Config
import Web.Saeplog.Types.CachedEntry
import Web.Saeplog.Types.Entry

data Blog m = Blog
    { _nextEntryId         :: Integer
    , _entries             :: IxSet Entry
    , _lastEntryUpdate     :: EntryUpdate
    , _lastUpdateCheck     :: UTCTime
    , _fileStore           :: FileStore
    , _blogEntryCache      :: Map Integer CachedEntry
    , _blogCacheChannel    :: TChan (Integer, CachedEntry)
    , _repositoryPath      :: FilePath
    , _contentRelativePath :: FilePath
    , _blogConfig          :: BlogConfig m
    }
makeLenses ''Blog

