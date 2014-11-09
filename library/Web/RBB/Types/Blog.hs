{-# LANGUAGE TemplateHaskell #-}
{- |
Module      :  RBB.Types.Blog
Description :  Internal Blog state
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module RBB.Types.Blog
    where

import Control.Concurrent.STM        (TChan)
import Control.Lens
import Data.FileStore                (FileStore)
import Data.IxSet                    (IxSet)
import Data.Map                      (Map)
import Data.Time                     (UTCTime)
import RBB.Config
import RBB.Types.CachedEntry
import RBB.Types.Entry

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

