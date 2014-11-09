{-# LANGUAGE TemplateHaskell #-}
{- |
Module      :  RBB.Types.CachedEntry
Description :  Type for cached entries together with some utility functions
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module RBB.Types.CachedEntry
    where

import RBB.Types
import RBB.Util

data CachedEntry = CachedEntry
    { _entry           :: Html
    , _cacheLastUpdate :: EntryUpdate
    , _cacheEntryData  :: Entry
    }
makeLenses ''CachedEntry
