{-# LANGUAGE TemplateHaskell #-}
{- |
Module      :  RBP.Types.CachedEntry
Description :  Type for cached entries together with some utility functions
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module RBP.Types.CachedEntry
    where

import RBP.Types
import RBP.Util

data CachedEntry = CachedEntry
    { _entry           :: Html
    , _cacheLastUpdate :: EntryUpdate
    , _cacheEntryData  :: Entry
    }
makeLenses ''CachedEntry
