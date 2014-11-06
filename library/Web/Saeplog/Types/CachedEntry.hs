{-# LANGUAGE TemplateHaskell #-}
{- |
Module      :  Web.Saeplog.Types.CachedEntry
Description :  Type for cached entries together with some utility functions
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Web.Saeplog.Types.CachedEntry
    where

import Web.Saeplog.Types
import Web.Saeplog.Util

data CachedEntry = CachedEntry
    { _cMetaBox     :: Html
    , _cMetaTable   :: Html
    , _cEntryMarkup :: Html
    , _cLastUpdate  :: EntryUpdate
    , _cEntry       :: Entry
    }
makeLenses ''CachedEntry
