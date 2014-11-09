{- |
Module      :  Web.Saeplog.Blog.Query
Description :  Utlities for creating search queries
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

This module provides some functions and data types that can be used to manage
search and query data for a list of 'Entry' values.
-}
module Web.Saeplog.Blog.Query (
    EntryQuery(..),

    -- * Sorting
    SortMethod(..),
    SortOrder(..),
    sortMethodToComparator,

    ) where

import Data.Default
import Web.Saeplog.Util
import Web.Saeplog.Types


-- | Simple enum that provides Show and read instances so that a parser can
-- convert the sting directly to a value of this type.
data SortMethod = Update | Identifier | Author
    deriving (Show, Read, Eq, Ord, Enum)

data SortOrder = Ascending | Descending
    deriving (Show, Read, Eq, Ord, Enum)


-- | Convert a 'SortMethod' value to a sorting function that can be used in
-- confunction with functions 'sortBy' from "Data.List".
sortMethodToComparator :: SortMethod -> SortOrder -> (Entry -> Entry -> Ordering)
sortMethodToComparator m o
    | o == Ascending = cmp
    | otherwise      = flip cmp
  where
    cmp = case m of
        Update -> compare `on` view lastUpdate
        Identifier -> compare `on` view entryId
        Author -> compare `on` view author


-- | The request data provided inside a URL.
-- @id=42&sortBy=Identifier@
-- For default and required values look at the 'parseQueryRqData' function.
data EntryQuery = EntryQuery
    { eqSortBy :: Entry -> Entry -> Ordering
    -- ^ The method entries are sorted by.
    }

-- | The 'Default' instance for an 'EntryQuery' type contains a function that
-- sorts the entries descending by the last update to the entry.
instance Default EntryQuery where
    def = EntryQuery (sortMethodToComparator Update  Descending)
