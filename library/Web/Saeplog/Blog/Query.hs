{- |
Module      :  Web.Saeplog.Blog.Query
Description :  Query data
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Web.Saeplog.Blog.Query
    where

import Web.Saeplog.Util
import Web.Saeplog.Types
import Happstack.Server (look, HasRqData, ServerPartT)

readMaybe :: Read a => String -> Maybe a
readMaybe x = case reads x of
    [(v,_)] -> Just v
    _ -> Nothing

-- | 'look' at the request data of the given name and try to parse it via
-- 'readMaybe'. If the parse failed or the request data did not exist, return
-- the provided default.
maybeLookAndRead :: (Monad m, Read a, Alternative m, HasRqData m)
                 => a -> String -> m a
maybeLookAndRead a qry = do
    l <- optional $ look qry
    return $ fromMaybe a (maybe (Just a) readMaybe l)

data SortMethod = Update | Identifier | Author
    deriving (Show, Read, Eq, Ord, Enum)

data SortOrder = Ascending | Descending
    deriving (Show, Read, Eq, Ord, Enum)


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
-- > id=42&sortBy=Identifier
-- For default and required values look at the 'parseQueryRqData' function.
data EntryQuery = EntryQuery
    { eqId   :: Maybe Integer
    -- ^ Entry identifier provided in the request data
    , eqSortBy :: Entry -> Entry -> Ordering
    -- ^ The method entries are sorted by.
    }

-- | Parse the supported request data and present it in a data type.
parseQueryRqData :: ServerPartT IO EntryQuery
parseQueryRqData = EntryQuery
    <$> (join . fmap readMaybe <$> optional (look "id"))
    <*> (sortMethodToComparator
        <$> maybeLookAndRead Update "sortBy"
        <*> maybeLookAndRead Descending "sortOrder")

