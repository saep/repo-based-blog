{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
Module      :  Web.Saeplog.Blog.Types.Entry
Description :  Entry data type definitions
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

This module contains a lot of newtype wrappers and is hence quite verbose. The
general contract for the unwrapping function is "get" followed by the data type
name. These instances are necessary because everything is put into an 'IxSet'
data structure.
-}
module Web.Saeplog.Blog.Types.Entry
    ( Entry(..)
    , EntryUpdate(..)
    ) where

import Data.Data                       (Data, Typeable)
import Data.FileStore                  (Author, RevisionId, UTCTime)
import Data.Function                   (on)
import Data.IxSet
import qualified Data.IxSet as IxSet
import Data.Text                       (Text)
import Text.Blaze.Html5                (Html)
import Web.Saeplog.Blog.Types.FileType

-- | Newtype around a 'UTCTime'
newtype EntryUpdateTime = EntryUpdateTime { getEntryTime :: UTCTime }
    deriving (Eq, Ord, Show, Read, Data, Typeable)
-- | Newtype around a 'RevisionId'
newtype EntryRevisionId = EntryRevisionId { getEntryRevisionId :: RevisionId }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

-- | Data type storing indexing information for changes made to an 'Entry'.
data EntryUpdate = EntryUpdate
    { entryUpdateTime :: UTCTime
    , entryRevisionId :: RevisionId
    }
    deriving (Eq, Show, Read, Data, Typeable)

instance Ord EntryUpdate where
    compare a b = case (compare `on` entryUpdateTime) a b of
        EQ -> (compare `on` entryRevisionId) a b -- quite arbitrary
        c -> c

instance Indexable EntryUpdate where
    empty = ixSet
        [ ixFun $ \eu -> [ EntryUpdateTime $ entryUpdateTime eu ]
        , ixFun $ \eu -> [ EntryRevisionId $ entryRevisionId eu ]
        ]

-- | Newtype for 'Integer'
newtype EntryId = EntryId { getEntryId :: Integer }
    deriving (Eq, Ord, Enum, Data, Typeable)
-- | Newtype for 'FilePath'
newtype RelativePath = RelativePath { getRelativePath :: FilePath }
    deriving (Eq, Ord, Show, Read, Data, Typeable)
-- | Newtype for 'FilePath'
newtype FullPath = FullPath { getFullPath :: FilePath }
    deriving (Eq, Ord, Show, Read, Data, Typeable)
-- | Newtype for 'Text'
newtype AuthorName = AuthorName { getAuthorName :: Text }
    deriving (Eq, Ord, Show, Read, Data, Typeable)
-- | Newtype for 'Text'
newtype AuthorEmail = AuthorEmail { getAuthorEmail :: Text }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

-- | Metadata for a blog entry.
data Entry = Entry
    { author       :: Text
    , authorEmail  :: Text
    , fileType     :: FileType
    , relativePath :: FilePath
    , fullPath     :: FilePath
    , updates      :: IxSet EntryUpdate
    }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

instance Indexable Entry where
    empty = ixSet
        [ ixFun $ \e -> [ AuthorName $ author e ]
        , ixFun $ \e -> [ AuthorEmail $ authorEmail e ]
        , ixFun $ \e -> [ fileType e ]
        , ixFun $ \e -> [ RelativePath $ relativePath e ]
        , ixFun $ \e -> [ FullPath $ fullPath e ]
        , ixFun $ \e -> IxSet.toDescList (Proxy :: Proxy EntryUpdate) (updates e)
        ]

-- | The path to the entry should generally be unique, so it identifies the
-- entry uniquely.
--instance Eq Entry where
    --(==) = (==) `on` fullPath

-- | Compare by the time of the last update to the entry. If that is equal,
-- compare the file path.
--instance Ord Entry where
    --compare a b = case (compare `on` (Set.findMax . updates)) a b of
        --EQ -> (compare `on` fullPath) a b
        --c  -> c


