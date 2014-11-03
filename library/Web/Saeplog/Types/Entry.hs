{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{- |
Module      :  Web.Saeplog.Types.Entry
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
module Web.Saeplog.Types.Entry
    where

import Data.Data                       (Data, Typeable)
import Data.FileStore                  (RevisionId, UTCTime)
import Control.Lens hiding (Indexable, Context)
import Data.Function                   (on)
import Data.IxSet
import Data.Set (Set)
import Data.Text                       (Text)
import Web.Saeplog.Types.FileType

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

-- | Newtype for 'Text'
newtype Title = Title { getTitle :: Text }
    deriving (Eq, Ord, Show, Read, Data, Typeable)
-- | Newtype for 'Text'
newtype AuthorName = AuthorName { getAuthorName :: Text }
    deriving (Eq, Ord, Show, Read, Data, Typeable)
-- | Newtype for 'Text'
newtype AuthorEmail = AuthorEmail { getAuthorEmail :: Text }
    deriving (Eq, Ord, Show, Read, Data, Typeable)
-- | Newtype for 'Set' 'Text'
newtype Tags = Tags { getTags :: Set Text }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

-- | Newtype for 'FilePath'
newtype RelativePath = RelativePath { getRelativePath :: FilePath }
    deriving (Eq, Ord, Show, Read, Data, Typeable)
-- | Newtype for 'FilePath'
newtype FullPath = FullPath { getFullPath :: FilePath }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

-- | Metadata for a blog entry.
data Entry = Entry
    { _title        :: Text
    , _author       :: Text
    , _authorEmail  :: Text
    , _tags         :: Set Text
    , _fileType     :: FileType
    , _relativePath :: FilePath
    , _fullPath     :: FilePath
    , _updates      :: IxSet EntryUpdate
    }
    deriving (Eq, Ord, Show, Read, Data, Typeable)
makeLenses ''Entry

instance Indexable Entry where
    empty = ixSet
        [ ixFun $ \e -> [ Title $ e^.title ]
        , ixFun $ \e -> [ AuthorName $ e^.author ]
        , ixFun $ \e -> [ AuthorEmail $ e^.authorEmail ]
        , ixFun $ \e -> [ e^.fileType ]
        , ixFun $ \e -> [ RelativePath $ e^.relativePath ]
        , ixFun $ \e -> [ FullPath $ e^.fullPath ]
        , ixFun $ \e -> toDescList (Proxy :: Proxy EntryUpdate) (e^.updates)
        ]


