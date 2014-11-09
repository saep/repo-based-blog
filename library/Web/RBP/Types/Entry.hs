{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{- |
Module      :  RBP.Types.Entry
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
module RBP.Types.Entry
    where

import Data.Data                       (Data, Typeable)
import Data.FileStore                  (RevisionId, UTCTime)
import Control.Lens hiding (Indexable, Context)
import Data.Function                   (on)
import Data.IxSet
import Data.Set (Set)
import Data.Text                       (Text)
import RBP.Types.FileType

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
-- | Newtype for 'Integer'
newtype Index = Index { getIndex :: Integer }
    deriving (Eq, Ord, Show, Read, Data, Typeable)
-- | Newtype for 'EntryUpdate'
newtype LastUpdate = LastUpdate { getLastUpdate :: EntryUpdate }
    deriving (Eq, Ord, Show, Read, Data, Typeable)

-- | Metadata for a blog entry.
data Entry = Entry
    { _entryId      :: Integer
    -- ^ Unique blog entry id
    , _title        :: Text
    -- ^ Title of a blog entry (may change over time)
    , _author       :: Text
    -- ^ Author of the blog entry
    , _authorEmail  :: Text
    -- ^ Email of the author
    , _tags         :: Set Text
    -- ^ Tags associated with the entry
    , _fileType     :: FileType
    -- ^ File type of the actual file (determined by extension)
    , _relativePath :: FilePath
    -- ^ Path of the actual content file relative to the blog entry repository
    -- definition
    , _fullPath     :: FilePath
    -- ^ Full path of the content file
    , _updates      :: IxSet EntryUpdate
    -- ^ Indexable set of update times to the entry
    , _lastUpdate   :: EntryUpdate
    -- ^ The latest update to the entry
    }
    deriving (Eq, Ord, Show, Read, Data, Typeable)
makeLenses ''Entry

instance Indexable Entry where
    empty = ixSet
        [ ixFun $ \e -> [ Index $ e^.entryId ]
        , ixFun $ \e -> [ Title $ e^.title ]
        , ixFun $ \e -> [ AuthorName $ e^.author ]
        , ixFun $ \e -> [ AuthorEmail $ e^.authorEmail ]
        , ixFun $ \e -> [ e^.fileType ]
        , ixFun $ \e -> [ RelativePath $ e^.relativePath ]
        , ixFun $ \e -> [ FullPath $ e^.fullPath ]
        , ixFun $ \e -> toDescList (Proxy :: Proxy EntryUpdate) (e^.updates)
        , ixFun $ \e -> [ LastUpdate $ e^.lastUpdate ]
        ]


