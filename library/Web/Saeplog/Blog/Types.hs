{-
Description: Commin data type definitions used in the blog module

-- TODO saep 2014-10-08 Create an Internal module for non-exported data types?
-}
module Web.Saeplog.Blog.Types
    where

import           Data.FileStore   (RevisionId)
import           Data.Function    (on)
import           Data.Map         (Map)
import qualified Data.Map         as Map
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Data.Time
import           System.FilePath  (takeExtensions)
import           Text.Blaze.Html5 (Html)

-- | Enumeration that contains all supported file type extensions for blog
-- entries.
data FileType = PandocMarkdown | LiterateHaskell
    deriving (Eq, Ord, Show, Read, Enum)

-- | This map stores all supported file type extensions mapped to the
-- appropriate 'FileType' value.
fileTypeMap :: Map FilePath FileType
fileTypeMap = Map.fromList
    [ (".md",  PandocMarkdown)
    , (".lhs", LiterateHaskell)
    ]

-- | Metadata for a blog entry.
data EntryData = EntryData
    { fileType     :: FileType
    , relativePath :: FilePath
    , fullPath     :: FilePath
    , updates      :: Set (UTCTime, RevisionId)
    , markup       :: Maybe Html
    }

-- | The path to the entry should generally be unique, so it identifies the
-- entry uniquely.
instance Eq EntryData where
    (==) = (==) `on` fullPath

-- | Compare by the time of the last update to the entry. If that is equal,
-- compare the file path.
instance Ord EntryData where
    compare a b = case (compare `on` (Set.findMax . updates)) a b of
        EQ -> (compare `on` fullPath) a b
        c  -> c

-- | Convert the given files extension to the internal 'FileType'
-- representation.
fileTypeFromExtension :: FilePath -> Maybe FileType
fileTypeFromExtension ext = Map.lookup (takeExtensions ext) fileTypeMap

