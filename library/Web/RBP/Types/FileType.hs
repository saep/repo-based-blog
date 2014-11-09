{-# LANGUAGE DeriveDataTypeable #-}
{- |
Module      :  RBP.Types.FileType
Description :  Supported file types and conversion functions
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module RBP.Types.FileType
    where

import           Data.Data       (Data, Typeable)
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           System.FilePath (takeExtensions)

-- | Enumeration that contains all supported file type extensions for blog
-- entries.
data FileType = PandocMarkdown | LiterateHaskell
    deriving (Eq, Ord, Show, Read, Enum, Data, Typeable, Bounded)

-- | This map stores all supported file type extensions mapped to the
-- appropriate 'FileType' value.
fileTypeMap :: Map FilePath FileType
fileTypeMap = Map.fromList
    [ (".md",  PandocMarkdown)
    , (".lhs", LiterateHaskell)
    ]

-- | Convert the given files extension to the internal 'FileType'
-- representation.
fileTypeFromExtension :: FilePath -> Maybe FileType
fileTypeFromExtension ext = Map.lookup (takeExtensions ext) fileTypeMap

