{- |
Module      :  Web.Saeplog.Blog.Crawler
Description :  Implementation of a meta data collector for the blog entry
               repository
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Web.Saeplog.Blog.Crawler
    where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Data.FileStore
import           Data.Function              (on)
import           Data.List                  (sortBy)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           System.Directory
import           System.FilePath
import           System.IO
import           Web.Saeplog.Blog.Types

-- | Search recursively in the given directory for blog entries.
collectEntryData :: (MonadIO io) => FilePath -> io (Set EntryData)
collectEntryData dir = do
    efs <- runExceptT $ initializeFileStore dir
    case efs of
        Left err -> do
            -- TODO saep 2014-10-09 proper logging or $(html error code) or
            -- error page
            liftIO $ hPutStrLn stderr err
            return mempty
        Right fsd -> createEntryDataFromFileStore fsd

-- | Search through all revisions and create and update the 'EntryData' fields
-- that shoud be returned'.
createEntryDataFromFileStore :: (MonadIO io)
                             => FileStoreData -> io (Set EntryData)
createEntryDataFromFileStore fsd = do
    rs <- liftIO $ searchRevisions
            (fileStore fsd) False (contentRelativePath fsd) ""
    return . Set.fromList . Map.elems . collect mempty
           $ sortBy (compare `on` revDateTime) rs

  where
    collect :: Map FilePath EntryData -> [Revision] -> Map FilePath EntryData
    collect m [] = m
    collect m (r:rs) = let m' = foldr go m (revChanges r)
                       in collect m' rs
      where
        go (Added fp) = case fileTypeFromExtension fp of
            Nothing -> id
            Just ft -> Map.insert fp EntryData
                            { fileType = ft
                            , relativePath = fp
                            , fullPath = repositoryPath fsd </> fp
                            , updates = Set.singleton (revDateTime r, revId r)
                            , markup = Nothing
                            }

        go (Modified fp) = Map.adjust
            (\e -> e { updates = Set.insert (revDateTime r, revId r) (updates e) })
            fp

        go (Deleted fp) = Map.delete fp

-- | Helper data type for vaiu passing.
data FileStoreData = FSD
    { repositoryPath      :: FilePath
    -- ^ Absolute path to the repository
    , contentRelativePath :: FilePath
    -- ^ Path for the content relative to the repository
    , fileStore           :: FileStore
    -- ^ Thu 'FileStore' for the repository
    }

-- | Initialize a 'FileStore' object for the given directory. This function
-- should automatically detect the underlying repository type and traverse into
-- parent directories if necessary. The result is the associated 'FileStore'
-- object together with the relative path relative to the repository for the
-- blog content.
initializeFileStore :: (MonadIO io) => FilePath -> ExceptT String io FileStoreData
initializeFileStore dir = do
    cd <- liftIO $ canonicalizePath dir
    d <- liftIO $ doesDirectoryExist cd
    unless d $ throwE $ "The directory '" ++ cd ++ "' does not exist."

    fileStores <- catMaybes `liftM` sequence
        [ lift (maybeGit cd)
        , lift (maybeDarcs cd)
        , lift (maybeMercurial cd)
        ]

    when (null fileStores) $ throwE $ concat
        [ "The directory '",  dir, "' which has been canonicalized to '"
        , cd, "' points to an unsupported repository "
        , "(includes no repository)."
        ]
    return $ head fileStores

  where

    maybeGit       = maybeFileStore gitFileStore ".git"
    maybeDarcs     = maybeFileStore darcsFileStore "_darcs"
    maybeMercurial = maybeFileStore darcsFileStore ".hg"

-- | Helper funtction to search for a (supported) repository containing blog
-- entries.
maybeFileStore :: (MonadIO io)
               => (FilePath -> FileStore)
               -> FilePath -- ^ The directory to search for
               -> FilePath -- ^ The directory to start traversing (canonicalized)
               -> io (Maybe FileStoreData)
maybeFileStore f qry cd =
    fmap (\p -> FSD cd (makeRelative p cd) (f p)) `liftM` findDirInParents cd qry

-- | Search for a directory named as the second argument to thins function.
-- Traverse the directory tree up to the root if the directory cannot be found
-- in one of the starting directory's parent directories.
findDirInParents :: (MonadIO io) => FilePath -> FilePath -> io (Maybe FilePath)
findDirInParents dir qry = do
    adir <- normalise `liftM` liftIO (canonicalizePath dir)
    containsQry . takeWhile (not . isDrive) $ iterate takeDirectory adir

  where
    containsQry [] = return Nothing
    containsQry (d:ds) = do
        p <- liftIO $ doesDirectoryExist (d </> qry)
        case () of
            _ | p -> return $ Just d
            _     -> containsQry ds
