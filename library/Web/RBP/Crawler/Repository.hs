{-# LANGUAGE FlexibleContexts #-}
{- |
Module      :  RBP.Crawler
Description :  Implementation of a meta data collector for the blog entry
               repository
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module RBP.Crawler.Repository
    where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Except
import           Data.FileStore                   (Change (..), FileStore,
                                                   Revision (..),
                                                   darcsFileStore, gitFileStore,
                                                   mercurialFileStore)
import qualified Data.FileStore                   as FS
import           Data.IxSet
import qualified Data.IxSet                       as IxSet
import           Data.List                        (foldl')
import           Data.Maybe
import           Data.Monoid
import           Data.Time
import           System.Directory
import           System.FilePath
import           RBP.Config
import           RBP.Crawler.MetaCombiner
import           RBP.Crawler.MetaParser
import           RBP.Types                as E
import           RBP.Types.Blog
import           RBP.Util

-- | Initialize the 'Blog' state by providing a path inside a repository.
initBlog :: (Functor io, MonadIO io) => BlogConfig m -> ExceptT String io (Blog m)
initBlog bcfg = do
    b <- initialBlog
    collectEntryData Nothing b

  where
    initialBlog = do
        (rp, crp, fs) <- initializeFileStore (entryPath bcfg)
        Blog <$> pure 1
             <*> pure mempty
             <*> pure (EntryUpdate (UTCTime (ModifiedJulianDay 0) 0) "")
             <*> liftIO getCurrentTime
             <*> pure fs
             <*> pure mempty
             <*> (liftIO . atomically) newTChan
             <*> pure rp
             <*> pure crp
             <*> pure bcfg

-- | Update the entries in the 'Blog' state.
updateBlog :: (Functor io, MonadIO io) => Blog m -> ExceptT String io (Blog m)
updateBlog blog = collectEntryData (Just (blog^.lastEntryUpdate)) blog

collectEntryData :: (Functor io, MonadIO io)
                 => Maybe EntryUpdate -- initial (Nothing) or update?
                 -> Blog m
                 -> ExceptT String io (Blog m)
collectEntryData eu blog =
    let interval = FS.TimeRange (entryUpdateTime <$> eu) Nothing
        fs = blog^.fileStore
        hist = FS.history fs
        notLatestKnownEntry = case entryRevisionId <$> eu of
            Nothing -> const True
            Just commit -> not . FS.idsMatch fs commit . revId
    in foldr collect blog . takeWhile notLatestKnownEntry -- . sortBy (compare `on` revDateTime)
        <$> liftIO (hist [blog^.contentRelativePath] interval Nothing)

collect :: Revision -> Blog m -> Blog m
collect r blog = foldl' go blog (revChanges r)
  where
    go b (Added fp)    = maybe b (addEntry r b fp) $ fileTypeFromExtension fp
    go b (Modified fp) = maybe b (modEntry r b fp) $ fileTypeFromExtension fp
    go b (Deleted fp)  = b & entries %~ IxSet.deleteIx (RelativePath fp)
                           & lastEntryUpdate .~ EntryUpdate (revDateTime r) (revId r)

metaFromRevision :: Revision -> [Meta]
metaFromRevision = either (const []) id . parseMeta . revDescription

addEntry :: Revision -> Blog m -> FilePath -> FileType -> Blog m
addEntry r blog fp ft =
    let meta = metaFromRevision r
        eu  = EntryUpdate (revDateTime r) (revId r)
        newEntry = Entry
                { _entryId      = blog^.nextEntryId
                , E._title      = (pack . takeBaseName . dropExtensions) fp
                , _author       = (pack . FS.authorName . revAuthor) r
                , _authorEmail  = (pack . FS.authorEmail . revAuthor) r
                , E._tags       = mempty
                , _fileType     = ft
                , _relativePath = fp
                , _fullPath     = blog^.repositoryPath </> fp
                , _updates      = fromList [eu]
                , _lastUpdate   = eu
                }
    in blog & nextEntryId %~ succ
            & entries     %~ contract (Just fp) meta . IxSet.insert newEntry
            & lastEntryUpdate .~ eu

modEntry :: Revision -> Blog m -> FilePath -> FileType -> Blog m
modEntry r blog fp _ =
    let meta = metaFromRevision r
        eu = EntryUpdate (revDateTime r) (revId r)
        insertUpdateTime = ixSetModifyIx (RelativePath fp) $ \e ->
                            e & updates %~ IxSet.insert eu
                              & lastUpdate .~ eu
    in blog & entries %~ (contract (Just fp) meta . insertUpdateTime)
            & lastEntryUpdate .~ eu

-- | Initialize a 'FileStore' object for the given directory. This function
-- should automatically detect the underlying repository type and traverse into
-- parent directories if necessary. The result is the associated 'FileStore'
-- object together with the relative path relative to the repository for the
-- blog content.
--
-- The return value is a triplet containing:
-- * The absolute path to the repository
-- * The content relative path inside the repository
-- * The associated 'FileStore' object for the repository
initializeFileStore :: (Functor io, MonadIO io)
                    => FilePath
                    -> ExceptT String io (FilePath, FilePath, FileStore)
initializeFileStore dir = do
    cd <- liftIO $ canonicalizePath dir
    d <- liftIO $ doesDirectoryExist cd
    unless d $ throwE $ "The directory '" ++ cd ++ "' does not exist."

    fileStores <- catMaybes `liftM` sequence
        [ lift (maybeGit cd)
        , lift (maybeDarcs cd)
        , lift (maybeMercurial cd)
        ]

    when (Prelude.null fileStores) $ throwE $ concat
        [ "The directory '",  dir, "' which has been canonicalized to '"
        , cd, "' points to an unsupported repository "
        , "(includes no repository)."
        ]
    return $ head fileStores

  where

    maybeGit       = maybeFileStore gitFileStore ".git"
    maybeDarcs     = maybeFileStore darcsFileStore "_darcs"
    maybeMercurial = maybeFileStore mercurialFileStore ".hg"

    maybeFileStore :: (Functor io, MonadIO io)
                   => (FilePath -> FileStore)
                   -> FilePath
                   -> FilePath
                   -> io (Maybe (FilePath, FilePath, FileStore))
    maybeFileStore f qry cd =
        fmap (\p -> (cd, makeRelative p cd, f p)) <$> findDirInParents cd qry

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
