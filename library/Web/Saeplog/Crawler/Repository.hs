{- |
Module      :  Web.Saeplog.Crawler
Description :  Implementation of a meta data collector for the blog entry
               repository
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Web.Saeplog.Crawler.Repository
    where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Data.FileStore                   (Change (..), FileStore,
                                                   Revision (..),
                                                   darcsFileStore, gitFileStore,
                                                   mercurialFileStore,
                                                   searchRevisions)
import qualified Data.FileStore                   as FS
import           Data.Function                    (on)
import           Data.IxSet
import           Data.List                        (sortBy)
import           Data.Map                         (Map)
import qualified Data.Map                         as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Text                        (pack)
import           System.Directory
import           System.FilePath
import           Web.Saeplog.Crawler.MetaCombiner as C
import           Web.Saeplog.Crawler.MetaParser
import           Web.Saeplog.Types                as E

-- | Search recursively in the given directory for blog entries.
collectEntryData :: (MonadIO io) => FilePath -> io (Either String (FileStore, IxSet Entry))
collectEntryData dir = do
    efs <- runExceptT $ initializeFileStore dir
    case efs of
        Left err -> return $ Left err
        Right fsd -> do
            es <- createEntryDataFromFileStore fsd
            return $ Right (fileStore fsd, es)

-- | Search through all revisions and create and update the 'EntryData' fields
-- that shoud be returned'.
createEntryDataFromFileStore :: (MonadIO io)
                             => FileStoreData -> io (IxSet Entry)
createEntryDataFromFileStore fsd = do
    rs <- liftIO $ searchRevisions
            (fileStore fsd) False (contentRelativePath fsd) ""
    return . fromList . Map.elems . collect fsd 1 mempty
           $ sortBy (compare `on` revDateTime) rs

collect :: FileStoreData -> Integer -> Map FilePath Entry -> [Revision] -> Map FilePath Entry
collect _ _ fpMap [] = fpMap
collect fsd eId fpMap (r:rs) =
    let (eId',m') = foldr go (eId,fpMap) (revChanges r)
    in collect fsd eId' m' rs
  where
    go (Added fp) (i,m) = case fileTypeFromExtension fp of
        Nothing -> (i,m)
        Just ft ->
            let meta = (either (const []) id . parseMeta . revDescription) r
                upd  = EntryUpdate (revDateTime r) (revId r)
                m' = contract (Just fp) meta $ Map.insert fp Entry
                        { _entryId      = i
                        , E._title      = (pack . takeBaseName . dropExtensions) fp
                        , _author       = (pack . FS.authorName . revAuthor) r
                        , _authorEmail  = (pack . FS.authorEmail . revAuthor) r
                        , E._tags       = mempty
                        , _fileType     = ft
                        , _relativePath = fp
                        , _fullPath     = repositoryPath fsd </> fp
                        , _updates      = fromList [upd]
                        , _lastUpdate   = upd
                        } m
            in (succ i,m')

    go (Modified fp) acc =
        let t = EntryUpdate (revDateTime r) (revId r)
            f e = e & updates %~ insert t & lastUpdate .~ t
        in Map.adjust f fp <$> acc

    go (Deleted fp) acc = fmap (Map.delete fp) acc

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
