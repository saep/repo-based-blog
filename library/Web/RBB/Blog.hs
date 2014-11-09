{-# LANGUAGE FlexibleContexts #-}
{- |
Module      :  RBB.Blog
Description :  Very experimental Blog-serving facilties
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module RBB.Blog
    ( withBlog
    , blogEntries
    , Blog
    , getBlogConfig
    ) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Data.IxSet                    (toList)
import           Data.List                     (sortBy)
import qualified Data.Map                      as Map
import           Data.Time
import           System.Exit                   (exitFailure)
import           System.IO
import           RBB.Blog.Query
import           RBB.Config
import           RBB.Converter         (renderEntries)
import           RBB.Crawler
import           RBB.Types.Blog        hiding (Blog)
import qualified RBB.Types.Blog        as Internal
import           RBB.Types.CachedEntry
import           RBB.Types.Entry
import           RBB.Util

-- | A value of this type contains all the data needed for the blog module to
-- operate.
newtype Blog m = Blog (Maybe (TVar (Internal.Blog m)))
-- TODO saep 2014-11-09 remove the Maybe wrapper?

-- | Retrieve the 'BlogConfig' from the 'Blog' value. Due to the resuorce
-- managmeent that the 'Blog' data type encapsulates, this function only works
-- inside an 'IO' monad.
getBlogConfig :: (Functor io, MonadIO io)
              => Blog m -> io (Maybe (BlogConfig m))
getBlogConfig (Blog Nothing) = return Nothing
getBlogConfig (Blog (Just blog)) = Just . view blogConfig <$> liftIO (readTVarIO blog)

-- | Create a 'Blog' object by providing a 'BlogConfig' value.
-- This function also starts threads which will handle the resource management
-- with some configurable settings that can be defined in the 'BlogConfig'.
withBlog :: BlogConfig m -> (Blog m -> IO ()) -> IO ()
withBlog cfg action = do
    mb <- runExceptT $ initBlog cfg
    case mb of
        Left err -> do
            hPutStrLn stderr err
            exitFailure
        Right b -> do
            tb <- atomically $ newTVar b
            _ <- forkIO $ manageEntryCache tb (b^.blogCacheChannel)
            action $ Blog (Just tb)

-- | Retrieve an 'IxSet' of blog 'Entry' values. If
blogEntries :: (Functor io, MonadIO io, Monad m)
            => Blog m                             -- ^ Blog configuration
            -> EntryQuery                         -- ^ Sorting order of the entries
            -> Maybe (IxSet Entry -> IxSet Entry) -- ^ Query function
            -> io (m Html)
blogEntries (Blog Nothing) _ _ = return $ return mempty
blogEntries (Blog (Just tb)) eq qfun = do
    _ <- liftIO . forkIO $ manageEntryUpdates tb
    b <- liftIO $ readTVarIO tb
    let es = toList . fromMaybe id qfun $ b^.entries
    renderEntries b $ sortBy (eqSortBy eq) es


-- | On a site rendering request, test whether the entry repository should
-- check for updates.
manageEntryUpdates :: TVar (Internal.Blog m) -> IO ()
manageEntryUpdates tb = do
    b <- liftIO $ readTVarIO tb
    let luc = b^.lastUpdateCheck
    now <- liftIO getCurrentTime
    let interval = max 1 . fromInteger . updateInterval $ b^.blogConfig
    shouldCheckForUpdate <- liftIO . atomically $ updateUpdateTime now luc interval
    when shouldCheckForUpdate $ do
        blog <- liftIO $ readTVarIO tb
        u <- runExceptT $ updateBlog blog
        case u of
            Left err -> hPutStrLn stderr err
            Right blog' -> liftIO . atomically $ writeTVar tb blog'

  where
    updateUpdateTime :: UTCTime -> UTCTime -> NominalDiffTime-> STM Bool
    updateUpdateTime now luc interval
        | diffUTCTime now luc > interval * 60 {- seconds -} = do
            modifyTVar tb $ lastUpdateCheck .~ now
            return True
        | otherwise = return False


manageEntryCache :: TVar (Internal.Blog m) -> TChan (Integer, CachedEntry) -> IO ()
manageEntryCache tb tc = forever $ do
    (i,h) <- atomically $ readTChan tc
    atomically . modifyTVar tb $ blogEntryCache %~ Map.insert i h


