{-# LANGUAGE FlexibleContexts #-}
{- |
Module      :  Web.Saeplog.Blog
Description :  Very experimental Blog-serving facilties
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Web.Saeplog.Blog
    ( withBlog
    , blogEntries
    , Blog
    , getBlogConfig
    ) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import qualified Data.IxSet                    as IxSet
import           Data.List                     (sortBy)
import qualified Data.Map                      as Map
import           Data.Time
import           Happstack.Server              (ServerPartT)
import           System.Exit                   (exitFailure)
import           System.IO
import           Web.Saeplog.Blog.Query
import           Web.Saeplog.Config
import           Web.Saeplog.Converter
import           Web.Saeplog.Crawler
import           Web.Saeplog.Types.Blog        hiding (Blog)
import qualified Web.Saeplog.Types.Blog        as Internal
import           Web.Saeplog.Types.CachedEntry
import           Web.Saeplog.Util

newtype Blog m = Blog (Maybe (TVar (Internal.Blog m)))

getBlogConfig :: (Functor io, MonadIO io)
              => Blog m -> io (Maybe (BlogConfig m))
getBlogConfig (Blog Nothing) = return Nothing
getBlogConfig (Blog (Just blog)) = Just . view blogConfig <$> liftIO (readTVarIO blog)

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

-- TODO saep 2014-11-05 Document those (e.g. exposing Web.Saeplog.Blog.Query)
-- | Generate a list of blog entries. The size and order of the list is
-- determined by the supplied request data.
blogEntries :: (Monad m) => Blog m -> ServerPartT IO (m Html)
blogEntries (Blog Nothing) = mzero
blogEntries (Blog (Just tb)) = do
    _ <- liftIO . forkIO $ manageEntryUpdates tb
    b <- liftIO $ readTVarIO tb
    qry <- parseQueryRqData
    case eqId qry of
        Just i -> renderEntries b (Left [i])
        Nothing -> do
            let es = sortBy (eqSortBy qry) $ IxSet.toList (b^.entries)
            renderEntries b (Right es)

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


