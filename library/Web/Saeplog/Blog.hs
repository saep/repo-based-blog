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
    ) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.IxSet             as IxSet
import           Data.List              (sortBy)
import qualified Data.Map               as Map
import           Data.Time
import           Happstack.Server       (ServerPartT)
import           System.IO
import           Text.Blaze.Html        (Html)
import           Web.Saeplog.Blog.Query
import           Web.Saeplog.Converter
import           Web.Saeplog.Crawler
import           Web.Saeplog.Types
import           Web.Saeplog.Types.Blog hiding (Blog)
import qualified Web.Saeplog.Types.Blog as Internal
import           Web.Saeplog.Util

newtype Blog = Blog (Maybe (TVar Internal.Blog))

withBlog :: Maybe FilePath -> (Blog -> IO ()) -> IO ()
withBlog Nothing action = action (Blog Nothing)
withBlog (Just ep) action = do
    mb <- runExceptT $ initBlog ep
    case mb of
        Left err -> do
            hPutStrLn stderr err
            action $ Blog Nothing
        Right b -> do
            tb <- atomically $ newTVar b
            _ <- forkIO $ manageEntryCache tb (b^.blogCacheChannel)
            action $ Blog (Just tb)

-- TODO saep 2014-11-05 Document those (e.g. exposing Web.Saeplog.Blog.Query)
-- | Generate a list of blog entries. The size and order of the list is
-- determined by the supplied request data.
blogEntries :: Blog -> ServerPartT IO [Html]
blogEntries (Blog Nothing) = mzero
blogEntries (Blog (Just tb)) = do
    _ <- liftIO . forkIO $ manageEntryUpdates tb
    b <- liftIO $ readTVarIO tb
    qry <- parseQueryRqData
    -- XXX saep 2014-11-03 Ordering is somewhat random
    let es = sortBy (eqSortBy qry) $ IxSet.toList (b^.entries)
    flip runReaderT b $ forM es $ \e -> renderEntry (e^.entryId)

manageEntryUpdates :: TVar Internal.Blog -> IO ()
manageEntryUpdates tb = do
    luc <- view lastUpdateCheck <$> liftIO (readTVarIO tb)
    now <- liftIO getCurrentTime

    -- The update should not run multiple times if the timout condition is met.
    -- Wrapping the check for the condition and the modification of the last
    -- update time into one STM function will solve this.
    shouldCheckForUpdate <- liftIO . atomically $ updateUpdateTime now luc
    when shouldCheckForUpdate $ do
        b <- liftIO $ readTVarIO tb
        u <- runExceptT $ updateBlog b
        case u of
            Left err -> hPutStrLn stderr err
            Right b' -> liftIO . atomically $ writeTVar tb b'

  where
    updateUpdateTime :: UTCTime -> UTCTime -> STM Bool
    updateUpdateTime now luc
        | fromEnum (diffUTCTime now luc) > 10 * 60 * 10^12 = do
            modifyTVar tb $ lastUpdateCheck .~ now
            return True
        | otherwise = return False


manageEntryCache :: TVar Internal.Blog -> TChan (Integer, Html) -> IO ()
manageEntryCache tb tc = forever $ do
    (i,h) <- atomically $ readTChan tc
    atomically . modifyTVar tb $ blogEntryCache %~ Map.insert i h


