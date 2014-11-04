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

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.FileStore
import qualified Data.IxSet             as IxSet
import           Data.Time
import           Happstack.Server       (ServerPartT, look)
import           System.IO
import           Text.Blaze.Html        (Html)
import           Web.Saeplog.Converter
import           Web.Saeplog.Crawler
import           Web.Saeplog.Types
import           Web.Saeplog.Types.Blog hiding (Blog)
import qualified Web.Saeplog.Types.Blog as Internal

newtype Blog = Blog { getBlog :: Maybe (TVar Internal.Blog) }

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
            action $ Blog (Just tb)

blogEntries :: Blog -> ServerPartT IO [Html]
blogEntries (Blog Nothing) = mzero
blogEntries (Blog (Just tb)) = do
    luc <- view lastUpdateCheck <$> liftIO (readTVarIO tb)
    now <- liftIO getCurrentTime
    when (fromEnum (diffUTCTime now luc) > 10 * 60 * 10^12) $ do
        liftIO . atomically . modifyTVar tb $ lastUpdateCheck .~ now
        b <- liftIO $ readTVarIO tb
        let lu = Just . entryUpdateTime $ b^.lastEntryUpdate
            lur = entryRevisionId $ b^.lastEntryUpdate
        rs <- liftIO $ (history (b^.fileStore)) [] (TimeRange lu Nothing) Nothing
        -- FIXME saep 2014-11-03 implement the incremental update by refactoring
        -- the Web.Saeplog.Crawler.Repository module
        return ()

    eId <- optional $ look "id"
    b <- liftIO $ readTVarIO tb
    -- XXX saep 2014-11-03 Ordering is somewhat random
    flip runReaderT b $ mapM renderEntry . reverse . map _entryId $ IxSet.toList (b^.entries)


