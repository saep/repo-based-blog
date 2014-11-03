{-# LANGUAGE TemplateHaskell #-}
{- |
Module      :  Web.Saeplog.Blog.State
Description :  Mutable state of the blog with utility functions
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Web.Saeplog.Blog.State
    where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.FileStore
import           Data.IxSet
import qualified Data.IxSet                 as IxSet
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Monoid
import           Data.Time
import           System.IO
import           Text.Blaze.Html5           (Html)
import           Web.Saeplog.Converter
import           Web.Saeplog.Crawler
import           Web.Saeplog.Templates.Saep
import           Web.Saeplog.Types

data Blog = Blog
    { _entries         :: IxSet Entry
    , _lastEntryUpdate :: EntryUpdate
    , _lastUpdateCheck :: UTCTime
    , _fileStore       :: FileStore
    , _blogEntryCache  :: TVar (Map Integer Html)
    }
makeLenses ''Blog

initBlog :: (MonadIO io) => FilePath -> io (Maybe Blog)
initBlog fp = do
    ed <- collectEntryData fp
    case ed of
        Left err -> do
            liftIO $ hPutStrLn stderr err
            return Nothing
        Right (fs, es) -> do
            let l = maximum . map (view lastUpdate) $ IxSet.toList es
            m <- liftIO . atomically $ newTVar mempty
            now <- liftIO getCurrentTime
            return $ Just (Blog es l now fs m)

renderEntry :: (Functor io, MonadIO io) => Integer -> ReaderT Blog io Html
renderEntry i = do
    cache <- liftIO . readTVarIO =<< view blogEntryCache
    maybe putInCache return $ Map.lookup i cache
  where
    errorPage = return . entryNotFound $ "Entry with the given id '"<>show i<>"' not found."

    putInCache = do
        es <- view entries
        case getOne $ es @= Index i of
            Nothing -> errorPage
            Just e -> do
                h <- convertToHTML e <$> liftIO (readFile (e^.fullPath))
                cache <- view blogEntryCache
                liftIO . atomically . modifyTVar cache $ Map.insert i h
                return h




