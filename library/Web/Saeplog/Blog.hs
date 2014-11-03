{- |
Module      :  Web.Saeplog.Blog
Description :  Very experimental Blog-serving facilties
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Web.Saeplog.Blog
    ( module Web.Saeplog.Crawler
    , BlogConfig
    , initBlog
    , createBlogEntries
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Data.Default
import Data.Function
import Data.IxSet            (toList, toSet)
import Data.List (sortBy)
import qualified Data.Set as Set
import Text.Blaze.Html5      as H
import Web.Saeplog.Converter
import Web.Saeplog.Crawler   (collectEntryData)
import Web.Saeplog.Types

-- | Dummy value for now. Might read some special configuration from the blog
-- contents repository in the future.
data BlogConfig = BlogConfig
    { entries :: [Html]
    }

instance Default BlogConfig where
    def = BlogConfig []

initBlog :: (MonadIO io) => FilePath -> io (Maybe BlogConfig)
initBlog fp = do
    es <- collectEntryData fp
    md <- liftIO $ forM (toList es) $ \e -> do
        fc <- readFile (e^.fullPath)
        return (e, fc)
    let md' = sortBy (flip compare `on` (Set.findMax . toSet . _updates . fst)) md
    let blogEntries = fmap (uncurry convertToHTML) md'
    return $ Just (BlogConfig blogEntries)

createBlogEntries :: (Functor m, Monad m)
          => Getting BlogConfig cfg BlogConfig
          -> ReaderT cfg m [Html]
createBlogEntries bcfg = entries <$> view bcfg


