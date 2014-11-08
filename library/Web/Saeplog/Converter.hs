{-# LANGUAGE OverloadedStrings #-}
{- |
Module      :  Web.Saeplog.Converter
Description :  Converter functions
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental


-}
module Web.Saeplog.Converter
    ( convertToHTML
    , fileContentToHtml
    , renderEntries
    ) where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Default
import           Data.IxSet                    as IxSet
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Text.Pandoc.Options
import           Text.Pandoc.Readers.Markdown
import           Text.Pandoc.Writers.HTML
import           Web.Saeplog.Config
import           Web.Saeplog.Types             as E
import           Web.Saeplog.Types.Blog
import           Web.Saeplog.Types.CachedEntry as E
import           Web.Saeplog.Util

renderEntries :: (Functor io, MonadIO io)
              => Blog -> Either [Integer] [Entry] -> io Html
renderEntries blog is = do
    cachedEntries <- foldM manageCache [] $ select is
    let bcfg = blog^.blogConfig
    return . entryRenderer bcfg bcfg $ reverse cachedEntries
  where
    select :: Either [Integer] [Entry] -> [(Maybe CachedEntry, Maybe Entry, Integer)]
    select =
        let c = blog^.blogEntryCache
            es = blog^.entries
        in either
            (map (\i -> (Map.lookup i c, getOne (es @= Index i), i)))
            (map (\e -> let eid = (e^.entryId) in (Map.lookup eid c, Just e, eid)))

    manageCache :: (Functor io, MonadIO io)
                => [(Entry, Html)]
                -> (Maybe CachedEntry, Maybe Entry, Integer)
                -> io [(Entry, Html)]
    manageCache acc ie =
        case ie of
            (_,Nothing, _) -> return acc
            (Just ce, Just e, _) | ((==) `on` updateTime) (ce^.cacheEntryData) e
                        -> return $ (ce^.cacheEntryData, ce^.entry) : acc
            (_, Just e, i) -> do
                en <- convertToHTML e <$> liftIO (readFile (e^.fullPath))
                let h = CachedEntry en (e^.lastUpdate) e
                liftIO . atomically $ writeTChan (blog^.blogCacheChannel) (i,h)
                return $ (e, en) : acc
      where
        updateTime e = entryUpdateTime (e^.lastUpdate)

-- | Converter function that choses an appropriate pandoc configuration for the
-- given 'FileType' and converts the given 'String' to 'Html'.
fileContentToHtml :: FileType -> String -> Html
fileContentToHtml ft = writeHtml defaultWriter . case ft of
    PandocMarkdown ->
        readMarkdown (def
        { readerExtensions = pandocExtensions })
    LiterateHaskell ->
        readMarkdown (def
        { readerExtensions = Ext_literate_haskell `Set.insert` pandocExtensions })

-- | Convert the given file contents given as a 'String' to an 'Html' element
-- and add some meta data from the 'EntryData' argument'.
convertToHTML :: Entry -> String -> Html
convertToHTML ed = fileContentToHtml (ed^.fileType)

-- | Default 'WriterOptions' for the converters.
defaultWriter :: WriterOptions
defaultWriter = def
    { writerHtml5 = True
    , writerEmailObfuscation = ReferenceObfuscation
    , writerHighlight = True
    }
