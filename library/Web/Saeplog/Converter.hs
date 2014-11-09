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

-- | Given a bunch of entries and a 'Blog'
renderEntries :: (Functor io, MonadIO io, Monad m)
              => Blog m -> Either [Integer] [Entry] -> io (m Html)
renderEntries blog is = do
    cachedEntries <- foldM manageCache [] $ select is
    let bcfg = blog^.blogConfig
    return . entryRenderer bcfg bcfg $ reverse cachedEntries
  where
    select :: Either [Integer] [Entry] -> [(Maybe CachedEntry, Maybe Entry)]
    select =
        let c = blog^.blogEntryCache
            es = blog^.entries
        in either
            (map (\i -> (Map.lookup i c, getOne (es @= Index i))))
            (map (\e -> (Map.lookup (e^.entryId) c, Just e)))

    manageCache :: (Functor io, MonadIO io)
                => [(Entry, Html)]
                -> (Maybe CachedEntry, Maybe Entry)
                -> io [(Entry, Html)]
    manageCache acc ie =
        case ie of
            (_,Nothing) -> return acc
            (Just ce, Just e) | ((==) `on` updateTime) (ce^.cacheEntryData) e
                        -> return $ (ce^.cacheEntryData, ce^.entry) : acc
            (_, Just e) -> do
                en <- convertToHTML e <$> liftIO (readFile (e^.fullPath))
                let h = CachedEntry en (e^.lastUpdate) e
                liftIO . atomically $
                    writeTChan (blog^.blogCacheChannel) (e^.entryId, h)
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
