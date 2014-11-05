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
    , withBlogHeader
    , renderEntry
    ) where

import Web.Saeplog.Types

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.IxSet                   as IxSet
import qualified Data.Map                     as Map
import           Data.Monoid
import qualified Data.Set                     as Set
import           Data.Time
import           System.Locale
import           Text.Blaze.Html5             as H
import           Text.Blaze.Html5.Attributes  as A
import           Text.Pandoc.Options
import           Text.Pandoc.Readers.Markdown
import           Text.Pandoc.Writers.HTML
import           Web.Saeplog.Types.Blog

-- | Render the page with the given identifier from the blog context. Will
-- print a minimal error page if the identifier does not point to a valid blog
-- entry.
renderEntry :: (Functor io, MonadIO io) => Integer -> ReaderT Blog io Html
renderEntry j = do
    cache <- view blogEntryCache
    maybe putInCache return $ Map.lookup j cache
  where
    -- TODO saep 2014-11-05 nicer error page
    errorPage = return . toHtml $ "Entry with the given id '"<>show j<>"' not found."

    putInCache = do
        es <- view entries
        case getOne $ es @= Index j of
            Nothing -> errorPage
            Just e -> do
                h <- convertToHTML e <$> liftIO (readFile (e^.fullPath))
                c <- view blogCacheChannel
                liftIO . atomically $ writeTChan c (j,h)
                return h

-- | Converter function that choses an appropriate pandoc configuration for the
-- given 'FileType' and converts the given 'String' to 'Html'.
fileContentToHtml :: FileType -> String -> Html
fileContentToHtml ft fileContent = writeHtml defaultWriter $ case ft of
    PandocMarkdown ->
        readMarkdown (def
        { readerExtensions = pandocExtensions })
        fileContent
    LiterateHaskell ->
        readMarkdown (def
        { readerExtensions = Ext_literate_haskell `Set.insert` pandocExtensions })
        fileContent

-- | Wrap the given 'Html' fragments inside a:
-- @<div class="blog-with-metadata"><section class="blog"> Html params </section></div>@
withBlogHeader :: [Html] -> Html
withBlogHeader es =
    H.div ! class_ "blog-with-metadata" $
        section ! class_ "blog" $ sequence_ es

-- | Convert the given file contents given as a 'String' to an 'Html' element
-- and add some meta data from the 'EntryData' argument'.
convertToHTML :: Entry -> String -> Html
convertToHTML ed fileContent =
    withBlogHeader
        [ H.a ! class_ "meta" ! href "TODO" $ do -- FIXME
            H.span $ (fmtTime . entryUpdateTime . Set.findMax . toSet) (ed^.updates)
            br
            H.span $ toHtml $ "by " <> ed^.author
        , H.article $ fileContentToHtml (ed^.fileType) fileContent ]

-- | Default 'WriterOptions' for the converters.
defaultWriter :: WriterOptions
defaultWriter = def
    { writerHtml5 = True
    , writerEmailObfuscation = ReferenceObfuscation
    , writerHighlight = True
    }

fmtTime :: UTCTime -> Html
fmtTime = toHtml . formatTime defaultTimeLocale (iso8601DateFormat Nothing)
