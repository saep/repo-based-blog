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
    ) where

import Web.Saeplog.Types

import           Control.Lens
import           Data.IxSet                   as IxSet
import           Data.Monoid
import qualified Data.Set                     as Set
import           Data.Time
import           System.Locale
import           Text.Blaze.Html5             as H
import           Text.Blaze.Html5.Attributes  as A
import           Text.Pandoc.Options
import           Text.Pandoc.Readers.Markdown
import           Text.Pandoc.Writers.HTML

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
