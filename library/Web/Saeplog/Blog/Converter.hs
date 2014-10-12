{-# LANGUAGE OverloadedStrings #-}
{- |
Module      :  Web.Saeplog.Blog.Converter
Description :  Converter functions
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental


-}
module Web.Saeplog.Blog.Converter
    ( convertToHTML
    ) where

import Web.Saeplog.Blog.Types

import qualified Data.Set                     as Set
import           Data.Time
import           System.Locale
import           Text.Blaze.Html5             as H
import           Text.Blaze.Html5.Attributes  as A
import           Text.Pandoc.Options
import           Text.Pandoc.Readers.Markdown
import           Text.Pandoc.Writers.HTML

-- | Convert the given file contents given as a 'String' to an 'Html' element
-- and add some meta data from the 'EntryData' argument'.
convertToHTML :: EntryData -> String -> Html
convertToHTML ed fileContent =
    H.div ! class_ "blog-with-metadata" $
        section ! class_ "blog" $ do
            H.div ! class_ "meta" $ H.span $
                (fmtTime . fst . Set.findMax) (updates ed)
            writeHtml def $ case fileType ed of
                PandocMarkdown ->
                    readMarkdown (def
                    { readerExtensions = pandocExtensions })
                    fileContent
                LiterateHaskell ->
                    readMarkdown (def
                    { readerExtensions = Ext_literate_haskell `Set.insert` pandocExtensions })
                    fileContent


fmtTime :: UTCTime -> Html
fmtTime = toHtml . formatTime defaultTimeLocale (iso8601DateFormat Nothing)
