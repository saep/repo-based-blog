{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
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
    , Default(def)
    -- | * Rendering options
    , RenderOptions
    , withMetaBox
    , withMetaTable
    , timeFormat
    ) where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Default
import           Data.IxSet                    as IxSet
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                      as Set
import           Data.Text                     as Text (concat, unwords)
import           Data.Time
import           System.Locale
import           Text.Blaze.Html5              as H
import           Text.Blaze.Html5.Attributes   as A
import           Text.Pandoc.Options
import           Text.Pandoc.Readers.Markdown
import           Text.Pandoc.Writers.HTML
import           Web.Saeplog.Types             as E
import           Web.Saeplog.Types.Blog
import           Web.Saeplog.Types.CachedEntry as E
import           Web.Saeplog.Util

data RenderOptions = RenderOptions
    { _withMetaBox   :: Bool
    , _withMetaTable :: Bool
    , _timeFormat    :: UTCTime -> Text
    }
makeLenses ''RenderOptions

instance Default RenderOptions where
    def = RenderOptions
            True
            False
            fmtTime

-- FIXME saep 2014-11-05 Cache entry should be independent of RenderOptions
-- (TypeClass/DataType with rendering functions for the different cases (single
-- entry, multiple entries, table of contents...)
-- | Render the page with the given identifier from the blog context. Will
-- print a minimal error page if the identifier does not point to a valid blog
-- entry.
renderEntry :: (Functor io, MonadIO io)
            => Text -> RenderOptions -> Integer -> ReaderT Blog io Html
renderEntry baseURL ropt j = do
    cache <- view blogEntryCache
    maybeCachedEntry <- manageCache $ Map.lookup j cache
    return $ renderCachedEntry baseURL ropt maybeCachedEntry
  where
    manageCache :: (Functor io, MonadIO io) => Maybe CachedEntry -> ReaderT Blog io (Maybe CachedEntry)
    manageCache mce = do
        es <- view entries
        case (mce, getOne $ es @= Index j) of
            (_,Nothing) -> return Nothing
            (Just ce, Just e) | ((==) `on` updateTime) (ce^.cEntry) e -> return $ Just ce
            (_, Just e) -> do
                en <- convertToHTML e <$> liftIO (readFile (e^.fullPath))
                let mb = renderMetaBox baseURL ropt e
                let mt = renderMetaTable ropt e
                let h = CachedEntry mb mt en (e^.lastUpdate) e
                c <- view blogCacheChannel
                liftIO . atomically $ writeTChan c (j,h)
                return $ Just h
      where
        updateTime e = entryUpdateTime (e^.lastUpdate)

renderCachedEntry :: Text -> RenderOptions -> Maybe CachedEntry -> Html
renderCachedEntry _ _ Nothing = toHtml $ pack "Page not Found"
renderCachedEntry baseURL ropt (Just ce) =
    withBlogHeader $ catMaybes [mb,mt,Just (ce^.cEntryMarkup)]
  where
    mb = do guard (ropt^.withMetaBox)
            Just $ renderMetaBox baseURL ropt (ce^.cEntry)
    mt = do guard (ropt^.withMetaTable)
            Just $ renderMetaTable ropt (ce^.cEntry)

-- | Converter function that choses an appropriate pandoc configuration for the
-- given 'FileType' and converts the given 'String' to 'Html'.
fileContentToHtml :: FileType -> String -> Html
fileContentToHtml ft = article . writeHtml defaultWriter . case ft of
    PandocMarkdown ->
        readMarkdown (def
        { readerExtensions = pandocExtensions })
    LiterateHaskell ->
        readMarkdown (def
        { readerExtensions = Ext_literate_haskell `Set.insert` pandocExtensions })

-- | Wrap the given 'Html' fragments inside a:
-- @<div class="blog-with-metadata"><section class="blog"> Html fragments </section></div>@
withBlogHeader :: [Html] -> Html
withBlogHeader es =
    H.div ! class_ "blog-with-metadata" $
        section ! class_ "blog" $ sequence_ es

-- | Convert the given file contents given as a 'String' to an 'Html' element
-- and add some meta data from the 'EntryData' argument'.
convertToHTML :: Entry -> String -> Html
convertToHTML ed = fileContentToHtml (ed^.fileType)

renderMetaTable :: RenderOptions -> Entry -> Html
renderMetaTable ropt e = renderMetaDataForEntry e
  where
    renderMetaDataForEntry e = H.div ! class_ "meta-table" $
        table $ tbody $ mapM_ trow
            [ ("Title:", e^.E.title)
            , ("Author:", e^.author)
            , ("Tags:", (Text.unwords . Set.toList) (e^.tags))
            , ("Last Update:", ((ropt^.timeFormat) . entryUpdateTime) (e^.lastUpdate))
            ]
    trow :: (Text, Text) -> Html
    trow (l,r) = tr $ td (toHtml l) >> td (toHtml r)

renderMetaBox :: Text -> RenderOptions -> Entry -> Html
renderMetaBox baseURL ropt ed =
    let url = Text.concat [baseURL, "?id=", (pack . show) (ed^.entryId)]
    in H.a ! class_ "meta" ! href (toValue url) $ do
        H.span $ (toHtml . (ropt^.timeFormat) . entryUpdateTime . Set.findMax . toSet) (ed^.updates)
        br
        H.span $ toHtml $ "by " <> ed^.author

-- | Default 'WriterOptions' for the converters.
defaultWriter :: WriterOptions
defaultWriter = def
    { writerHtml5 = True
    , writerEmailObfuscation = ReferenceObfuscation
    , writerHighlight = True
    }

fmtTime :: UTCTime -> Text
fmtTime = pack . formatTime defaultTimeLocale (iso8601DateFormat Nothing)
