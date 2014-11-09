{-# LANGUAGE OverloadedStrings #-}
{- |
Module      :  Web.Saeplog.Templates.Default
Description :  The default blog template
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Web.Saeplog.Templates.Default
    where

import Data.Time
import System.Locale
import qualified Web.Saeplog.Config as Config
import Web.Saeplog.Types.Entry as E
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Web.Saeplog.Util
import qualified Data.IxSet as IxSet
import qualified Data.Set as Set
import qualified Data.Text as Text

entryRenderer :: Monad m => Config.BlogConfig m -> [(Entry, Html)] -> m Html
entryRenderer cfg filteredEntries  = case filteredEntries of
    [] -> return $ return ()
    [(d, m)] ->
        let metaTable = renderMetaTable (Config.timeFormatter cfg) d
        in return $ withBlogHeader [metaTable, m]
    es -> do
        bURL <- Config.baseURL cfg
        return . forM_ es $ \(d,m) -> do
            let metaBox = renderMetaBox (Config.timeFormatter cfg) bURL d
            withBlogHeader [metaBox, m]

cssFileName :: Text
cssFileName = "default.css"

timeFormatter :: UTCTime -> Text
timeFormatter = pack . formatTime defaultTimeLocale (iso8601DateFormat Nothing)

renderMetaTable :: (UTCTime -> Text) -> Entry -> Html
renderMetaTable tf = renderMetaDataForEntry
  where
    renderMetaDataForEntry ed = H.div ! class_ "meta-table" $
        table $ tbody $ mapM_ trow
            [ ("Title:", ed^.E.title)
            , ("Author:", ed^.author)
            , ("Tags:", (Text.unwords . Set.toList) (ed^.tags))
            , ("Last Update:", (tf . entryUpdateTime) (ed^.lastUpdate))
            ]
    trow :: (Text, Text) -> Html
    trow (l,r) = tr $ td (toHtml l) >> td (toHtml r)

renderMetaBox :: (UTCTime -> Text) -> Text -> Entry -> Html
renderMetaBox tf baseURL ed =
    let url = Text.concat [baseURL, "?id=", (pack . show) (ed^.entryId)]
    in H.a ! class_ "meta" ! href (toValue url) $ do
        H.span $ (toHtml . tf . entryUpdateTime . Set.findMax . IxSet.toSet) (ed^.updates)
        br
        H.span $ toHtml $ "by " <> ed^.author


-- | Wrap the given 'Html' fragments inside a:
-- @<div class="blog-with-metadata"><section class="blog"> Html fragments </section></div>@
withBlogHeader :: [Html] -> Html
withBlogHeader es =
    H.div ! class_ "blog-with-metadata" $
        section ! class_ "blog" $ sequence_ es

-- | Given the path to the blog entries and the path to the static resources,
-- create a 'BlogConfig' value that is otherwise containing the default
-- configuration.
createDefaultBlogConfig :: (Monad m) => FilePath -> FilePath -> Config.BlogConfig m
createDefaultBlogConfig ep resp = Config.BlogConfig
    { Config.baseURL = return "http://127.0.0.1/blog"
    , Config.entryRenderer = entryRenderer
    , Config.timeFormatter = timeFormatter
    , Config.entryPath = ep
    , Config.resourcesPath = resp
    , Config.updateInterval = 10
    }

