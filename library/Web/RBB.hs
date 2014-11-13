{- |
Module      :  Web.RBB
Description :  This module re-exports all necessary and some useful modules
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Web.RBB (
    -- * Configuration
    -- $configuration
    BlogConfig (..),
    createDefaultBlogConfig,

    -- * Usage
    -- ** General
    -- $usage
    Blog,
    getBlogConfig,
    withBlog,
    blogEntries,

    -- ** XMonad style usage
    -- $example
    rbb,

    -- * Entry querying
    -- ** Entry
    Entry,
    entryId,
    -- | Unique 'Entry' identifier. It you do not mess with the repositories
    -- history, this should be the same across restarts.
    title,
    -- | Title of the 'Entry'.
    author,
    -- | Author of the 'Entry'.
    authorEmail,
    tags,
    fileType,

    -- $ixsetquery
    Title(..),
    AuthorName(..),
    AuthorEmail(..),
    Tags(..),
    Index(..),

    -- ** FileType
    FileType(..),

    def,
    ) where

import Data.Default              (Default (..))
import Web.RBB.Blog              (Blog, blogEntries, getBlogConfig, withBlog)
import Web.RBB.Config            (BlogConfig (..))
import Web.RBB.Main              (rbb)
import Web.RBB.Templates.Default (createDefaultBlogConfig)
import Web.RBB.Types.Entry
import Web.RBB.Types.FileType    (FileType (..))

-- $configuration
-- The blog configuration is essentially done by setting the fields of the
-- 'BlogConfig' data type. A basic configuration can be created with the
-- function 'createDefaultBlogConfig' that only takes the absolutely necessary
-- parameters.

-- $usage
-- To use the blog in an web application, you have to create an abstact 'Blog'
-- value via the 'withBlog' function and pass it to functions that need access
-- to it because they call blog related functions.
--
-- > importWeb.RBB
-- >
-- > myBlogConfig = createDefaultBlogConfig
-- >     "/path/to/repository/with/blog/entries"
-- >     "/path/to/folder/with/static/resources"
-- >
-- > main = withBlog myBlogConfig $ \blog -> do
-- >     putStrLn "Replace this with your web application code."

-- $example
--
-- You can also define an xmonad-like configuration for your blog. If the few
-- functions from this module do not give you a basic idea on how to use this
-- library, a trivial example will not suffice.
-- The following example will contain two modules and for the sake of this
-- example, I will assume that the configuration is in
-- @~\/.config\/repo-based-blog@.
--
-- The first file is @repo-based-blog.hs@. It will containt the markup and a
-- minimal happstack server example. The blog will be present at
-- <http://127.0.0.1:8000>.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Web.RBB
-- >
-- > import Query
-- >
-- > import Text.Blaze.Html5 as H
-- > import Control.Applicative ((<$>), optional)
-- > import Control.Monad
-- > import Control.Monad.IO.Class
-- > import Data.Text                   (Text, unpack)
-- > import Happstack.Server
-- > import Data.Maybe
-- > import qualified Data.IxSet as IxSet
-- > import System.Directory
-- > import Text.Blaze.Html5.Attributes as A hiding (dir, start, id)
-- > import Happstack.Server (Conf (port), notFound, nullConf, toResponse, simpleHTTP)
-- >
-- > siteTemplate :: (Monad m, Functor m)
-- >              => [Html] -- ^ Additional Headers
-- >              -> [Html] -- ^ Body content
-- >              -> m Html
-- > siteTemplate hs bodyContent = return $ do
-- >         docType
-- >         html $ do
-- >             H.head $ do
-- >                 meta ! httpEquiv "Content-Type"
-- >                      ! content "text/html;charset=utf-8"
-- >                 meta ! content "width=device-width, initial-scale=1, maximum-scale=1"
-- >                      ! name "viewport"
-- >                 link ! rel "stylesheet" ! type_ "text/css"
-- >                      ! href "resources/default.css"
-- >                 sequence_ hs
-- >             H.body $ do
-- >                 topNavigationBar [ ("Blog", Just "?")
-- >                                  , ("Github", Just "https://github.com/saep")
-- >                                  ]
-- >                 sequence_ bodyContent
-- >
-- > topNavigationBar :: [(Html, Maybe String)] -> Html
-- > topNavigationBar [] = return ()
-- > topNavigationBar xs =
-- >     H.div ! class_ "horiz-nav" $
-- >         H.nav ! A.class_ "horiz-nav" $
-- >             ul ! class_ "horiz-nav" $
-- >                 forM_ xs $ \(t, ml) ->
-- >                     case ml of
-- >                         Just l -> li $ a ! href (toValue l) $ t
-- >                         _ -> li t
-- >
-- > serveBlog :: Blog (ServerPartT IO)
-- >           -> ServerPartT IO Response
-- > serveBlog blog = do
-- >     qd <- parseQueryRqData
-- >     mId <- join . fmap readMaybe <$> optional (look "id")
-- >     let qfun = fmap (IxSet.getEQ . Index) mId
-- >     entries <- join $ blogEntries blog qd qfun
-- >     blogMarkup <- siteTemplate hs [entries]
-- >     ok . toResponse $ blogMarkup -- happstack specific
-- >   where
-- >     hs = [ H.title "Saeptain's log" ]
-- >
-- > myBlogConfig :: BlogConfig (ServerPartT IO)
-- > myBlogConfig =
-- >     let cfg = createDefaultBlogConfig "/home/saep/git/myblog"
-- >     in cfg { baseURL = return "http://127.0.0.1:8000/blog" }
-- >
-- > main :: IO ()
-- > main = rbb $ withBlog myBlogConfig $ \b -> do
-- >     liftIO $ simpleHTTP (nullConf { port = 8000 }) $ msum
-- >         [ dir "resources" $ serveDirectory DisableBrowsing [] "/home/saep/git/blog/resources"
-- >         , serveBlog b
-- >         , notFound $ toResponse ()
-- >         ]
--
-- The Query module, which you will be missing apart from the obvious missing
-- libraries is located in @lib/Query.hs@
--
-- > module Query where
-- >
-- > import Happstack.Server (look, HasRqData, ServerPartT)
-- > import Web.RBB.Blog.Query
-- > import Control.Applicative
-- > import Data.Maybe
-- >
-- > readMaybe :: Read a => String -> Maybe a
-- > readMaybe x = case reads x of
-- >     [(v,_)] -> Just v
-- >     _ -> Nothing
-- >
-- > -- | 'look' at the request data of the given name and try to parse it via
-- > -- 'readMaybe'. If the parse failed or the request data did not exist, return
-- > -- the provided default.
-- > maybeLookAndRead :: (Monad m, Read a, Alternative m, HasRqData m)
-- >                  => a -> String -> m a
-- > maybeLookAndRead a qry = do
-- >     l <- optional $ look qry
-- >     return $ fromMaybe a (maybe (Just a) readMaybe l)
-- >
-- > -- | Parse the supported request data and present it in a data type.
-- > parseQueryRqData :: ServerPartT IO EntryQuery
-- > parseQueryRqData = EntryQuery
-- >     <$> (sortMethodToComparator
-- >         <$> maybeLookAndRead Update "sortBy"
-- >         <*> maybeLookAndRead Descending "sortOrder")



-- $ixsetquery
-- These newtype wrappers are used by the 'IxSet' of 'Entry' values and needed
-- for search queries.
--
-- > queryEntryForIdentifier :: Ixset Entry -> Maybe Entry
-- > queryEntryForIdentifier set = getOne $ set @= Index 42
--
