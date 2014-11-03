{-# LANGUAGE OverloadedStrings #-}
{- |
Module      :  Main
Description :  Saeplog web server
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Main where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Maybe
import Data.Monoid
import Data.Text              (pack)
import Example
import Happstack.Server       (Conf (port), notFound, nullConf, toResponse, simpleHTTP)
import Options.Applicative
import Web.Routes.Happstack   (implSite)
import Web.Saeplog.Blog

data BlogOptions = BlogOptions
    { blogEntryRepository      :: Maybe FilePath
    , staticResourcesDirectory :: Maybe FilePath
    , address                  :: Maybe String
    , listenPort               :: Maybe Int
    }

blogOptions :: Parser BlogOptions
blogOptions = BlogOptions
    <$> optional (strOption
        ( long "entry-path"
        <> short 'e'
        <> metavar "PATH"
        <> help "Path to the git repository that holds the blog entries."))
    <*> optional (strOption
        ( long "resources"
        <> short 'r'
        <> metavar "PATH"
        <> help "Path to the folder which contains static resources (e.g. \
            \style sheets)"))
    <*> optional (strOption
        ( long "address"
        <> short 'a'
        <> metavar "ADDRESS"
        <> help "Set the base address for the web page. (Default:http://127.0.0.1:8000)"))
    <*> optional (option auto
        ( long "port"
        <> short 'p'
        <> metavar "PORT"
        <> help "Set the port the server will listen on. (Default:8000)"))


main :: IO ()
main = do
    args <- execParser opts
    let siteAddress = pack $ fromMaybe "http://127.0.0.1:8000" (address args)
        sitePort    = fromMaybe 8000 $ listenPort args

    withBlog (blogEntryRepository args) $ \b -> do
        liftIO $ simpleHTTP (nullConf { port = sitePort }) $ msum
            [ flip runReaderT (staticResourcesDirectory args) $ implSite siteAddress "" (site b)
            , notFound $ toResponse ()
            ]

  where
    opts = info (helper <*> blogOptions)
      ( fullDesc
     <> progDesc "Start a web server."
     <> header "Saeplog - a blogging platform in Haskell!" )

