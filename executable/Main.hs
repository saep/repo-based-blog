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

import           Control.Monad.IO.Class
import           Happstack.Server       (nullConf)
import qualified Web.Saeplog.Server     as Server


import Options.Applicative

data BlogOptions = BlogOptions
    { blogEntryRepository      :: Maybe FilePath
    , staticResourcesDirectory :: Maybe FilePath
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

main :: IO ()
main = do
    args <- execParser opts
    let conf = Server.Options
            { Server.happstackConf            = nullConf
            , Server.blogEntryRepository      = blogEntryRepository args
            , Server.staticResourcesDirectory = staticResourcesDirectory args
            }
    liftIO $ Server.start conf

  where
    opts = info (helper <*> blogOptions)
      ( fullDesc
     <> progDesc "Start a web server."
     <> header "Saeplog - a blogging platform in Haskell!" )

