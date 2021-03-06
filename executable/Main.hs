{- |
Module      :  Main
Description :  Saeplog web server
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Main where

import System.Exit
import System.IO
import Web.RBB     (rbb)

main :: IO ()
main = rbb $ do
    mapM_ (hPutStrLn stderr)
        [ ""
        , "You do not seem to have configured your web site at all!"
        , ""
        , "As you should decide how your site is going to look like, the"
        , "default configuration does not do anything other than printing"
        , "this help text."
        , ""
        , "To get you started on creating your own website with haskell and"
        , "this blogging library, check out the documentation on github:"
        , "    https://github.com/saep/repo-based-blog"
        , ""
        ]
    exitFailure

