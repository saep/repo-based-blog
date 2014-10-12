{-# LANGUAGE OverloadedStrings #-}
{- |
Module      :  Web.Saeplog.Templates.Saep
Description :  Example template stled by saep
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Web.Saeplog.Templates.Saep
    where

import Control.Monad
import Text.Blaze.Html5            as H
import Text.Blaze.Html5.Attributes as A

-- | A navigation bar at the top that occupies all of the screen's witdh.
topNavigationBar :: [(Html, Maybe String)] -> Html
topNavigationBar [] = return ()
topNavigationBar xs =
    H.nav ! A.class_ "horiz-nav" $
        ul ! class_ "horiz-nav" $
            forM_ xs $ \(t, ml) ->
                case ml of
                    Just l -> li $ a ! href (toValue l) $ t
                    _ -> li t
