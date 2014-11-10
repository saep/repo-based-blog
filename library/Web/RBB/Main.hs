{- |
Module      : Web.RBB.Main
Description :  Dyre wrapper
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Web.RBB.Main
    ( rbb
    ) where

import qualified Config.Dyre as Dyre
import           System.IO

-- | This function wrapping is needed to let the dyre library detect changes to
-- the configuration and recompile everything. Simply define your main in
-- @~\/.config\/repo-based-blog\/rbb.hs@ as follows:
--
-- > importWeb.RBB
-- >
-- > main = rbb $ do
-- >     putStrLn "Hello, World!"
--
rbb :: IO () -> IO ()
rbb = Dyre.wrapMain $ Dyre.defaultParams
    { Dyre.projectName = "repo-based-blog"
    , Dyre.realMain    = id
    , Dyre.showError   = \_ msg -> hPutStrLn stderr msg
    }
