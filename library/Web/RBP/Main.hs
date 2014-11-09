{- |
Module      :  RBP.Main
Description :  Dyre wrapper
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module RBP.Main
    ( saeplog
    ) where

import qualified Config.Dyre as Dyre
import System.IO

-- | This function wrapping is needed to let the dyre library detect changes to
-- the configuration and recompile everything. Simply define your main in
-- @~/.config/saeplog/saeplog.hs@ as follows:
--
-- > import RBP
-- >
-- > main = saeplog $ do
-- >     putStrLn "Hello, World!"
--
saeplog :: IO () -> IO ()
saeplog = Dyre.wrapMain $ Dyre.defaultParams
    { Dyre.projectName = "saeplog"
    , Dyre.realMain    = id
    , Dyre.showError   = \_ msg -> hPutStrLn stderr msg
    }
