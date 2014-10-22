{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Web.Saeplog.State
    where

import Control.Applicative  ((<$>))
import Control.Exception    (bracket)
import Control.Monad        (msum)
import Control.Monad.Reader (ask)
import Control.Monad.State  (get, put)
import Data.Acid            (AcidState, Query, Update, makeAcidic,
                             openLocalState)
import Data.Acid.Advanced   (query', update')
import Data.Acid.Local      (createCheckpointAndClose)
import Data.Data            (Data, Typeable)
import Data.SafeCopy        (base, deriveSafeCopy)
import Happstack.Server     (Response, ServerPart, dir, nullConf, nullDir, ok,
                             simpleHTTP, toResponse)
