{- |
Module      :  RBB.Util
Description :  Globally used utility functions
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module RBB.Util
    ( module ReExport
    , ixSetModifyIx
    ) where

import           Control.Applicative as ReExport
import           Control.Lens        as ReExport
import           Control.Monad       as ReExport
import           Control.Monad.Base  as ReExport (MonadBase)
import           Data.Data           as ReExport (Typeable)
import           Data.Function       as ReExport (on)
import           Data.IxSet
import           Data.IxSet          as ReExport (IxSet)
import qualified Data.IxSet          as IxSet
import           Data.Maybe          as ReExport
import           Data.Monoid         as ReExport
import           Data.Text           as ReExport (Text, pack)
import           Data.Time           as ReExport (UTCTime)
import           Text.Blaze.Html5    as ReExport (Html)

-- | Modify the unique value indexable by @k@. If there ir no value or if there
-- are multiple values for the given index, the 'IxSet' is unmodified.
ixSetModifyIx :: (Ord a, Typeable a, IxSet.Indexable a, Typeable k)
              => k -> (a -> a) -> IxSet a -> IxSet a
ixSetModifyIx k f is = case getOne $ is @= k of
    Nothing -> is
    Just a  -> insert (f a) $ delete a is

