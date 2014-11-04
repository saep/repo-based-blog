{- |
Module      :  Web.Saeplog.Util
Description :  Globally used utility functions
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Web.Saeplog.Util
    where

import Data.IxSet
import Data.Data (Typeable)

-- | Modify the unique value indexable by @k@. If there ir no value or if there
-- are multiple values for the given index, the 'IxSet' is unmodified.
ixSetModifyIx :: (Ord a, Typeable a, Indexable a, Typeable k)
              => k -> (a -> a) -> IxSet a -> IxSet a
ixSetModifyIx k f is = case getOne $ is @= k of
    Nothing -> is
    Just a  -> insert (f a) $ delete a is
