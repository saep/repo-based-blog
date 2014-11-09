{-# LANGUAGE TemplateHaskell #-}
{- |
Module      :  RBB.Crawler.MetaCombiner
Description :  Contract a list of meta data into a single value type
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module RBB.Crawler.MetaCombiner
    where

import           Control.Lens                   hiding (Context)
import           Control.Monad.State
import           Data.IxSet                     hiding (null)
import qualified Data.IxSet                     as IxSet
import           Data.Monoid
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.Text                      (Text)
import           RBB.Crawler.MetaParser as M
import           RBB.Types.Entry

data MetaDataContractionState = S
    { _context     :: Maybe FilePath
    , _metaDataMap :: IxSet Entry
    }
makeLenses ''MetaDataContractionState

-- | Add the given meta information to the giten 'IxSet'.
contract :: Maybe FilePath -- ^ Content relative path for an 'Entry'
         -> [Meta]         -- ^ Parsed meta information to add
         -> IxSet Entry    -- ^ Original entry 'IxSet'
         -> IxSet Entry
contract initialContext meta m =
    let initialState = S initialContext m
    in execState (mapM_ contract' meta) initialState ^. metaDataMap

contract' :: Meta -> State MetaDataContractionState ()
contract' (Context c) = context .= Just c
contract' None = return ()
contract' meta = maybe (return ()) (updateMetaData meta) =<< use context

updateMetaDataMap :: FilePath
                  -> (Entry -> Entry)
                  -> State MetaDataContractionState ()
updateMetaDataMap c f = do
    m <- use metaDataMap
    let ixC = RelativePath c
    case getOne $ m @= ixC of
        Nothing -> return ()
        Just e -> do
            metaDataMap %= IxSet.deleteIx ixC
            metaDataMap %= IxSet.insert (f e)

updateMetaData :: Meta -> FilePath -> State MetaDataContractionState ()
updateMetaData meta c = case meta of
    M.Tags ts    -> updateMetaDataMap c $ tags %~ updateTags ts
    ~(M.Title t) -> updateMetaDataMap c $ title .~ t

updateTags :: [(TagQuantifier, Text)] -> Set Text -> Set Text
updateTags ts tset
    | null reps = foldr update tset ts
    | otherwise = foldr update mempty ts
  where
    reps = filter ((== TagReplace) . fst) ts

    update (TagRemove, t) = Set.delete t
    update (_,         t) = Set.insert t


