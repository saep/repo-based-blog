{-# LANGUAGE TemplateHaskell #-}
{- |
Module      :  Web.Saeplog.Crawler.MetaCombiner
Description :  Contract a list of meta data into a single value type
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module Web.Saeplog.Crawler.MetaCombiner
    where

import           Control.Lens                   hiding (Context)
import           Control.Monad.State
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Monoid
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.Text                      (Text)
import           Web.Saeplog.Crawler.MetaParser as M
import           Web.Saeplog.Types.Entry

data MetaDataContractionState = S
    { _context     :: Maybe FilePath
    , _metaDataMap :: Map FilePath Entry
    }
makeLenses ''MetaDataContractionState

contract :: Maybe FilePath
         -> [Meta]
         -> Map FilePath Entry
         -> Map FilePath Entry
contract initialContext meta m =
    let initialState = S initialContext m
    in execState (mapM_ contract' meta) initialState ^. metaDataMap

contract' :: Meta -> State MetaDataContractionState ()
contract' (Context c) = context .= Just c
contract' (None    _) = return ()
contract' meta = maybe (return ()) (updateMetaData meta) =<< use context

updateMetaDataMap :: FilePath
                  -> (Entry -> Entry)
                  -> State MetaDataContractionState ()
updateMetaDataMap c f = metaDataMap %= Map.alter (fmap f) c

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


