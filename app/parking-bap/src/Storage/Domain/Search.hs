{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Domain.Search
  ( module Storage.Domain.Search,
    module Reexport,
  )
where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import Data.Time (UTCTime)
import Storage.Tabular.Search
import Storage.Tabular.Search as Reexport (SearchT)
import Storage.Tabular.Search as Reexport hiding (SearchT (..))

data Search = Search
  { id :: Id Search,
    searchLocationId :: Text, -- TODO: CHANGE THESE, REMIND ME IF I FORGOT
    requestorId :: Text, -- TODO: CHANGE THESE, REMIND ME IF I FORGOT
    fromDate :: UTCTime,
    toDate :: UTCTime,
    createdAt :: UTCTime
  }

instance TEntityKey SearchT Search where
  fromKey (SearchTKey _id) = Id _id
  toKey Search {id} = SearchTKey id.getId

instance TEntity SearchT Search where
  fromTEntity entity = do
    let _id = fromKey $ entityKey entity
        SearchT {..} = entityVal entity
    return $
      Search
        { id = _id,
          searchLocationId = searchTSearchLocationId,
          requestorId = searchTRequestorId,
          fromDate = searchTFromDate,
          toDate = searchTToDate,
          createdAt = searchTCreatedAt
        }
  toTType Search {..} = do
    SearchT
      { searchTSearchLocationId = searchLocationId,
        searchTRequestorId = requestorId,
        searchTFromDate = fromDate,
        searchTToDate = toDate,
        searchTCreatedAt = createdAt
      }
  toTEntity a = do
    Entity (toKey a) $ toTType a
