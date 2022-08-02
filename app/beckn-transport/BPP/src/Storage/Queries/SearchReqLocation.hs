{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Storage.Queries.SearchReqLocation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.SearchRequest.SearchReqLocation
import Storage.Tabular.SearchRequest.SearchReqLocation ()

create :: SearchReqLocation -> SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id SearchReqLocation ->
  m (Maybe SearchReqLocation)
findById = Esq.findById
