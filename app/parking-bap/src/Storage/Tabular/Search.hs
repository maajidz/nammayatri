{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Tabular.Search where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Data.Time (UTCTime)
import Database.Persist.TH
import Storage.Domain.SearchLocation (SearchLocationTId)

share
  [mkPersist sqlSettings]
  [defaultQQ|
    SearchT sql=search
      Id Text default=uuid_generate_v4() sqltype=varchar(36)
      searchLocationId SearchLocationTId
      requestorId Text
      fromDate UTCTime
      toDate UTCTime
      createdAt UTCTime
      deriving Generic
    |]
