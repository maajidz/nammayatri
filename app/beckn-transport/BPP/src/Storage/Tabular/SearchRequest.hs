{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.SearchRequest where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Id
import qualified Domain.Types.SearchRequest as Domain
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.SearchRequest.SearchReqLocation (SearchReqLocationTId)
import qualified Storage.Tabular.SearchRequest.SearchReqLocation as SLoc

derivePersistField "Domain.SearchRequestStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SearchRequestT sql=search_request
      id Text
      messageId Text
      startTime UTCTime
      validTill UTCTime
      providerId MerchantTId
      fromLocationId SearchReqLocationTId
      toLocationId SearchReqLocationTId Maybe
      bapId Text
      bapUri Text
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey SearchRequestT where
  type DomainKey SearchRequestT = Id Domain.SearchRequest
  fromKey (SearchRequestTKey _id) = Id _id
  toKey (Id id) = SearchRequestTKey id

type FullSearchRequestT = (SearchRequestT, SLoc.SearchReqLocationT, Maybe SLoc.SearchReqLocationT)

instance TType FullSearchRequestT Domain.SearchRequest where
  fromTType (SearchRequestT {..}, fromLoc, mbToLoc) = do
    pUrl <- parseBaseUrl bapUri
    fromLocation <- fromTType fromLoc
    toLocation <- mapM fromTType mbToLoc
    return $
      Domain.SearchRequest
        { id = Id id,
          providerId = fromKey providerId,
          bapUri = pUrl,
          ..
        }
  toTType Domain.SearchRequest {..} = do
    let fromLoc = toTType fromLocation
        mbToLoc = toTType <$> toLocation
        searchReq =
          SearchRequestT
            { id = getId id,
              providerId = toKey providerId,
              fromLocationId = toKey fromLocation.id,
              toLocationId = toKey <$> (toLocation <&> (.id)),
              bapUri = showBaseUrl bapUri,
              ..
            }
    (searchReq, fromLoc, mbToLoc)
