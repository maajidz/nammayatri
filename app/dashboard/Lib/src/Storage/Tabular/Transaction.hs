{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Transaction where

import qualified Domain.Types.Transaction as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.Person (PersonTId)

derivePersistField "Domain.Endpoint"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    TransactionT sql=transaction
      id Text
      requestorId PersonTId
      merchantId MerchantTId Maybe
      commonDriverId Text Maybe
      commonRideId Text Maybe
      endpoint Domain.Endpoint
      request Text Maybe
      response Text Maybe
      responseError Text Maybe
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey TransactionT where
  type DomainKey TransactionT = Id Domain.Transaction
  fromKey (TransactionTKey _id) = Id _id
  toKey (Id id) = TransactionTKey id

instance TType TransactionT Domain.Transaction where
  fromTType TransactionT {..} = do
    return $
      Domain.Transaction
        { id = Id id,
          requestorId = fromKey requestorId,
          merchantId = fromKey <$> merchantId,
          commonDriverId = Id <$> commonDriverId,
          commonRideId = Id <$> commonRideId,
          ..
        }
  toTType Domain.Transaction {..} =
    TransactionT
      { id = getId id,
        requestorId = toKey requestorId,
        merchantId = toKey <$> merchantId,
        commonDriverId = getId <$> commonDriverId,
        commonRideId = getId <$> commonRideId,
        ..
      }
