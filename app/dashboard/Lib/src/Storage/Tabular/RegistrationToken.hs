{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.RegistrationToken where

import qualified Domain.Types.RegistrationToken as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.App (RegToken)
import Kernel.Types.Id
import Storage.Tabular.Merchant (MerchantTId)
import Storage.Tabular.Person (PersonTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RegistrationTokenT sql=registration_token
      id Text
      token RegToken
      personId PersonTId
      merchantId MerchantTId
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey RegistrationTokenT where
  type DomainKey RegistrationTokenT = Id Domain.RegistrationToken
  fromKey (RegistrationTokenTKey _id) = Id _id
  toKey (Id id) = RegistrationTokenTKey id

instance TType RegistrationTokenT Domain.RegistrationToken where
  fromTType RegistrationTokenT {..} = do
    return $
      Domain.RegistrationToken
        { id = Id id,
          personId = fromKey personId,
          merchantId = fromKey merchantId,
          ..
        }
  toTType Domain.RegistrationToken {..} =
    RegistrationTokenT
      { id = getId id,
        personId = toKey personId,
        merchantId = toKey merchantId,
        ..
      }
