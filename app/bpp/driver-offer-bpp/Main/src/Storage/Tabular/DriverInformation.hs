{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.DriverInformation where

import qualified Data.ByteString as BS
import qualified Domain.Types.DriverInformation as Domain
import Domain.Types.Person (Person)
import Kernel.External.Encryption (DbHash (..), Encrypted (..), EncryptedHashed (..))
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Person (PersonTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    DriverInformationT sql=driver_information
      driverId PersonTId
      adminId PersonTId Maybe
      active Bool
      onRide Bool
      enabled Bool
      blocked Bool
      verified Bool
      lastEnabledOn UTCTime Maybe
      createdAt UTCTime
      updatedAt UTCTime
      referralCode Text Maybe
      Primary driverId
      deriving Generic
    |]

instance TEntityKey DriverInformationT where
  type DomainKey DriverInformationT = Id Person
  fromKey (DriverInformationTKey _id) = fromKey _id
  toKey id = DriverInformationTKey $ toKey id

instance TType DriverInformationT Domain.DriverInformation where
  fromTType DriverInformationT {..} = do
    return $
      Domain.DriverInformation
        { driverId = fromKey driverId,
          adminId = fromKey <$> adminId,
          referralCode = EncryptedHashed <$> (Encrypted <$> referralCode) <*> Just (DbHash BS.empty),
          ..
        }
  toTType Domain.DriverInformation {..} =
    DriverInformationT
      { driverId = toKey driverId,
        adminId = toKey <$> adminId,
        referralCode = referralCode <&> unEncrypted . (.encrypted),
        ..
      }
