{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Ride.Table where

import qualified Domain.Types.Ride as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common (HighPrecMeters, Meters, Money)
import Kernel.Types.Id
import Storage.Tabular.Booking (BookingTId)
import qualified Storage.Tabular.FareParameters as Fare
import Storage.Tabular.Person (PersonTId)

derivePersistField "Domain.RideStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    RideT sql=ride
      id Text
      bookingId BookingTId
      shortId Text
      status Domain.RideStatus
      driverId PersonTId
      otp Text
      trackingUrl Text
      fare Money Maybe
      traveledDistance HighPrecMeters
      chargeableDistance Meters Maybe
      driverArrivalTime UTCTime Maybe
      tripStartTime UTCTime Maybe
      tripEndTime UTCTime Maybe
      tripStartLat Double Maybe
      tripStartLon Double Maybe
      tripEndLat Double Maybe
      tripEndLon Double Maybe
      fareParametersId Fare.FareParametersTId Maybe
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey RideT where
  type DomainKey RideT = Id Domain.Ride
  fromKey (RideTKey _id) = Id _id
  toKey (Id id) = RideTKey id
