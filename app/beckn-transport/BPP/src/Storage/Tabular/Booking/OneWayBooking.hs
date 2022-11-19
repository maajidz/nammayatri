{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Booking.OneWayBooking where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Types.Common (HighPrecMeters)
import Beckn.Types.Id
import Beckn.Utils.Common (Seconds)
import qualified Domain.Types.Booking.Type as Domain
import Storage.Tabular.Booking.BookingLocation
import Storage.Tabular.Booking.Table

-- FIXME estimated_finish_time should be not null in DB
mkPersist
  defaultSqlSettings
  [defaultQQ|
    OneWayBookingT sql=one_way_booking
      bookingId BookingTId
      estimatedFinishTime UTCTime
      toLocationId BookingLocationTId
      estimatedDistance HighPrecMeters
      estimatedDuration Seconds
      Primary bookingId
      deriving Generic
    |]

instance TEntityKey OneWayBookingT where
  type DomainKey OneWayBookingT = Id Domain.Booking
  fromKey (OneWayBookingTKey _id) = fromKey _id
  toKey id = OneWayBookingTKey $ toKey id
