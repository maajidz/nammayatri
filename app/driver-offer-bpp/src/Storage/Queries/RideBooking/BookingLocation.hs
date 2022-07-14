{-# LANGUAGE PartialTypeSignatures #-}

module Storage.Queries.RideBooking.BookingLocation where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.RideBooking.BookingLocation
import Storage.Tabular.RideBooking.BookingLocation
import Utils.Common

updateAddress :: Id BookingLocation -> LocationAddress -> SqlDB ()
updateAddress blId LocationAddress {..} = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ BookingLocationStreet =. val street,
        BookingLocationDoor =. val door,
        BookingLocationCity =. val city,
        BookingLocationState =. val state,
        BookingLocationCountry =. val country,
        BookingLocationBuilding =. val building,
        BookingLocationAreaCode =. val areaCode,
        BookingLocationArea =. val area,
        BookingLocationUpdatedAt =. val now
      ]
    where_ $ tbl ^. BookingLocationTId ==. val (toKey blId)
