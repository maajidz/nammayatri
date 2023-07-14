{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Booking where

import Domain.Types.Booking as DRB
import Domain.Types.Estimate (Estimate)
import qualified Domain.Types.LocationMapping as DLocationMapping
import Domain.Types.Merchant
import Domain.Types.Person (Person)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Storage.Queries.FullEntityBuilders (buildFullBooking)
import qualified Storage.Queries.LocationMapping as QLocationMapping
import Storage.Tabular.Booking
import qualified Storage.Tabular.Booking as RB
import qualified Storage.Tabular.DriverOffer as DrOff
import qualified Storage.Tabular.Quote as Quote
import qualified Storage.Tabular.Ride as R

-- we already created TripTerms and RentalSlab when created Quote

create :: Booking -> [DLocationMapping.LocationMapping] -> SqlDB ()
create booking mappings = do
  Esq.withFullEntity booking $ \(bookingT, fromLoc, _mbTripTermsT, bookingDetailsT) -> do
    void $ Esq.createUnique' fromLoc
    case bookingDetailsT of
      OneWayDetailsT toLocT -> traverse_ Esq.createUnique' toLocT
      RentalDetailsT _rentalSlabT -> pure ()
      DriverOfferDetailsT toLocT -> traverse_ Esq.createUnique' toLocT
      OneWaySpecialZoneDetailsT toLocT -> traverse_ Esq.createUnique' toLocT
    Esq.create' bookingT
  QLocationMapping.createMany mappings

findById :: Transactionable m => Id Booking -> m (Maybe Booking)
findById bookingId = Esq.buildDType $ do
  mbFullBookingT <- Esq.findOne' $ do
    booking <- from $ table @BookingT
    where_ $ booking ^. RB.BookingTId ==. val (toKey bookingId)
    pure booking
  join <$> mapM buildFullBooking mbFullBookingT

findByBPPBookingId :: Transactionable m => Id BPPBooking -> m (Maybe Booking)
findByBPPBookingId bppRbId = Esq.buildDType $ do
  mbFullBookingT <- Esq.findOne' $ do
    booking <- from $ table @BookingT
    where_ $ booking ^. RB.BookingBppBookingId ==. val (Just $ getId bppRbId)
    pure booking
  join <$> mapM buildFullBooking mbFullBookingT

updateStatus :: Id Booking -> BookingStatus -> SqlDB ()
updateStatus rbId rbStatus = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RB.BookingUpdatedAt =. val now,
        RB.BookingStatus =. val rbStatus
      ]
    where_ $ tbl ^. RB.BookingId ==. val (getId rbId)

updateBPPBookingId :: Id Booking -> Id BPPBooking -> SqlDB ()
updateBPPBookingId rbId bppRbId = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RB.BookingUpdatedAt =. val now,
        RB.BookingBppBookingId =. val (Just $ getId bppRbId)
      ]
    where_ $ tbl ^. RB.BookingId ==. val (getId rbId)

updateOtpCodeBookingId :: Id Booking -> Text -> SqlDB ()
updateOtpCodeBookingId rbId otp = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RB.BookingUpdatedAt =. val now,
        RB.BookingOtpCode =. val (Just otp)
      ]
    where_ $ tbl ^. RB.BookingId ==. val (getId rbId)

findLatestByRiderIdAndStatus :: Transactionable m => Id Person -> [BookingStatus] -> m (Maybe BookingStatus)
findLatestByRiderIdAndStatus riderId statusList =
  Esq.findOne $ do
    booking <- from $ table @BookingT
    where_ $
      booking ^. RB.BookingRiderId ==. val (toKey riderId)
        &&. booking ^. RB.BookingStatus `in_` valList statusList
    orderBy [desc $ booking ^. RB.BookingCreatedAt]
    limit 1
    pure $ booking ^. RB.BookingStatus

compressBooking :: [Booking] -> Maybe Booking
compressBooking list =
  if null list
    then Nothing
    else do
      let Booking {..} = head list
      let toLoc =
            concatMap
              ( \booking -> case booking.bookingDetails of
                  DRB.OneWayDetails owbd -> owbd.toLocation
                  DRB.RentalDetails _ -> []
                  DRB.DriverOfferDetails owbd -> owbd.toLocation
                  DRB.OneWaySpecialZoneDetails owszbd -> owszbd.toLocation
              )
              list

      let finalDetails = case bookingDetails of
            DRB.OneWayDetails OneWayBookingDetails {..} ->
              DRB.DriverOfferDetails
                OneWayBookingDetails
                  { toLocation = toLoc,
                    ..
                  }
            DRB.RentalDetails rs -> DRB.RentalDetails rs
            DRB.DriverOfferDetails OneWayBookingDetails {..} ->
              DRB.DriverOfferDetails
                OneWayBookingDetails
                  { toLocation = toLoc,
                    ..
                  }
            DRB.OneWaySpecialZoneDetails OneWaySpecialZoneBookingDetails {..} ->
              DRB.DriverOfferDetails
                OneWayBookingDetails
                  { toLocation = toLoc,
                    ..
                  }

      Just
        Booking
          { bookingDetails = finalDetails,
            ..
          }

findCountByRideIdAndStatus :: Transactionable m => Id Person -> BookingStatus -> m Int
findCountByRideIdAndStatus personId status = do
  mkCount <$> do
    Esq.findAll $ do
      messageReport <- from $ table @BookingT
      where_ $
        messageReport ^. BookingRiderId ==. val (toKey personId)
          &&. messageReport ^. BookingStatus ==. val status
      return (countRows :: SqlExpr (Esq.Value Int))
  where
    mkCount [counter] = counter
    mkCount _ = 0

findCountByRideIdStatusAndTime :: Transactionable m => Id Person -> BookingStatus -> UTCTime -> UTCTime -> m Int
findCountByRideIdStatusAndTime personId status startTime endTime = do
  mkCount <$> do
    Esq.findAll $ do
      booking <- from $ table @BookingT
      where_ $
        booking ^. BookingRiderId ==. val (toKey personId)
          &&. booking ^. BookingStatus ==. val status
          &&. (booking ^. BookingCreatedAt >=. val startTime &&. booking ^. BookingCreatedAt <. val endTime)
      return (countRows :: SqlExpr (Esq.Value Int))
  where
    mkCount [counter] = counter
    mkCount _ = 0

findByRiderIdAndStatus :: Transactionable m => Id Person -> [BookingStatus] -> m [Booking]
findByRiderIdAndStatus personId statusList = Esq.buildDType $ do
  fullBookingsT <- Esq.findAll' $ do
    booking <- from $ table @BookingT
    where_ $
      booking ^. RB.BookingRiderId ==. val (toKey personId)
        &&. booking ^. RB.BookingStatus `in_` valList statusList
    pure booking
  catMaybes <$> mapM buildFullBooking fullBookingsT

findAssignedByRiderId :: Transactionable m => Id Person -> m (Maybe Booking)
findAssignedByRiderId personId = Esq.buildDType $ do
  fullBookingsT <- Esq.findOne' $ do
    booking <- from $ table @BookingT
    where_ $
      booking ^. RB.BookingRiderId ==. val (toKey personId)
        &&. booking ^. RB.BookingStatus ==. val TRIP_ASSIGNED
    pure booking
  join <$> mapM buildFullBooking fullBookingsT

findBookingIdAssignedByEstimateId :: Transactionable m => Id Estimate -> m (Maybe (Id Booking))
findBookingIdAssignedByEstimateId estimateId =
  Esq.findOne $ do
    (booking :& _ :& driverOffer) <-
      from $
        table @BookingT
          `innerJoin` table @Quote.QuoteT
            `Esq.on` ( \(rb :& quote) ->
                         rb ^. RB.BookingQuoteId ==. Esq.just (quote ^. Quote.QuoteTId)
                     )
          `innerJoin` table @DrOff.DriverOfferT
            `Esq.on` ( \(_ :& quote :& driverOffer) ->
                         quote ^. Quote.QuoteDriverOfferId ==. Esq.just (driverOffer ^. DrOff.DriverOfferTId)
                     )
    where_ $
      driverOffer ^. DrOff.DriverOfferEstimateId ==. val (toKey estimateId)
        &&. booking ^. RB.BookingStatus ==. val TRIP_ASSIGNED
    pure (booking ^. BookingTId)

findAllByRiderIdAndRide :: Transactionable m => Id Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe BookingStatus -> m [Booking]
findAllByRiderIdAndRide personId mbLimit mbOffset mbOnlyActive mbBookingStatus = Esq.buildDType $ do
  let isOnlyActive = Just True == mbOnlyActive
  fullBookingsT <- Esq.findAll' $ do
    (booking :& mbRide) <-
      from $
        table @BookingT `leftJoin` table @R.RideT
          `Esq.on` (\(booking :& mbRide) -> just (booking ^. RB.BookingTId) ==. mbRide ?. R.RideBookingId)
    where_ $
      booking ^. RB.BookingRiderId ==. val (toKey personId)
        &&. ( whenTrue_ isOnlyActive (not_ (booking ^. RB.BookingStatus `in_` valList [DRB.COMPLETED, DRB.CANCELLED]))
                &&. whenJust_ mbBookingStatus (\status -> booking ^. RB.BookingStatus ==. val status)
                &&. ( not_ (Esq.isNothing (mbRide ?. R.RideTId))
                        ||. (Esq.isNothing (mbRide ?. R.RideTId) &&. not_ (Esq.isNothing (booking ^. RB.BookingOtpCode)))
                    )
            )
    limit $ fromIntegral $ fromMaybe 10 mbLimit
    offset $ fromIntegral $ fromMaybe 0 mbOffset
    orderBy [desc $ booking ^. RB.BookingCreatedAt]
    pure booking
  catMaybes <$> mapM buildFullBooking fullBookingsT

findAllByRiderId :: Transactionable m => Id Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> m [Booking]
findAllByRiderId personId mbLimit mbOffset mbOnlyActive = Esq.buildDType $ do
  let isOnlyActive = Just True == mbOnlyActive
  fullBookingsT <- Esq.findAll' $ do
    booking <- from $ table @BookingT
    where_ $
      booking ^. RB.BookingRiderId ==. val (toKey personId)
        &&. whenTrue_ isOnlyActive (not_ (booking ^. RB.BookingStatus `in_` valList [DRB.COMPLETED, DRB.CANCELLED]))
    limit $ fromIntegral $ fromMaybe 10 mbLimit
    offset $ fromIntegral $ fromMaybe 0 mbOffset
    orderBy [desc $ booking ^. RB.BookingCreatedAt]
    pure booking
  catMaybes <$> mapM buildFullBooking fullBookingsT

updatePaymentInfo :: Id Booking -> Money -> Maybe Money -> Money -> Maybe Text -> SqlDB ()
updatePaymentInfo rbId estimatedFare discount estimatedTotalFare mbPaymentUrl = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RB.BookingUpdatedAt =. val now,
        RB.BookingEstimatedFare =. val (realToFrac estimatedFare),
        RB.BookingDiscount =. val (realToFrac <$> discount),
        RB.BookingEstimatedTotalFare =. val (realToFrac estimatedTotalFare),
        RB.BookingPaymentUrl =. val mbPaymentUrl
      ]
    where_ $ tbl ^. RB.BookingId ==. val (getId rbId)

updatePaymentUrl :: Id Booking -> Text -> SqlDB ()
updatePaymentUrl bookingId paymentUrl = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RB.BookingPaymentUrl =. val (Just paymentUrl),
        RB.BookingUpdatedAt =. val now
      ]
    where_ $ tbl ^. RB.BookingId ==. val (getId bookingId)

findByIdAndMerchantId :: Transactionable m => Id Booking -> Id Merchant -> m (Maybe Booking)
findByIdAndMerchantId bookingId merchantId = Esq.buildDType $ do
  mbFullBookingT <- Esq.findOne' $ do
    booking <- from $ table @BookingT
    where_ $
      booking ^. RB.BookingId ==. val bookingId.getId
        &&. booking ^. RB.BookingMerchantId ==. val (toKey merchantId)
    pure booking
  join <$> mapM buildFullBooking mbFullBookingT

findAllByPersonIdLimitOffset ::
  Transactionable m =>
  Id Person ->
  Maybe Integer ->
  Maybe Integer ->
  m [Booking]
findAllByPersonIdLimitOffset personId mlimit moffset = Esq.buildDType $ do
  fullBookingsT <- Esq.findAll' $ do
    booking <- from $ table @BookingT
    where_ $
      booking ^. RB.BookingRiderId ==. val (toKey personId)
    limit $ fromIntegral $ fromMaybe 100 mlimit
    offset $ fromIntegral $ fromMaybe 0 moffset
    orderBy [desc $ booking ^. RB.BookingCreatedAt]
    pure booking
  catMaybes <$> mapM buildFullBooking fullBookingsT

findStuckBookings :: Transactionable m => Id Merchant -> [Id Booking] -> UTCTime -> m [Id Booking]
findStuckBookings merchantId bookingIds now = do
  Esq.findAll $ do
    booking <- from $ table @BookingT
    let upcoming6HrsCond =
          booking ^. BookingCreatedAt +. Esq.interval [Esq.HOUR 6] <=. val now
    where_ $
      booking ^. BookingMerchantId ==. val (toKey merchantId)
        &&. booking ^. BookingTId `in_` valList (toKey <$> bookingIds)
        &&. (booking ^. BookingStatus `in_` valList [NEW, CONFIRMED, TRIP_ASSIGNED] &&. upcoming6HrsCond)
    pure $ booking ^. BookingTId

cancelBookings :: [Id Booking] -> UTCTime -> SqlDB ()
cancelBookings bookingIds now = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ BookingStatus =. val CANCELLED,
        BookingUpdatedAt =. val now
      ]
    where_ $ tbl ^. BookingTId `in_` valList (toKey <$> bookingIds)
