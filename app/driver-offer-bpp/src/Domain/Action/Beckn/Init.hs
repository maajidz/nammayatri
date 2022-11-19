module Domain.Action.Beckn.Init where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Hedis
import Beckn.Types.Common
import Beckn.Types.Error
import Beckn.Types.Id
import Beckn.Utils.Error.Throwing
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Booking.BookingLocation as DLoc
import qualified Domain.Types.DriverQuote as DQuote
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.DriverQuote as QDQuote
import qualified Storage.Queries.SearchRequest as QSR

data InitReq = InitReq
  { driverQuoteId :: Id DQuote.DriverQuote,
    bapId :: Text,
    bapUri :: BaseUrl
  }

data InitRes = InitRes
  { booking :: DRB.Booking,
    transporter :: DM.Merchant
  }

buildBookingLocation :: (MonadGuid m) => DLoc.SearchReqLocation -> m DLoc.BookingLocation
buildBookingLocation DLoc.SearchReqLocation {..} = do
  let address = DLoc.LocationAddress {..}
  guid <- generateGUIDText
  pure
    DLoc.BookingLocation
      { id = Id guid,
        ..
      }

handler :: (HasCacheConfig r, HedisFlow m r, EsqDBFlow m r) => Id DM.Merchant -> InitReq -> m InitRes
handler merchantId req = do
  transporter <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  now <- getCurrentTime
  driverQuote <- QDQuote.findById req.driverQuoteId >>= fromMaybeM (QuoteNotFound req.driverQuoteId.getId)
  when (driverQuote.validTill < now) $
    throwError $ QuoteExpired driverQuote.id.getId
  searchRequest <- QSR.findById driverQuote.searchRequestId >>= fromMaybeM (SearchRequestNotFound driverQuote.searchRequestId.getId)
  -- do we need to check searchRequest.validTill?
  booking <- buildBooking searchRequest driverQuote now
  Esq.runTransaction $
    QRB.create booking
  pure InitRes {..}
  where
    buildBooking searchRequest driverQuote now = do
      id <- Id <$> generateGUID
      fromLocation <- buildBookingLocation searchRequest.fromLocation
      toLocation <- buildBookingLocation searchRequest.toLocation
      pure
        DRB.Booking
          { quoteId = req.driverQuoteId,
            status = DRB.NEW,
            providerId = merchantId,
            bapId = req.bapId,
            bapUri = req.bapUri,
            startTime = searchRequest.startTime,
            riderId = Nothing,
            vehicleVariant = driverQuote.vehicleVariant,
            estimatedDistance = driverQuote.distance,
            createdAt = now,
            updatedAt = now,
            fromLocation,
            toLocation,
            estimatedFare = driverQuote.estimatedFare,
            riderName = Nothing,
            estimatedDuration = searchRequest.estimatedDuration,
            fareParams = driverQuote.fareParams,
            ..
          }
