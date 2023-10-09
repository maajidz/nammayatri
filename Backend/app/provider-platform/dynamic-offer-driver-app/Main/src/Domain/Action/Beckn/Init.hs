{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Domain.Action.Beckn.Init where

import qualified Domain.Action.Beckn.Search as BS
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Booking.BookingLocation as DLoc
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.DriverQuote as DDQ
import qualified Domain.Types.Exophone as DExophone
import qualified Domain.Types.FareParameters as DFP
import qualified Domain.Types.FareProduct as FareProductD
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM
import qualified Domain.Types.QuoteSpecialZone as DQSZ
import qualified Domain.Types.RideRoute as RI
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import qualified Domain.Types.SearchRequestSpecialZone as DSRSZ
import qualified Domain.Types.SearchTry as DST
import qualified Domain.Types.Vehicle.Variant as Veh
import Kernel.Prelude
import Kernel.Randomizer (getRandomElement)
import Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.SessionizerMetrics.Types.Event
import qualified SharedLogic.CallBAP as BP
import qualified Storage.CachedQueries.Exophone as CQExophone
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.Merchant.MerchantServiceUsageConfig as CMSUC
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.DriverQuote as QDQuote
import qualified Storage.Queries.QuoteSpecialZone as QSZoneQuote
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchRequestSpecialZone as QSRSpecialZone
import qualified Storage.Queries.SearchTry as QST
import Tools.Error
import Tools.Event

data InitReq = InitReq
  { estimateId :: Text,
    driverId :: Maybe Text,
    vehicleVariant :: Veh.Variant,
    bapId :: Text,
    bapUri :: BaseUrl,
    bapCity :: Context.City,
    bapCountry :: Context.Country,
    initTypeReq :: InitTypeReq,
    maxEstimatedDistance :: Maybe HighPrecMeters,
    paymentMethodInfo :: Maybe DMPM.PaymentMethodInfo
  }

data InitTypeReq = InitSpecialZoneReq | InitNormalReq | InitRentalReq

data InitRes = InitRes
  { booking :: DRB.Booking,
    transporter :: DM.Merchant,
    paymentMethodInfo :: Maybe DMPM.PaymentMethodInfo,
    driverName :: Maybe Text,
    driverId :: Maybe Text
  }

data ValidatedInitRequest
  = ONE_WAY_INIT (DDQ.DriverQuote, DSR.SearchRequest, DST.SearchTry)
  | SPECIAL_ZONE_INIT (DQSZ.QuoteSpecialZone, DSRSZ.SearchRequestSpecialZone)
  | RENTAL_INIT --TODO:RENTAL -- have to make this

buildBookingLocation :: (MonadGuid m) => DLoc.SearchReqLocation -> m DLoc.BookingLocation
buildBookingLocation DLoc.SearchReqLocation {..} = do
  let address = DLoc.LocationAddress {..}
  guid <- generateGUIDText
  pure
    DLoc.BookingLocation
      { id = Id guid,
        ..
      }

cancelBooking ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c
  ) =>
  DRB.Booking ->
  Id DM.Merchant ->
  m AckResponse
cancelBooking booking transporterId = do
  logTagInfo ("BookingId-" <> getId booking.id) ("Cancellation reason " <> show DBCR.ByApplication)
  let transporterId' = booking.providerId
  unless (transporterId' == transporterId) $ throwError AccessDenied
  bookingCancellationReason <- buildBookingCancellationReason
  transporter <- QM.findById transporterId >>= fromMaybeM (MerchantNotFound transporterId.getId)
  _ <- QBCR.upsert bookingCancellationReason
  _ <- QRB.updateStatus booking.id DRB.CANCELLED
  fork "cancelBooking - Notify BAP" $ do
    BP.sendBookingCancelledUpdateToBAP booking transporter bookingCancellationReason.source
  pure Ack
  where
    buildBookingCancellationReason = do
      return $
        DBCR.BookingCancellationReason
          { driverId = Nothing,
            bookingId = booking.id,
            merchantId = Just booking.providerId,
            rideId = Nothing,
            source = DBCR.ByApplication,
            reasonCode = Nothing,
            additionalInfo = Nothing,
            driverCancellationLocation = Nothing,
            driverDistToPickup = Nothing
          }

--TODO:RENTAL -- code the rental part
handler ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EventStreamFlow m r
  ) =>
  Id DM.Merchant ->
  InitReq ->
  ValidatedInitRequest ->
  m InitRes
handler merchantId req validatedReq = do
  transporter <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  now <- getCurrentTime

  mbPaymentMethod <- forM req.paymentMethodInfo $ \paymentMethodInfo -> do
    allPaymentMethods <-
      CQMPM.findAllByMerchantId merchantId
    let mbPaymentMethod = find (compareMerchantPaymentMethod paymentMethodInfo) allPaymentMethods
    mbPaymentMethod & fromMaybeM (InvalidRequest "Payment method not allowed")
  let paymentUrl = DMPM.getPrepaidPaymentUrl =<< mbPaymentMethod
  (booking, driverName, driverId) <- case req.initTypeReq of
    InitNormalReq -> do
      case validatedReq of
        ONE_WAY_INIT (driverQuote, searchRequest, searchTry) -> do
          booking <- buildBooking searchRequest driverQuote driverQuote.id.getId searchTry.startTime DRB.NormalBooking now (mbPaymentMethod <&> (.id)) paymentUrl searchRequest.disabilityTag
          triggerBookingCreatedEvent BookingEventData {booking = booking, personId = driverQuote.driverId, merchantId = transporter.id}
          QST.updateStatus searchTry.id DST.COMPLETED
          _ <- QRB.create booking
          return (booking, Just driverQuote.driverName, Just driverQuote.driverId.getId)
        SPECIAL_ZONE_INIT _ -> throwError $ InvalidRequest "Can't have specialZoneQuote in normal booking"
        RENTAL_INIT -> throwError $ InvalidRequest "Can't have rentalQuote in normal booking"
    InitSpecialZoneReq -> do
      case validatedReq of
        SPECIAL_ZONE_INIT (specialZoneQuote, searchRequest) -> do
          booking <- buildBooking searchRequest specialZoneQuote specialZoneQuote.id.getId searchRequest.startTime DRB.SpecialZoneBooking now (mbPaymentMethod <&> (.id)) paymentUrl Nothing
          _ <- QRB.create booking
          -- moving route from search request id to booking id
          routeInfo :: Maybe RI.RouteInfo <- Redis.safeGet (BS.searchRequestKey $ getId searchRequest.id)
          case routeInfo of
            Just route -> Redis.setExp (BS.searchRequestKey $ getId booking.id) route 3600
            Nothing -> logDebug "Unable to get the key"

          return (booking, Nothing, Nothing)
        ONE_WAY_INIT _ -> throwError $ InvalidRequest "Can't have driverQuote in specialZone booking"
        RENTAL_INIT -> throwError $ InvalidRequest "Can't have rentalQuote in specialZone booking"
    InitRentalReq -> do
      case validatedReq of
        RENTAL_INIT -> do
          undefined --TODO:RENTAL --code this part of inserting it in booking table
        ONE_WAY_INIT _ -> throwError $ InvalidRequest "Can't have driverQuote in rental booking"
        SPECIAL_ZONE_INIT _ -> throwError $ InvalidRequest "Can't have specialZoneQuote in rental booking"

  let paymentMethodInfo = req.paymentMethodInfo
  pure InitRes {..}
  where
    buildBooking ::
      ( CacheFlow m r,
        EsqDBFlow m r,
        HasField "transactionId" sr Text,
        HasField "fromLocation" sr DLoc.SearchReqLocation,
        HasField "toLocation" sr DLoc.SearchReqLocation,
        HasField "estimatedDuration" sr Seconds,
        HasField "area" sr (Maybe FareProductD.Area),
        HasField "vehicleVariant" q Veh.Variant,
        HasField "distance" q Meters,
        HasField "estimatedFare" q Money,
        HasField "fareParams" q DFP.FareParameters,
        HasField "specialLocationTag" q (Maybe Text)
      ) =>
      sr ->
      q ->
      Text ->
      UTCTime ->
      DRB.BookingType ->
      UTCTime ->
      Maybe (Id DMPM.MerchantPaymentMethod) ->
      Maybe Text ->
      Maybe Text ->
      m DRB.Booking
    buildBooking searchRequest driverQuote quoteId startTime bookingType now mbPaymentMethodId paymentUrl disabilityTag = do
      id <- Id <$> generateGUID
      fromLocation <- buildBookingLocation searchRequest.fromLocation
      toLocation <- buildBookingLocation searchRequest.toLocation
      exophone <- findRandomExophone merchantId
      pure
        DRB.Booking
          { transactionId = searchRequest.transactionId,
            status = DRB.NEW,
            providerId = merchantId,
            primaryExophone = exophone.primaryPhone,
            bapId = req.bapId,
            bapUri = req.bapUri,
            bapCity = Just req.bapCity,
            bapCountry = Just req.bapCountry,
            riderId = Nothing,
            vehicleVariant = driverQuote.vehicleVariant,
            estimatedDistance = driverQuote.distance,
            maxEstimatedDistance = req.maxEstimatedDistance,
            createdAt = now,
            updatedAt = now,
            fromLocation,
            toLocation = Just toLocation,
            estimatedFare = driverQuote.estimatedFare,
            riderName = Nothing,
            estimatedDuration = searchRequest.estimatedDuration,
            fareParams = driverQuote.fareParams,
            specialLocationTag = driverQuote.specialLocationTag,
            specialZoneOtpCode = Nothing,
            disabilityTag = disabilityTag,
            area = searchRequest.area,
            paymentMethodId = mbPaymentMethodId,
            ..
          }

findRandomExophone :: (CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> m DExophone.Exophone
findRandomExophone merchantId = do
  merchantServiceUsageConfig <- CMSUC.findByMerchantId merchantId >>= fromMaybeM (MerchantServiceUsageConfigNotFound merchantId.getId)
  exophones <- CQExophone.findByMerchantServiceAndExophoneType merchantId merchantServiceUsageConfig.getExophone DExophone.CALL_RIDE
  nonEmptyExophones <- case exophones of
    [] -> throwError $ ExophoneNotFound merchantId.getId
    e : es -> pure $ e :| es
  getRandomElement nonEmptyExophones

validateRequest :: (CacheFlow m r, EsqDBFlow m r) => Id DM.Merchant -> InitReq -> m ValidatedInitRequest
validateRequest merchantId req = do
  _ <- QM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  now <- getCurrentTime
  case req.initTypeReq of
    InitNormalReq -> do
      driverId <- req.driverId & fromMaybeM (InvalidRequest "driverId Not Found for Normal Booking")
      driverQuote <- QDQuote.findActiveQuoteByDriverIdAndVehVarAndEstimateId (Id req.estimateId) (Id driverId) req.vehicleVariant now >>= fromMaybeM (QuoteNotFound req.estimateId)
      when (driverQuote.validTill < now || driverQuote.status == DDQ.Inactive) $
        throwError $ QuoteExpired driverQuote.id.getId
      searchRequest <- QSR.findById driverQuote.requestId >>= fromMaybeM (SearchRequestNotFound driverQuote.requestId.getId)
      searchTry <- QST.findById driverQuote.searchTryId >>= fromMaybeM (SearchTryNotFound driverQuote.searchTryId.getId)
      return $ ONE_WAY_INIT (driverQuote, searchRequest, searchTry)
    InitSpecialZoneReq -> do
      specialZoneQuote <- QSZoneQuote.findById (Id req.estimateId) >>= fromMaybeM (QuoteNotFound req.estimateId)
      when (specialZoneQuote.validTill < now) $
        throwError $ QuoteExpired specialZoneQuote.id.getId
      searchRequest <- QSRSpecialZone.findById specialZoneQuote.searchRequestId >>= fromMaybeM (SearchRequestNotFound specialZoneQuote.searchRequestId.getId)
      return $ SPECIAL_ZONE_INIT (specialZoneQuote, searchRequest)
    InitRentalReq -> do
      undefined -- TODO:RENTAL --valid this for Rental

compareMerchantPaymentMethod :: DMPM.PaymentMethodInfo -> DMPM.MerchantPaymentMethod -> Bool
compareMerchantPaymentMethod providerPaymentMethod DMPM.MerchantPaymentMethod {..} =
  paymentType == providerPaymentMethod.paymentType
    && paymentInstrument == providerPaymentMethod.paymentInstrument
    && collectedBy == providerPaymentMethod.collectedBy
