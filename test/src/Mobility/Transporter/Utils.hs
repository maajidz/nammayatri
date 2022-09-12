module Mobility.Transporter.Utils where

import qualified "beckn-transport" API.UI.Booking as BookingAPI
import "beckn-transport" API.UI.Location as LocationAPI
import "app-backend" API.UI.Quote
import qualified "beckn-transport" API.UI.Ride as RideAPI
import qualified "app-backend" API.UI.Search as AppSearch
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Types.MapSearch
import Beckn.Types.Time
import Beckn.Utils.Common
import Common
import qualified "beckn-transport" Domain.Action.UI.Booking as DUB
import qualified "app-backend" Domain.Types.Booking as BRB
import qualified "beckn-transport" Domain.Types.Booking as TRB
import qualified "app-backend" Domain.Types.CancellationReason as AppCR
import qualified "beckn-transport" Domain.Types.CancellationReason as SCR
import "beckn-transport" Domain.Types.Person as TPerson
import qualified "app-backend" Domain.Types.Quote as AppQuote
import qualified "app-backend" Domain.Types.Ride as BRide
import qualified "beckn-transport" Domain.Types.Ride as TRide
import qualified "app-backend" Domain.Types.SearchRequest as AppSearchReq
import HSpec
import qualified Mobility.AppBackend.APICalls as API
import Mobility.AppBackend.Fixtures
import qualified Mobility.Transporter.APICalls as API
import Mobility.Transporter.Fixtures
import qualified "app-backend" Storage.Queries.Booking as BQRB
import qualified "beckn-transport" Storage.Queries.Booking as TQRB
import qualified "beckn-transport" Storage.Queries.DriverInformation as DriverInfo
import "beckn-transport" Storage.Queries.DriverLocation
import qualified "beckn-transport" Storage.Queries.Ride as TQRide
import Utils

getFutureTime :: IO UTCTime
getFutureTime =
  -- Generate a time 2 hours in to the future else booking will fail
  addUTCTime 7200 <$> getCurrentTime

-- db calls
getBAPBooking ::
  Id BRB.Booking ->
  ClientsM BRB.Booking
getBAPBooking bapRBId = do
  mbBRB <- liftIO $ runAppFlow "" $ BQRB.findById bapRBId
  mbBRB `shouldSatisfy` isJust
  let Just bRB = mbBRB
  return bRB

getBPPBooking ::
  Id BRB.Booking ->
  ClientsM TRB.Booking
getBPPBooking bapRBId = do
  bRB <- getBAPBooking bapRBId
  bRB.bppBookingId `shouldSatisfy` isJust
  let Just bppBookingId = bRB.bppBookingId
  mbTRB <- liftIO $ runTransporterFlow "" $ TQRB.findById $ cast bppBookingId
  mbTRB $> () `shouldSatisfy` isJust
  let Just tRB = mbTRB
  return tRB

getBPPRide ::
  Id TRB.Booking ->
  ClientsM TRide.Ride
getBPPRide rideBookingId = do
  mbRide <- liftIO $ runTransporterFlow "" $ TQRide.findActiveByRBId rideBookingId
  mbRide `shouldSatisfy` isJust
  return $ fromJust mbRide

getBPPRideById ::
  Id TRide.Ride ->
  ClientsM TRide.Ride
getBPPRideById rideId = do
  mbRide <- liftIO $ runTransporterFlow "" $ TQRide.findById rideId
  mbRide `shouldSatisfy` isJust
  return $ fromJust mbRide

getBPPDriverLocation ::
  Id TPerson.Person ->
  ClientsM LatLong
getBPPDriverLocation driverId = do
  mbRes <- liftIO $ runTransporterFlow "" $ findById driverId
  mbRes `shouldSatisfy` isJust
  let res = fromJust mbRes
  pure $
    LatLong
      { lat = res.lat,
        lon = res.lon
      }

-- driver setup/reset
setupDriver :: DriverTestData -> LatLong -> ClientsM ()
setupDriver driver initialPoint = do
  void . callBPP $ API.setDriverOnline driver.token True
  -- Moves driver to the pickup point
  preUpdate <- liftIO $ API.buildUpdateLocationRequest $ initialPoint :| []
  void . callBPP $
    API.updateLocation driver.token preUpdate

resetDriver :: DriverTestData -> IO ()
resetDriver driver = runTransporterFlow "" $
  Esq.runTransaction $ do
    DriverInfo.updateActivity (cast driver.driverId) False
    DriverInfo.updateOnRide (cast driver.driverId) False

-- flow primitives
search :: Text -> AppSearch.SearchReq -> ClientsM (Id AppSearchReq.SearchRequest)
search token searchReq_ = callBAP $ searchServices token searchReq_ <&> (.searchId)

getOnSearchTaxiQuotesByTransporterName ::
  Text ->
  Id AppSearchReq.SearchRequest ->
  Text ->
  ClientsM (NonEmpty AppQuote.QuoteAPIEntity)
getOnSearchTaxiQuotesByTransporterName appToken searchId transporterName =
  pollFilteredList
    "get on_search quotes"
    (\p -> p.agencyName == transporterName)
    $ callBAP (getQuotes searchId appToken)
      <&> (.quotes)
      <&> mapMaybe \case
        OnDemandCab p -> Just p
        _ -> Nothing

{-
initWithCheck :: Text -> Id AppQuote.Quote -> ClientsM (Id BRB.Booking)
initWithCheck appToken quoteId = do
  bBookingId <- fmap (.bookingId) $ callBAP $ API.appInitRide appToken $ API.mkAppInitReq quoteId
  void . pollDesc "init result" $ do
    initRB <- getBAPBooking bBookingId
    initRB.bppBookingId `shouldSatisfy` isJust
    return $ Just ()
  pure bBookingId
-}

confirmWithCheck :: Text -> DriverTestData -> Id AppQuote.Quote -> ClientsM (Id BRB.Booking, TRB.Booking, DUB.RideInfo)
confirmWithCheck appToken driver quoteId = do
  bBookingId <- fmap (.bookingId) $ callBAP $ API.appConfirmRide appToken quoteId
  void . pollDesc "confirm result" $ do
    initRB <- getBAPBooking bBookingId
    initRB.bppBookingId `shouldSatisfy` isJust
    return $ Just ()

  void . pollDesc "ride confirmed" $
    callBAP (API.appBookingStatus bBookingId appRegistrationToken)
      <&> (.status)
      >>= (`shouldBe` BRB.CONFIRMED)
      <&> Just

  tBooking <- pollDesc "ride booking id should exist and should be confirmed" $ do
    trb <- getBPPBooking bBookingId
    trb.status `shouldBe` TRB.CONFIRMED
    return $ Just trb

  rideInfo <- getRideInfo driver tBooking.id
  pure (bBookingId, tBooking, rideInfo)

acceptRide :: DriverTestData -> TRB.Booking -> ClientsM TRide.Ride
acceptRide driver tBooking = do
  void . callBPP $
    API.rideRespond tBooking.id driver.token $
      BookingAPI.SetDriverAcceptanceReq BookingAPI.ACCEPT

  pollDesc ("ride with booking id=" <> tBooking.id.getId <> " should exist and should have status=NEW") $ do
    tRide <- getBPPRide tBooking.id
    tRide.status `shouldBe` TRide.NEW
    return $ Just tRide

rejectRide :: Text -> DriverTestData -> TRB.Booking -> Id BRB.Booking -> ClientsM ()
rejectRide appToken driver tBooking bBookingId = do
  void . callBPP $
    API.rideRespond tBooking.id driver.token $
      BookingAPI.SetDriverAcceptanceReq BookingAPI.REJECT
  checkBookingBapStatus appToken bBookingId BRB.CANCELLED

cancelRideByApp :: Text -> Id BRB.Booking -> ClientsM ()
cancelRideByApp appToken bBookingId = do
  void . callBAP $ API.cancelRide bBookingId appRegistrationToken (API.mkAppCancelReq AppCR.OnConfirm)
  checkBookingBapStatus appToken bBookingId BRB.CANCELLED

cancelRideByDriver :: DriverTestData -> TRide.Ride -> ClientsM ()
cancelRideByDriver driver tRide = do
  void . callBPP $
    API.rideCancel driver.token tRide.id $
      RideAPI.CancelRideReq (SCR.CancellationReasonCode "OTHER") Nothing

  void $
    pollDesc ("cancelled ride by driver id=" <> driver.driverId.getId) $ do
      tRide' <- getBPPRideById tRide.id
      tRide'.status `shouldBe` TRide.CANCELLED
      pure $ Just tRide'

checkBookingBapStatus :: Text -> Id BRB.Booking -> BRB.BookingStatus -> ClientsM ()
checkBookingBapStatus appToken bBookingId status =
  void . pollDesc ("bap booking with id=" <> bBookingId.getId <> " should exist and should have status=" <> show status) $
    callBAP (API.appBookingStatus bBookingId appToken)
      <&> (.status)
      >>= (`shouldBe` status)
      <&> Just

getRideInfo :: DriverTestData -> Id TRB.Booking -> ClientsM DUB.RideInfo
getRideInfo driver bppBookingId = do
  rideInfo2 <-
    pollDesc ("poll for rideInfo for driver with token:" <> show driver.token) . callBPP $
      API.getNotificationInfo bppBookingId driver.token
        <&> (.rideRequest)
  rideInfo2.bookingId `shouldBe` bppBookingId
  pure rideInfo2

startRide :: Text -> DriverTestData -> LatLong -> TRide.Ride -> Id BRB.Booking -> ClientsM ()
startRide appToken driver origin tRide bBookingId = do
  void . callBPP $
    API.rideStart driver.token tRide.id $
      API.buildStartRideReq tRide.otp origin

  void . pollDesc "trip started" $ do
    inprogressRBStatusResult <- callBAP (API.appBookingStatus bBookingId appToken)
    inprogressRBStatusResult.rideList `shouldSatisfy` not . null
    inprogressRBStatusResult.status `shouldBe` BRB.TRIP_ASSIGNED
    let [inprogressRide] = inprogressRBStatusResult.rideList
    inprogressRide.status `shouldBe` BRide.INPROGRESS
    return $ Just ()

updateLocation ::
  DriverTestData ->
  NonEmpty LocationAPI.Waypoint ->
  ClientsM ()
updateLocation driver updatesList = do
  void . callBPP $
    API.updateLocation driver.token updatesList

endRide ::
  Text ->
  DriverTestData ->
  LatLong ->
  TRide.Ride ->
  Id BRB.Booking ->
  ClientsM (Id BRide.Ride)
endRide appToken driver destination tRide bBookingId = do
  void . callBPP $ API.rideEnd driver.token tRide.id $ RideAPI.EndRideReq destination
  pollDesc "ride completed" $ do
    completedRBStatusResult <- callBAP (API.appBookingStatus bBookingId appToken)
    completedRBStatusResult.rideList `shouldSatisfy` not . null
    completedRBStatusResult.status `shouldBe` BRB.COMPLETED
    let [completedRide] = completedRBStatusResult.rideList
    completedRide.status `shouldBe` BRide.COMPLETED
    return $ Just completedRide.id

data SearchConfirmResult = SearchConfirmResult
  { bapBookingId :: Id BRB.Booking,
    bppBooking :: TRB.Booking,
    rideInfo :: DUB.RideInfo
  }

search'Confirm :: Text -> DriverTestData -> AppSearch.SearchReq -> ClientsM SearchConfirmResult
search'Confirm appToken driver searchReq' = do
  appSearchId <- search appToken searchReq'
  (bapQuoteAPIEntity :| _) <- getOnSearchTaxiQuotesByTransporterName appToken appSearchId bapTransporterName
  let quoteId = bapQuoteAPIEntity.id

  (bapBookingId, bppBooking, rideInfo) <- confirmWithCheck appToken driver quoteId
  pure $
    SearchConfirmResult
      { bapBookingId,
        bppBooking,
        rideInfo
      }
