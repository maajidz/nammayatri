{-# LANGUAGE TypeApplications #-}

module Mobility.Transporter.APICalls where

import qualified "beckn-transport" API.UI.Booking as TbeBookingAPI
import qualified "beckn-transport" API.UI.Driver as TbeDriverAPI
import qualified "beckn-transport" API.UI.Location as TbeLocation
import qualified "beckn-transport" API.UI.Ride as TbeRideAPI
import Data.Time
import qualified "beckn-transport" Domain.Types.Booking as TRB
import qualified "beckn-transport" Domain.Types.Ride as TRide
import EulerHS.Prelude
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Types.APISuccess
import Kernel.Types.App
import Kernel.Types.Id
import Servant hiding (Context)
import Servant.Client

rideStart :: Text -> Id TRide.Ride -> TbeRideAPI.StartRideReq -> ClientM APISuccess
rideEnd :: Text -> Id TRide.Ride -> TbeRideAPI.EndRideReq -> ClientM APISuccess
rideCancel :: Text -> Id TRide.Ride -> TbeRideAPI.CancelRideReq -> ClientM APISuccess
_ :<|> _ :<|> rideStart :<|> rideEnd :<|> rideCancel = client (Proxy :: Proxy TbeRideAPI.API)

getDriverInfo :: Text -> ClientM TbeDriverAPI.DriverInformationRes
setDriverOnline :: Text -> Bool -> ClientM APISuccess
( _
    :<|> _
    :<|> _
    :<|> _
  )
  :<|> ( setDriverOnline
           :<|> _
           :<|> ( getDriverInfo
                    :<|> _
                  )
         ) = client (Proxy :: Proxy TbeDriverAPI.API)

rideRespond :: Id TRB.Booking -> Text -> TbeBookingAPI.SetDriverAcceptanceReq -> ClientM TbeBookingAPI.SetDriverAcceptanceRes
rideRespond bookingId = rideResp
  where
    _ :<|> driver_rb_path = client (Proxy :: Proxy TbeBookingAPI.API)
    rideResp :<|> _ = driver_rb_path bookingId

getNotificationInfo :: Id TRB.Booking -> Text -> ClientM TbeBookingAPI.GetRideInfoRes
getNotificationInfo bookingId = getNotif
  where
    _ :<|> driver_rb_path = client (Proxy :: Proxy TbeBookingAPI.API)
    _ :<|> getNotif = driver_rb_path bookingId

tBookingStatus :: Id TRB.Booking -> Text -> ClientM TRB.BookingAPIEntity
tBookingList :: Text -> Maybe Integer -> Maybe Integer -> Maybe Bool -> Maybe TRB.BookingStatus -> ClientM TbeBookingAPI.BookingListRes
(tBookingStatus :<|> tBookingList :<|> _) :<|> _ = client (Proxy :: Proxy TbeBookingAPI.API)

buildDriverStartRideReq :: Text -> LatLong -> TbeRideAPI.StartRideReq
buildDriverStartRideReq otp pt =
  TbeRideAPI.StartRideReq
    { TbeRideAPI.rideOtp = otp,
      TbeRideAPI.point = pt
    }

updateLocation :: RegToken -> NonEmpty TbeLocation.Waypoint -> ClientM APISuccess
(_ :<|> updateLocation) = client (Proxy @TbeLocation.API)

buildUpdateLocationRequest :: NonEmpty LatLong -> IO (NonEmpty TbeLocation.Waypoint)
buildUpdateLocationRequest pts =
  forM pts $ \ll -> do
    now <- getCurrentTime
    return $
      TbeLocation.Waypoint
        { pt = ll,
          ts = now,
          acc = Nothing
        }

getTransporterBaseUrl :: BaseUrl
getTransporterBaseUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "localhost",
      baseUrlPort = 8014,
      baseUrlPath = "/v2"
    }
