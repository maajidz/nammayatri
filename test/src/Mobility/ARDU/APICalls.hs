module Mobility.ARDU.APICalls where

import qualified "driver-offer-bpp" API.Dashboard as DashboardAPI
import qualified "driver-offer-bpp" API.UI.Driver as DriverAPI
import "driver-offer-bpp" API.UI.Location as LocationAPI
import qualified "driver-offer-bpp" API.UI.Ride as RideAPI
import qualified "dashboard-bpp-helper-api" Dashboard.BPP.Ride as Dashboard
import Data.Time
import qualified "driver-offer-bpp" Domain.Types.Merchant as TDM
import qualified "driver-offer-bpp" Domain.Types.Ride as TRide
import EulerHS.Prelude
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Types.APISuccess
import Kernel.Types.App
import Kernel.Types.Id
import Servant hiding (Context)
import Servant.Client

data UIAPIs = UIAPIs
  { healthCheck :: ClientM Text,
    ride :: RideAPIs,
    driver :: DriverAPIs,
    location :: LocationAPIs
  }

data RideAPIs = RideAPIs
  { rideStart :: Text -> Id TRide.Ride -> RideAPI.StartRideReq -> ClientM APISuccess,
    rideEnd :: Text -> Id TRide.Ride -> RideAPI.EndRideReq -> ClientM APISuccess,
    rideCancel :: Text -> Id TRide.Ride -> RideAPI.CancelRideReq -> ClientM APISuccess
  }

data DriverAPIs = DriverAPIs
  { getDriverInfo :: Text -> ClientM DriverAPI.DriverInformationRes,
    getNearbySearchRequests :: RegToken -> ClientM DriverAPI.GetNearbySearchRequestsRes,
    offerQuote :: RegToken -> DriverAPI.DriverOfferReq -> ClientM APISuccess,
    respondQuote :: RegToken -> DriverAPI.DriverRespondReq -> ClientM APISuccess,
    setDriverOnline :: Text -> Bool -> ClientM APISuccess
  }

newtype LocationAPIs = LocationAPIs
  { updateLocation :: RegToken -> NonEmpty LocationAPI.Waypoint -> ClientM APISuccess
  }

-- most of apis do not used in tests, so let's simplify API type
type HealthCheckAPI = Get '[JSON] Text

type UIAPI =
  "ui"
    :> ( HealthCheckAPI
           :<|> DriverAPI.API
           :<|> LocationAPI.API
           :<|> RideAPI.API
       )

ui :: UIAPIs
ui = do
  let ride = RideAPIs {..}
  let driver = DriverAPIs {..}
  let location = LocationAPIs {..}
  UIAPIs {..}
  where
    healthCheck
      :<|> driverClient
      :<|> locationClient
      :<|> rideClient = client (Proxy :: Proxy UIAPI)

    _ :<|> _ :<|> rideStart :<|> rideEnd :<|> rideCancel = rideClient

    ( _
        :<|> _
        :<|> _
        :<|> _
      )
      :<|> ( setDriverOnline
               :<|> getNearbySearchRequests
               :<|> offerQuote
               :<|> respondQuote
               :<|> ( getDriverInfo
                        :<|> _
                        :<|> _
                      )
             ) = driverClient

    (_ :<|> updateLocation) = locationClient

newtype DashboardAPIs = DashboardAPIs
  { ride :: DashboardRideAPIs
  }

newtype DashboardRideAPIs = DashboardRideAPIs
  { rideSync :: Id Dashboard.Ride -> ClientM Dashboard.RideSyncRes
  }

dashboard :: ShortId TDM.Merchant -> Text -> DashboardAPIs
dashboard merchantId token = do
  let ride = DashboardRideAPIs {..}
  DashboardAPIs {..}
  where
    _ :<|> rideClient :<|> _ = client (Proxy :: Proxy DashboardAPI.API) merchantId token

    _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> rideSync = rideClient

buildStartRideReq :: Text -> LatLong -> RideAPI.StartRideReq
buildStartRideReq otp initialPoint =
  RideAPI.StartRideReq
    { RideAPI.rideOtp = otp,
      point = initialPoint
    }

buildUpdateLocationRequest :: NonEmpty LatLong -> IO (NonEmpty LocationAPI.Waypoint)
buildUpdateLocationRequest pts =
  forM pts $ \ll -> do
    now <- getCurrentTime
    return $
      LocationAPI.Waypoint
        { pt = ll,
          ts = now,
          acc = Nothing
        }

getDriverOfferBppBaseUrl :: BaseUrl
getDriverOfferBppBaseUrl =
  BaseUrl
    { baseUrlScheme = Http,
      baseUrlHost = "localhost",
      baseUrlPort = 8016,
      baseUrlPath = ""
    }
