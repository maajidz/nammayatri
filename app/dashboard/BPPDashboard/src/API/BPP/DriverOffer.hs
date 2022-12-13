module API.BPP.DriverOffer
  ( API,
    handler,
  )
where

import qualified API.BPP.DriverOffer.Driver as Driver
import qualified API.BPP.DriverOffer.Ride as Ride
import Beckn.Types.Id
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import "lib-dashboard" Environment
import Servant

type API =
  "driver-offer"
    :> Capture "merchantId" (ShortId DM.Merchant)
    :> ( Driver.API
           :<|> Ride.API
       )

handler :: FlowServer API
handler merchantId =
  Driver.handler merchantId
    :<|> Ride.handler merchantId
