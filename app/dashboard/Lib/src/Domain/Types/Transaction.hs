module Domain.Types.Transaction where

import Beckn.Prelude
import Beckn.Types.Id
import qualified "dashboard-bpp-helper-api" Dashboard.Common.Driver as Common
import qualified "dashboard-bpp-helper-api" Dashboard.Common.Driver.Registration as Common
import qualified "dashboard-bpp-helper-api" Dashboard.Common.Ride as Common
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP

-- request is raw Text here, because if some field will be changed, we can't parse it
data Transaction = Transaction
  { id :: Id Transaction,
    personId :: Id DP.Person,
    merchantId :: Id DM.Merchant,
    commonDriverId :: Maybe (Id Common.Driver),
    commonRideId :: Maybe (Id Common.Ride),
    endpoint :: Endpoint, -- Text?
    request :: Text,
    response :: Maybe Text,
    errorCode :: Maybe Text,
    createdAt :: UTCTime
  }

data Endpoint
  = RideEndpoint Common.RideEndpoint
  | DriverEndpoint Common.DriverEndpoint
  | DriverRegistrationEndpoint Common.DriverRegistrationEndpoint
  deriving (Show, Read)
