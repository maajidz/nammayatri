module API where

import qualified API.Beckn as Beckn
import qualified API.Dashboard as Dashboard
import qualified API.UI as UI
import Data.OpenApi
-- import Domain.Action.UI.DriverOnboarding.DriverLicense as DriverOnboarding
-- import Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DriverOnboarding
import Domain.Action.UI.DriverOnboarding.IdfyWebhook as DriverOnboarding
import Environment
import EulerHS.Prelude
import qualified Idfy.Flow as Idfy
import Servant
import Servant.OpenApi

type DriverOfferAPI =
  MainAPI
    :<|> SwaggerAPI

type MainAPI =
  UI.API
    :<|> Beckn.API
    :<|> Idfy.IdfyWebhookAckAPI
    -- :<|> Idfy.IdfyWebhookAPI
    :<|> Dashboard.API

driverOfferAPI :: Proxy DriverOfferAPI
driverOfferAPI = Proxy

mainServer :: FlowServer MainAPI
mainServer =
  UI.handler
    :<|> Beckn.handler
    :<|> Idfy.idfyWebhookHandlerAck DriverOnboarding.onVerify
    -- :<|> Idfy.idfyWebhookHandler DriverOnboarding.onVerifyDL DriverOnboarding.onVerifyRC
    :<|> Dashboard.handler

driverOfferServer :: FlowServer DriverOfferAPI
driverOfferServer =
  mainServer
    :<|> writeSwaggerJSONFlow

type SwaggerAPI = "swagger" :> Get '[JSON] OpenApi

swagger :: OpenApi
swagger = do
  let openApi = toOpenApi (Proxy :: Proxy MainAPI)
  openApi
    { _openApiInfo =
        (_openApiInfo openApi)
          { _infoTitle = "Namma Yatri Partner",
            _infoVersion = "1.0"
          }
    }

writeSwaggerJSONFlow :: FlowServer SwaggerAPI
writeSwaggerJSONFlow = return swagger
