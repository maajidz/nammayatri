module API
  ( API,
    handler,
  )
where

import qualified API.BAP as BAP
import qualified "lib-dashboard" API.Dashboard as Dashboard
import Beckn.Prelude
import Beckn.Utils.Servant.HTML
import qualified Data.ByteString as BS
import Data.OpenApi
import "lib-dashboard" Environment
import Servant
import Servant.OpenApi

type API =
  MainAPI
    :<|> SwaggerAPI
    :<|> OpenAPI
    :<|> Raw

type MainAPI =
  Dashboard.API
    :<|> BAP.API

handler :: FlowServer API
handler =
  mainServer
    :<|> writeSwaggerHTMLFlow
    :<|> writeOpenAPIFlow
    :<|> serveDirectoryWebApp "swagger"

mainServer :: FlowServer MainAPI
mainServer =
  Dashboard.handler
    :<|> BAP.handler

type SwaggerAPI = "swagger" :> Get '[HTML] BS.ByteString

type OpenAPI = "openapi" :> Get '[JSON] OpenApi

openAPI :: OpenApi
openAPI = do
  let openApi = toOpenApi (Proxy :: Proxy MainAPI)
  openApi
    { _openApiInfo =
        (_openApiInfo openApi)
          { _infoTitle = "BAP Dashboard",
            _infoVersion = "1.0"
          }
    }

writeSwaggerHTMLFlow :: FlowServer SwaggerAPI
writeSwaggerHTMLFlow = lift $ BS.readFile "swagger/index.html"

writeOpenAPIFlow :: FlowServer OpenAPI
writeOpenAPIFlow = pure openAPI
