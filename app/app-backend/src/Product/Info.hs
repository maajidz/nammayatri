module Product.Info where

import Beckn.Types.App
import Beckn.Types.Common (AckResponse (..), generateGUID)
import Beckn.Types.Core.Ack
import Beckn.Types.Mobility.Service
import qualified Beckn.Types.Mobility.Trip as Trip
import qualified Beckn.Types.Storage.CaseProduct as SCP
import qualified Beckn.Types.Storage.Products as SProducts
import Beckn.Utils.Common (withFlowHandler, decodeFromText)
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as DTE
import Epass.Utils.Extra
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified External.Gateway.Flow as External
import qualified Storage.Queries.Case as QCase
import qualified Storage.Queries.CaseProduct as QCP
import qualified Storage.Queries.Products as QProducts
import Servant
import Types.App
import Types.API.Location
import Types.API.Product
import qualified Types.Storage.Tracker as Tracker
import Utils.Common (verifyToken)
import Utils.Routes

getProductInfo :: Maybe RegToken -> Text -> FlowHandler GetProductInfoRes
getProductInfo regToken prodId = withFlowHandler $ do
  reg <- verifyToken regToken
  caseProduct <- QCP.findByProductId (ProductsId prodId)
  case' <- QCase.findById (SCP._caseId caseProduct)
  product <- QProducts.findById (ProductsId prodId)
  case SProducts._info product of
    Just info -> do
      case decodeFromText info of
        Nothing -> L.throwException $ err500 {errBody = "NO_TRACKING_INFORMATION_FOUND"}
        Just (tracker :: Tracker.Tracker) -> do
          let trip = Tracker.trip tracker
          return $ GetProductInfoRes
            { vehicle = Trip.vehicle trip,
              driver = Trip.driver trip,
              travellers = Trip.travellers trip,
              fare = Trip.fare trip,
              caseId = _getCaseId (SCP._caseId caseProduct),
              productId = prodId
            }
    Nothing ->
      L.logInfo "get Product info" "No info found in products table" >>
        L.throwException (err400 {errBody = "NO_DETAILS_FOUND"})

getLocation :: Maybe RegToken -> Text -> FlowHandler GetLocationRes
getLocation regToken caseId = withFlowHandler $ do
  verifyToken regToken
  baseUrl <- External.getBaseUrl
  caseProducts <- QCP.listAllCaseProduct (QCP.ByApplicationId $ CaseId caseId) ([SCP.CONFIRMED])
  when (null caseProducts) $ L.throwException $ err400 {errBody = "INVALID_CASE"}
  product <- QProducts.findById (SCP._productId $ head caseProducts)
  case decodeFromText =<< SProducts._info product of
    Nothing -> L.throwException $ err500 {errBody = "NO_TRACKING_INFORMATION_FOUND"}
    Just (tracker :: Tracker.Tracker) -> do
      resp <- External.location baseUrl (Trip.id $ Tracker.trip tracker)
      case resp of
        Left err -> L.throwException $ err500 {errBody = encode err}
        Right r -> return r
