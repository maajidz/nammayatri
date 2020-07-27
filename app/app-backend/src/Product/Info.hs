module Product.Info where

import App.Types
import Beckn.Types.App
import qualified Beckn.Types.Mobility.Payload as Trip
import qualified Beckn.Types.Mobility.Trip as Trip
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as SPI
import Beckn.Utils.Common (decodeFromText, withFlowHandler)
import Data.Aeson
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.Flow as External
import qualified Models.Case as MC
import qualified Models.ProductInstance as MPI
import Servant
import Types.API.Location
import Types.API.Product
import Types.ProductInfo as ProductInfo

getProductInfo :: Person.Person -> Text -> FlowHandler GetProductInfoRes
getProductInfo person prodInstId = withFlowHandler $ do
  productInstance <- MPI.findById (ProductInstanceId prodInstId)
  case' <- MC.findIdByPerson person (SPI._caseId productInstance)
  case decodeFromText =<< SPI._info productInstance of
    Just info ->
      case ProductInfo._tracker info of
        Nothing -> L.throwException $ err500 {errBody = "NO_TRACKING_INFORMATION_FOUND"}
        Just tracker -> do
          let trip = ProductInfo._trip tracker
          return $
            GetProductInfoRes
              { vehicle = Trip.vehicle trip,
                driver = Trip.driver trip,
                travellers = Trip._travellers $ Trip.payload trip,
                fare = Trip.fare trip,
                caseId = _getCaseId (SPI._caseId productInstance),
                product = productInstance
              }
    Nothing ->
      L.logInfo "get Product info" "No info found in products table"
        >> L.throwException (err400 {errBody = "NO_DETAILS_FOUND"})

getLocation :: Person.Person -> Text -> FlowHandler GetLocationRes
getLocation person caseId = withFlowHandler $ do
  baseUrl <- External.getProviderBaseUrl
  productInstances <- MPI.listAllProductInstanceByPerson person (ByApplicationId $ CaseId caseId) [SPI.CONFIRMED]
  when (null productInstances) $ L.throwException $ err400 {errBody = "INVALID_CASE"}
  -- TODO: what if there are multiple CONFIRMED products possible?
  case decodeFromText =<< SPI._info (head productInstances) of
    Nothing -> L.throwException $ err500 {errBody = "NO_TRACKING_INFORMATION_FOUND"}
    Just info -> do
      let mtracker = ProductInfo._tracker info
      case mtracker of
        Nothing -> L.throwException $ err500 {errBody = "NO_TRACKING_INFORMATION_FOUND"}
        Just tracker -> do
          resp <- External.location baseUrl (Trip.id $ ProductInfo._trip tracker)
          case resp of
            Left err -> L.throwException $ err500 {errBody = encode err}
            Right r -> return r
