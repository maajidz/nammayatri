module Product.Info where

import App.Types
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Case as SC
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as SPI
import EulerHS.Prelude
import qualified ExternalAPI.Flow as ExternalAPI
import qualified Models.ProductInstance as MPI
import Types.API.Location
import Types.API.Product
import Types.Error
import Types.ProductInfo as ProductInfo
import Utils.Common

getProductInfo :: Person.Person -> Id SPI.ProductInstance -> FlowHandler GetProductInfoRes
getProductInfo _ prodInstId = withFlowHandlerAPI $ do
  productInstance <- MPI.findById prodInstId
  case decodeFromText =<< SPI.info productInstance of
    Just info ->
      case ProductInfo.tracker info of
        Nothing -> throwError $ PIFieldNotPresent "tracker"
        Just tracker -> do
          let trip = ProductInfo.trip tracker
          return $
            GetProductInfoRes
              { vehicle = trip.vehicle,
                driver = trip.driver,
                travellers = trip.travellers,
                fare = trip.fare,
                caseId = getId (SPI.caseId productInstance),
                product = productInstance
              }
    Nothing ->
      logTagInfo "get Product info" "No info found in products table"
        >> throwError (PIFieldNotPresent "info")

-- TODO: fetch tracking URL from tracker info
getLocation :: Person.Person -> Id SC.Case -> FlowHandler GetLocationRes
getLocation person caseId = withFlowHandlerAPI $ do
  baseUrl <- xProviderUri <$> ask
  productInstances <- MPI.listAllProductInstanceByPerson person (SPI.ByApplicationId caseId) [SPI.CONFIRMED]
  when (null productInstances) $ throwError PIDoesNotExist
  let pI = head productInstances
  ExternalAPI.location baseUrl (getId $ pI.id)
