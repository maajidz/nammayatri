{-# LANGUAGE OverloadedLabels #-}

module Product.Info where

import App.Types
import Beckn.Types.Error
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as SPI
import Beckn.Utils.Common
import Beckn.Utils.Logging (Log (..))
import EulerHS.Prelude
import qualified External.Gateway.Flow as External
import qualified Models.ProductInstance as MPI
import Types.API.Location
import Types.API.Product
import Types.ProductInfo as ProductInfo

getProductInfo :: Person.Person -> Text -> FlowHandler GetProductInfoRes
getProductInfo _person prodInstId = withFlowHandler $ do
  productInstance <- MPI.findById (Id prodInstId)
  case decodeFromText =<< SPI._info productInstance of
    Just info ->
      case ProductInfo._tracker info of
        Nothing -> throwError500 ProductInfoNotFound
        Just tracker -> do
          let trip = ProductInfo._trip tracker
          return $
            GetProductInfoRes
              { vehicle = trip ^. #vehicle,
                driver = trip ^. #driver,
                travellers = trip ^. #travellers,
                fare = trip ^. #fare,
                caseId = getId (SPI._caseId productInstance),
                product = productInstance
              }
    Nothing ->
      logInfo "get Product info" "No info found in products table"
        >> throwErrorWithInfo400 ProductInstanceInvalidState "_info is null."

-- TODO: fetch tracking URL from tracker info
getLocation :: Person.Person -> Text -> FlowHandler GetLocationRes
getLocation person caseId = withFlowHandler $ do
  baseUrl <- xProviderUri <$> ask
  productInstances <- MPI.listAllProductInstanceByPerson person (SPI.ByApplicationId $ Id caseId) [SPI.CONFIRMED]
  when (null productInstances) $ throwError400 ProductInstanceNotFound
  let pI = head productInstances
  resp <- External.location baseUrl (getId $ pI ^. #_id)
  case resp of
    Left err -> throwErrorWithInfo500 InvalidRequest err
    Right r -> return r
