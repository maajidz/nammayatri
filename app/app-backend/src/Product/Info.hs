{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Info where

import App.Types
import Beckn.Types.App
import Beckn.Types.Common
import qualified Beckn.Types.Mobility.Trip as Trip
import qualified Beckn.Types.Storage.Person as Person
import qualified Beckn.Types.Storage.ProductInstance as SPI
import Beckn.Utils.Common
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.Flow as External
import qualified Models.ProductInstance as MPI
import Types.API.Location
import Types.API.Product
import Types.ProductInfo as ProductInfo

getProductInfo :: Person.Person -> Text -> FlowHandler GetProductInfoRes
getProductInfo _person prodInstId = withFlowHandler $ do
  productInstance <- MPI.findById (ProductInstanceId prodInstId)
  case decodeFromText =<< SPI._info productInstance of
    Just info ->
      case ProductInfo._tracker info of
        Nothing -> throwError500 "NO_TRACKING_INFORMATION_FOUND"
        Just tracker -> do
          let trip = ProductInfo._trip tracker
          return $
            GetProductInfoRes
              { vehicle = trip ^. #vehicle,
                driver = trip ^. #driver,
                travellers = trip ^. #travellers,
                fare = trip ^. #fare,
                caseId = _getCaseId (SPI._caseId productInstance),
                product = productInstance
              }
    Nothing ->
      L.logInfo @Text "get Product info" "No info found in products table"
        >> throwError400 "NO_DETAILS_FOUND"

getLocation :: Person.Person -> Text -> FlowHandler GetLocationRes
getLocation person caseId = withFlowHandler $ do
  baseUrl <- xProviderUri <$> ask
  productInstances <- MPI.listAllProductInstanceByPerson person (ByApplicationId $ CaseId caseId) [SPI.CONFIRMED]
  when (null productInstances) $ throwError400 "INVALID_CASE"
  -- TODO: what if there are multiple CONFIRMED products possible?
  case decodeFromText =<< SPI._info (head productInstances) of
    Nothing -> throwError500 "NO_TRACKING_INFORMATION_FOUND"
    Just info -> do
      let mtracker = ProductInfo._tracker info
      case mtracker of
        Nothing -> throwError500 "NO_TRACKING_INFORMATION_FOUND"
        Just tracker -> do
          resp <- External.location baseUrl (Trip.id $ toBeckn $ ProductInfo._trip tracker)
          case resp of
            Left err -> throwError500 $ encodeToText err
            Right r -> return r
