{-# LANGUAGE OverloadedLabels #-}

module Product.TrackTrip where

import Beckn.Types.API.Track
import Beckn.Types.App
import Beckn.Types.Common (AckResponse (..), generateGUID)
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Person as Person
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import qualified Beckn.Types.Storage.Products as Products
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common (decodeFromText, encodeToText, withFlowHandler)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.Flow as Gateway
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.ProductInstance as ProductInstance
import qualified Storage.Queries.Products as Products
import Types.ProductInfo as ProductInfo
import qualified Utils.Notifications as Notify

track :: SR.RegistrationToken -> TrackTripReq -> FlowHandler TrackTripRes
track SR.RegistrationToken {..} req = withFlowHandler $ do
  let context = req ^. #context
      tripId = req ^. #message ^. #id
  prd <- Products.findById $ ProductsId tripId
  ack <-
    case decodeFromText =<< (prd ^. #_info) of
      Nothing -> return $ Ack "Error" "No product to track"
      Just (info :: ProductInfo) -> do
        case ProductInfo._tracker info of
          Nothing -> return $ Ack "Error" "No product to track"
          Just tracker -> do
            let gTripId = tracker ^. #trip ^. #id
            gatewayUrl <- Gateway.getBaseUrl
            eres <- Gateway.track gatewayUrl $ req & (#message . #id) .~ gTripId
            case eres of
              Left err -> return $ Ack "Error" (show err)
              Right _ -> return $ Ack "Successful" "Tracking initiated"
  return $ AckResponse context ack

track_cb :: OnTrackTripReq -> FlowHandler OnTrackTripRes
track_cb req = withFlowHandler $ do
  -- TODO: verify api key
  let context = req ^. #context
      tracking = req ^. #message
      caseId = CaseId $ req ^. #context ^. #transaction_id
  case_ <- Case.findById caseId
  cp <- ProductInstance.listAllProductInstance (ProductInstance.ByApplicationId caseId) [ProductInstance.CONFIRMED]
  let pids = map ProductInstance._productId cp
  confirmedProducts <- Products.findAllByIds pids

  res <-
    case length confirmedProducts of
      0 -> return $ Right ()
      1 -> do
        let product = head confirmedProducts
            personId = Case._requestor case_
        Notify.notifyOnTrackCb personId tracking case_
        updateTracker product tracking
      _ -> return $ Left "Multiple products confirmed, ambiguous selection"
  case res of
    Left err -> return $ AckResponse context (Ack "Error" err)
    Right _ -> return $ AckResponse context (Ack "Successful" "Ok")

updateTracker :: Products.Products -> Tracker -> L.Flow (Either Text ())
updateTracker product tracker = do
  let minfo = decodeFromText =<< product ^. #_info
  let uInfo = (\info -> info {ProductInfo._tracker = Just tracker}) <$> minfo
  let updatedPrd =
        product {Products._info = Just $ encodeToText uInfo}
  Products.updateMultiple (_getProductsId $ product ^. #_id) updatedPrd
  return $ Right ()
