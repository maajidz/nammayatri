{-# LANGUAGE OverloadedLabels #-}

module Product.Products where

import Beckn.Types.App
import Beckn.Types.Common as BC
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.CaseProduct as CaseP
import Beckn.Types.Storage.Location as Location
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.Products as Product
import qualified Beckn.Types.Storage.Products as Storage
import qualified Beckn.Types.Storage.RegistrationToken as SR
import qualified Beckn.Types.Storage.Vehicle as V
import Beckn.Utils.Common (encodeToText, withFlowHandler)
import Beckn.Utils.Extra (headMaybe)
import qualified Data.Accessor as Lens
import Data.Aeson
import qualified Data.Text as T
import Data.Time.LocalTime
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Product.BecknProvider.BP as BP
import Servant
import qualified Storage.Queries.Case as CQ
import qualified Storage.Queries.CaseProduct as CPQ
import Storage.Queries.Location as LQ
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Products as DB
import qualified Storage.Queries.RegistrationToken as QR
import qualified Storage.Queries.Vehicle as VQ
import System.Environment
import Types.API.Case
import Types.API.CaseProduct
import Types.API.Products
import Types.App

update :: Maybe Text -> Text -> ProdReq -> FlowHandler ProdInfoRes
update regToken productId ProdReq {..} = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.verifyAuth regToken
  user <- QP.findPersonById (PersonId _EntityId)
  vehIdRes <- case _vehicleId of
    Just k ->
      whenM (return $ (user ^. #_role) == SP.ADMIN || (user ^. #_role) == SP.DRIVER) $
        DB.updateVeh (ProductsId productId) _vehicleId
    Nothing -> return ()
  dvrIdRes <- case _assignedTo of
    Just k ->
      whenM (return $ (user ^. #_role) == SP.ADMIN) $
        DB.updateDvr (ProductsId productId) _assignedTo
    Nothing -> return ()
  tripRes <- case _status of
    Just c ->
      whenM (return $ (user ^. #_role) == SP.ADMIN || (user ^. #_role) == SP.DRIVER) $
        updateTrip (ProductsId productId) c
    Nothing -> return ()
  updatedProd <- DB.findById (ProductsId productId)
  driverInfo <- case (updatedProd ^. #_assignedTo) of
    Just driverId -> QP.findPersonById (PersonId driverId)
    Nothing -> L.throwException $ err400 {errBody = "DRIVER_ID MISSING"}
  vehicleInfo <- case (updatedProd ^. #_udf3) of
    Just vehicleId -> VQ.findVehicleById (VehicleId vehicleId)
    Nothing -> return Nothing
  infoObj <- updateInfo (ProductsId productId) (Just driverInfo) vehicleInfo
  notifyTripDataToGateway (ProductsId productId)
  notifyCancelReq productId _status
  return $ updatedProd {Storage._info = infoObj}

notifyTripDataToGateway :: ProductsId -> L.Flow ()
notifyTripDataToGateway productId = do
  cps <- CPQ.findAllByProdId productId
  cases <- CQ.findAllByIds (CaseP._caseId <$> cps)
  let trackerCase = headMaybe $ filter (\x -> x ^. #_type == Case.TRACKER) cases
  let parentCase = headMaybe $ filter (\x -> x ^. #_type == Case.RIDEBOOK) cases
  case (trackerCase, parentCase) of
    (Just x, Just y) -> BP.notifyTripUrlToGateway x y
    _ -> return ()

updateInfo :: ProductsId -> Maybe SP.Person -> Maybe V.Vehicle -> L.Flow (Maybe Text)
updateInfo productId driverInfo vehicleInfo = do
  let info = Just $ encodeToText (mkInfoObj driverInfo vehicleInfo)
  DB.updateInfo productId info
  return info
  where
    mkInfoObj drivInfo vehiInfo =
      Storage.ProdInfo
        { driverInfo = encodeToText drivInfo,
          vehicleInfo = encodeToText vehiInfo
        }

updateTrip :: ProductsId -> Product.ProductsStatus -> L.Flow ()
updateTrip productId k = do
  cpList <- CPQ.findAllByProdId productId
  trackerCase_ <- CQ.findByIdType (CaseP._caseId <$> cpList) (Case.TRACKER)
  parentCase_ <- CQ.findByIdType (CaseP._caseId <$> cpList) (Case.RIDEBOOK)
  case k of
    Product.CANCELLED -> do
      DB.updateStatus productId k
      CPQ.updateStatusByIds (CaseP._id <$> cpList) k
      CQ.updateStatus (Case._id trackerCase_) Case.CLOSED
      return ()
    Product.INPROGRESS -> do
      DB.updateStatus productId k
      CPQ.updateStatusByIds (CaseP._id <$> cpList)  k
      CQ.updateStatus (Case._id trackerCase_) Case.INPROGRESS
      return ()
    Product.COMPLETED -> do
      DB.updateStatus productId k
      CPQ.updateStatusByIds (CaseP._id <$> cpList) k
      CQ.updateStatus (Case._id trackerCase_) Case.COMPLETED
      CQ.updateStatus (Case._id parentCase_) Case.COMPLETED
      return ()
    _ -> return ()

listRides :: Maybe Text -> FlowHandler ProdListRes
listRides regToken = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.verifyAuth regToken
  person <- QP.findPersonById (PersonId _EntityId)
  rideList <- DB.findAllByAssignedTo $ _getPersonId (SP._id person)
  locList <- LQ.findAllByLocIds (catMaybes (Storage._fromLocation <$> rideList)) (catMaybes (Storage._toLocation <$> rideList))
  return $ catMaybes $ joinByIds locList <$> rideList
  where
    joinByIds locList ride =
      case find (\x -> (Storage._fromLocation ride == Just (_getLocationId (Location._id x)))) locList of
        Just k -> buildResponse k
        Nothing -> Nothing
      where
        buildResponse k = (prepare ride k) <$> find (\x -> (Storage._toLocation ride == Just (_getLocationId (Location._id x)))) locList
        prepare ride from to =
          ProdRes
            { _product = ride,
              _fromLocation = from,
              _toLocation = to
            }

listCasesByProd :: Maybe Text -> Text -> Maybe Case.CaseType -> FlowHandler CaseListRes
listCasesByProd regToken productId csType = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.verifyAuth regToken
  cpList <- CPQ.findAllByProdId (ProductsId productId)
  caseList <- case csType of
    Just type_ -> CQ.findAllByIdType (CaseP._caseId <$> cpList) type_
    Nothing -> CQ.findAllByIds (CaseP._caseId <$> cpList)
  locList <- LQ.findAllByLocIds (Case._fromLocationId <$> caseList) (Case._toLocationId <$> caseList)
  return $ catMaybes $ joinByIds locList <$> caseList
  where
    joinByIds locList cs =
      case find (\x -> (Case._fromLocationId cs == _getLocationId (Location._id x))) locList of
        Just k -> buildResponse k
        Nothing -> Nothing
      where
        buildResponse k = (prepare cs k) <$> find (\x -> (Case._toLocationId cs == _getLocationId (Location._id x))) locList
        prepare cs from to =
          CaseRes
            { _case = cs,
              _fromLocation = from,
              _toLocation = to
            }

notifyCancelReq :: Text -> Maybe Product.ProductsStatus -> L.Flow ()
notifyCancelReq prodId status = do
  case status of
    Just k -> case k of
      Product.CANCELLED -> BP.notifyCancelToGateway prodId
      _ -> return ()
    Nothing -> return ()
