{-# LANGUAGE OverloadedLabels #-}

module Product.BecknProvider.BP where

import App.Types (FlowHandler)
import Beckn.Product.Validation.Context
  ( validateContextCommons,
    validateDomain,
  )
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import qualified Beckn.Types.Core.API.Cancel as API
import qualified Beckn.Types.Core.API.Status as API
import qualified Beckn.Types.Core.API.Update as API
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context (Context (..))
import Beckn.Types.Core.DecimalValue (convertAmountToDecimalValue)
import qualified Beckn.Types.Core.Domain as Domain
import Beckn.Types.Core.Order
  ( Order (..),
    OrderItem (OrderItem),
  )
import Beckn.Types.Core.Price
import Beckn.Types.Core.Scalar
import Beckn.Types.Id
import Beckn.Types.Mobility.Driver (Driver)
import qualified Beckn.Types.Mobility.Order as Mobility
import Beckn.Types.Mobility.Payload (Payload (..))
import Beckn.Types.Mobility.Route
import Beckn.Types.Mobility.Trip (Trip (..))
import qualified Beckn.Types.Mobility.Vehicle as BVehicle
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import Control.Lens ((?~))
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified ExternalAPI.Flow as ExternalAPI
import qualified ExternalAPI.Transform as ExternalAPITransform
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.DriverInformation as DriverInformation
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Organization as Organization
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.ProductInstance as ProductInstance
import qualified Storage.Queries.RideCancellationReason as QRCR
import qualified Storage.Queries.RideRequest as RideRequest
import qualified Storage.Queries.Vehicle as Vehicle
import qualified Test.RandomStrings as RS
import Types.App (Ride)
import Types.Error
import Types.Metrics (CoreMetrics)
import qualified Types.Storage.Case as Case
import qualified Types.Storage.Organization as Organization
import qualified Types.Storage.Person as Person
import qualified Types.Storage.ProductInstance as ProductInstance
import qualified Types.Storage.RideCancellationReason as SRCR
import qualified Types.Storage.RideRequest as SRideRequest
import Utils.Common
import qualified Utils.Notifications as Notify

cancel ::
  Id Organization.Organization ->
  SignatureAuthResult Organization.Organization ->
  API.CancelReq ->
  FlowHandler AckResponse
cancel transporterId _ req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    let context = req.context
    validateContext "cancel" context
    let prodInstId = req.message.order.id -- transporter search productInstId
    transporterOrg <-
      Organization.findOrganizationById transporterId
        >>= fromMaybeM OrgNotFound
    prodInst <- ProductInstance.findById (Id prodInstId) >>= fromMaybeM PIDoesNotExist
    piList <- ProductInstance.findAllByParentId (prodInst.id)
    orderPi <-
      ProductInstance.findByIdType (ProductInstance.id <$> piList) ProductInstance.RIDEORDER
        >>= fromMaybeM PINotFound
    RideRequest.createFlow =<< mkRideReq (orderPi.id) (transporterOrg.shortId) SRideRequest.CANCELLATION
    return Ack

cancelRide ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    FCMFlow m r,
    CoreMetrics m
  ) =>
  Id Ride ->
  SRCR.RideCancellationReason ->
  m ()
cancelRide rideId rideCReason = do
  orderPi <- ProductInstance.findById (cast rideId) >>= fromMaybeM PIDoesNotExist
  searchPiId <- ProductInstance.parentId orderPi & fromMaybeM (PIFieldNotPresent "parent_id")
  searchPi <- ProductInstance.findById searchPiId >>= fromMaybeM PINotFound
  cancelRideTransaction orderPi rideCReason
  logTagInfo ("rideId-" <> getId rideId) ("Cancellation reason " <> show rideCReason.source)
  fork "cancelRide - Notify BAP" $ do
    let transporterId = ProductInstance.organizationId orderPi
    transporter <-
      Organization.findOrganizationById transporterId
        >>= fromMaybeM OrgNotFound
    notifyCancelToGateway searchPi transporter rideCReason.source
    case_ <- Case.findById (orderPi.caseId) >>= fromMaybeM CaseNotFound
    whenJust orderPi.personId $ \driverId -> do
      driver <- Person.findPersonById driverId >>= fromMaybeM PersonNotFound
      Notify.notifyOnCancel case_ driver.id driver.deviceToken rideCReason.source

cancelRideTransaction ::
  DBFlow m r =>
  ProductInstance.ProductInstance ->
  SRCR.RideCancellationReason ->
  m ()
cancelRideTransaction orderPi rideCReason = DB.runSqlDBTransaction $ do
  let mbPersonId = orderPi.personId
  whenJust mbPersonId updateDriverInfo
  ProductInstance.updateStatus orderPi.id ProductInstance.CANCELLED
  QRCR.create rideCReason
  where
    updateDriverInfo personId = do
      let driverId = cast personId
      DriverInformation.updateOnRide driverId False
      when (rideCReason.source == Mobility.ByDriver) $ QDriverStats.updateIdleTime driverId

notifyServiceStatusToGateway ::
  ( DBFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  Organization.Organization ->
  ProductInstance.ProductInstance ->
  ProductInstance.ProductInstance ->
  m ()
notifyServiceStatusToGateway transporter searchPi orderPI = do
  mkOnServiceStatusPayload (searchPi.id) orderPI
    >>= ExternalAPI.callBAP "on_status" API.onStatus transporter (searchPi.caseId) . Right

mkOnServiceStatusPayload :: MonadTime m => Id ProductInstance.ProductInstance -> ProductInstance.ProductInstance -> m API.OnStatusReqMessage
mkOnServiceStatusPayload searchPiId orderPi = do
  mkOrderRes searchPiId (getId $ orderPi.productId) (show $ orderPi.status)
    <&> API.OnStatusReqMessage
  where
    mkOrderRes searchPiId' productId status = do
      now <- getCurrentTime
      return $
        Order
          { id = getId searchPiId',
            state = T.pack status,
            items = [OrderItem productId Nothing],
            created_at = now,
            updated_at = now,
            billing = Nothing,
            payment = Nothing,
            update_action = Nothing,
            quotation = Nothing
          }

notifyTripInfoToGateway ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  ProductInstance.ProductInstance ->
  Id Case.Case ->
  Organization.Organization ->
  m ()
notifyTripInfoToGateway orderPI caseId transporter = do
  mkOnUpdatePayload orderPI
    >>= ExternalAPI.callBAP "on_update" API.onUpdate transporter caseId . Right

notifyCancelToGateway ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  ProductInstance.ProductInstance ->
  Organization.Organization ->
  Mobility.CancellationSource ->
  m ()
notifyCancelToGateway searchPi transporter cancellationSource = do
  trip <- mkCancelTripObj searchPi
  order <- ExternalAPITransform.mkOrder searchPi.id (Just trip) $ Just cancellationSource
  ExternalAPI.callBAP "on_cancel" API.onCancel transporter (searchPi.caseId) . Right $ API.OnCancelReqMessage order

mkTrip :: (DBFlow m r, EncFlow m r) => ProductInstance.ProductInstance -> m Trip
mkTrip orderPi = do
  driver <- mapM mkDriverInfo $ orderPi.personId
  vehicle <- join <$> mapM mkVehicleInfo (orderPi.entityId)
  tripCode <- orderPi.udf4 & fromMaybeM (PIFieldNotPresent "udf4")
  logTagInfo "vehicle" $ show vehicle
  return $
    Trip
      { id = tripCode,
        pickup = Nothing,
        drop = Nothing,
        state = Nothing,
        vehicle = vehicle,
        driver,
        payload = Payload Nothing Nothing [] Nothing,
        fare = mkPrice <$> orderPi.actualPrice,
        route =
          Just $
            Route
              RouteEdge
                { path = "",
                  duration = emptyScalar "seconds", -- TODO: calculate duration and put it here
                  distance = emptyScalar "meters" & #computed_value ?~ orderPi.distance
                }
      }
  where
    mkPrice price =
      emptyPrice & #computed_value ?~ convertAmountToDecimalValue price

mkOnUpdatePayload ::
  (DBFlow m r, EncFlow m r) =>
  ProductInstance.ProductInstance ->
  m API.OnUpdateOrder
mkOnUpdatePayload orderPI = do
  trip <- mkTrip orderPI
  searchPiId <- orderPI.parentId & fromMaybeM (PIFieldNotPresent "parentId")
  order <- ExternalAPITransform.mkOrder searchPiId (Just trip) Nothing
  return $ API.OnUpdateOrder order

mkDriverInfo :: (DBFlow m r, EncFlow m r) => Id Person.Person -> m Driver
mkDriverInfo driverId = do
  person <-
    Person.findPersonById driverId
      >>= fromMaybeM PersonNotFound
  ExternalAPITransform.mkDriverObj person

mkVehicleInfo :: DBFlow m r => Text -> m (Maybe BVehicle.Vehicle)
mkVehicleInfo vehicleId = do
  vehicle <- Vehicle.findVehicleById (Id vehicleId)
  return $ ExternalAPITransform.mkVehicleObj <$> vehicle

mkCancelTripObj :: (DBFlow m r, EncFlow m r) => ProductInstance.ProductInstance -> m Trip
mkCancelTripObj searchPI = do
  driver <- mapM mkDriverInfo $ searchPI.personId
  vehicle <- join <$> mapM mkVehicleInfo (searchPI.entityId)
  return $
    Trip
      { id = getId $ searchPI.id,
        pickup = Nothing,
        drop = Nothing,
        state = Nothing,
        vehicle = vehicle,
        driver,
        payload = Payload Nothing Nothing [] Nothing,
        fare = Just $ ExternalAPITransform.mkPrice searchPI,
        route = Nothing
      }

getIdShortIdAndTime :: (MonadFlow m, GuidLike m b) => m (UTCTime, b, ShortId a)
getIdShortIdAndTime = do
  now <- getCurrentTime
  guid <- generateGUID
  shortId <- T.pack <$> L.runIO (RS.randomString (RS.onlyAlphaNum RS.randomASCII) 16)
  return (now, guid, ShortId shortId)

validateContext :: HasFlowEnv m r ["coreVersion" ::: Text, "domainVersion" ::: Text] => Text -> Context -> m ()
validateContext action context = do
  validateDomain Domain.MOBILITY context
  validateContextCommons action context

mkRideReq ::
  MonadFlow m =>
  Id ProductInstance.ProductInstance ->
  ShortId Organization.Organization ->
  SRideRequest.RideRequestType ->
  m SRideRequest.RideRequest
mkRideReq prodInstID shortOrgId rideRequestType = do
  guid <- generateGUID
  currTime <- getCurrentTime
  pure
    SRideRequest.RideRequest
      { id = Id guid,
        rideId = cast prodInstID,
        shortOrgId = shortOrgId,
        createdAt = currTime,
        _type = rideRequestType,
        info = Nothing
      }

notifyUpdateToBAP ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    FCMFlow m r,
    CoreMetrics m
  ) =>
  ProductInstance.ProductInstance ->
  ProductInstance.ProductInstance ->
  ProductInstance.ProductInstanceStatus ->
  m ()
notifyUpdateToBAP searchPi orderPi updatedStatus = do
  -- Send callbacks to BAP
  transporter <-
    Organization.findOrganizationById searchPi.organizationId
      >>= fromMaybeM OrgNotFound
  notifyTripDetailsToGateway transporter searchPi orderPi
  notifyStatusUpdateReq transporter searchPi updatedStatus

notifyTripDetailsToGateway ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CoreMetrics m
  ) =>
  Organization.Organization ->
  ProductInstance.ProductInstance ->
  ProductInstance.ProductInstance ->
  m ()
notifyTripDetailsToGateway transporter searchPi orderPi = do
  notifyTripInfoToGateway orderPi searchPi.caseId transporter

notifyStatusUpdateReq ::
  ( DBFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    FCMFlow m r,
    CoreMetrics m
  ) =>
  Organization.Organization ->
  ProductInstance.ProductInstance ->
  ProductInstance.ProductInstanceStatus ->
  m ()
notifyStatusUpdateReq transporterOrg searchPi status = do
  case status of
    ProductInstance.CANCELLED -> do
      admins <- getAdmins
      notifyCancelToGateway searchPi transporterOrg Mobility.ByOrganization
      Notify.notifyCancelReqByBP searchPi admins
    ProductInstance.TRIP_REASSIGNMENT -> do
      admins <- getAdmins
      Notify.notifyDriverCancelledRideRequest searchPi admins
      notifyStatusToGateway
    _ -> notifyStatusToGateway
  where
    getAdmins = do
      if transporterOrg.enabled
        then Person.findAllByOrgId [Person.ADMIN] searchPi.organizationId
        else pure []
    notifyStatusToGateway = do
      orderPI <-
        ProductInstance.findByParentIdType (searchPi.id) ProductInstance.RIDEORDER
          >>= fromMaybeM PINotFound
      notifyServiceStatusToGateway transporterOrg searchPi orderPI
