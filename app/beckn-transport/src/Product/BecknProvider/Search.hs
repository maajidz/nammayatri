module Product.BecknProvider.Search (search) where

import App.Types
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Amount
import Beckn.Types.Common
import qualified Beckn.Types.Core.API.Callback as Callback
import qualified Beckn.Types.Core.API.Search as API
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Domain as Domain
import qualified Beckn.Types.Core.Error as Core
import qualified Beckn.Types.Core.Tag as Tag
import Beckn.Types.Id
import qualified Beckn.Types.Mobility.Stop as Stop
import qualified Beckn.Types.Storage.Case as Case
import qualified Beckn.Types.Storage.Location as Location
import qualified Beckn.Types.Storage.Organization as Org
import qualified Beckn.Types.Storage.ProductInstance as ProductInstance
import qualified Beckn.Types.Storage.Vehicle as Vehicle
import qualified Data.List as List
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified ExternalAPI.Flow as ExternalAPI
import qualified ExternalAPI.Transform as ExternalAPITransform
import qualified Models.ProductInstance as MPI
import qualified Product.BecknProvider.BP as BP
import Product.FareCalculator
import qualified Product.Location as Location
import qualified Product.Person as Person
import qualified Storage.Queries.Case as QCase
import qualified Storage.Queries.Location as Loc
import qualified Storage.Queries.Organization as Org
import qualified Storage.Queries.ProductInstance as ProductInstance
import qualified Storage.Queries.Products as SProduct
import qualified Test.RandomStrings as RS
import qualified Types.API.Case as APICase
import Types.Error
import Utils.Common

search :: Id Org.Organization -> Org.Organization -> API.SearchReq -> FlowHandler AckResponse
search transporterId bapOrg req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    let context = req.context
    BP.validateContext "search" context
    uuid <- L.generateGUID
    transporter <- Org.findOrganizationById transporterId
    when (transporter.enabled) $ do
      let intent = req.message.intent
      now <- getCurrentTime
      let pickup = head $ intent.pickups
      let dropOff = head $ intent.drops
      let startTime = pickup.departure_time.est
      validity <- getValidTime now startTime
      fromLocation <- mkFromStop now pickup
      toLocation <- mkFromStop now dropOff
      let bapOrgId = bapOrg.id
      bapCallbackUrl <- bapOrg.callbackUrl & fromMaybeM (OrgFieldNotPresent "callback_url")
      deadDistance <- calculateDeadDistance transporter fromLocation
      let productCase = mkCase req uuid now validity startTime fromLocation toLocation transporterId bapOrgId deadDistance
      DB.runSqlDBTransaction $ do
        Loc.create fromLocation
        Loc.create toLocation
        QCase.create productCase
      fork "OnSearchCallback" $ onSearchCallback bapCallbackUrl productCase transporter fromLocation toLocation
    return Ack

mkFromStop :: UTCTime -> Stop.Stop -> Flow Location.Location
mkFromStop now stop = do
  let loc = stop.location
  let mgps = loc.gps
  let maddress = loc.address
  uuid <- Id <$> L.generateGUID
  pure $
    Location.Location
      { id = uuid,
        locationType = Location.POINT,
        lat = read . T.unpack . (.lat) <$> mgps,
        long = read . T.unpack . (.lon) <$> mgps,
        ward = (.ward) =<< maddress,
        district = Nothing,
        city = (.city) <$> maddress,
        state = (.state) <$> maddress,
        country = (.country) <$> maddress,
        pincode = (.area_code) <$> maddress,
        address = encodeToText <$> maddress,
        bound = Nothing,
        point = Location.Point,
        createdAt = now,
        updatedAt = now
      }

getValidTime :: UTCTime -> UTCTime -> Flow UTCTime
getValidTime now startTime = do
  caseExpiry_ <- fromMaybe 7200 . caseExpiry <$> ask
  let minExpiry = 300 -- 5 minutes
      timeToRide = startTime `diffUTCTime` now
      validTill = addUTCTime (minimum [fromInteger caseExpiry_, maximum [minExpiry, timeToRide]]) now
  pure validTill

mkCase :: API.SearchReq -> Text -> UTCTime -> UTCTime -> UTCTime -> Location.Location -> Location.Location -> Id Org.Organization -> Id Org.Organization -> Maybe Float -> Case.Case
mkCase req uuid now validity startTime fromLocation toLocation transporterId bapOrgId deadDistance = do
  let intent = req.message.intent
  let distance = Tag.value <$> find (\x -> x.key == "distance") (fromMaybe [] $ intent.tags)
  let tId = getId transporterId
  let bapId = getId bapOrgId
  Case.Case
    { id = Id uuid,
      name = Nothing,
      description = Just "Case to search for a Ride",
      shortId = ShortId $ tId <> "_" <> req.context.transaction_id,
      industry = Case.MOBILITY,
      _type = Case.RIDESEARCH,
      exchangeType = Case.FULFILLMENT,
      status = Case.NEW,
      startTime = startTime,
      endTime = Nothing,
      validTill = validity,
      provider = Just tId,
      providerType = Nothing,
      requestor = Nothing,
      requestorType = Just Case.CONSUMER,
      parentCaseId = Nothing,
      fromLocationId = fromLocation.id,
      toLocationId = toLocation.id,
      udf1 = Just $ intent.vehicle.variant,
      udf2 = Just . show . length $ intent.payload.travellers,
      udf3 = encodeToText <$> deadDistance,
      udf4 = Just bapId,
      udf5 = distance,
      info = Nothing, --Just $ show $ req.message
      createdAt = now,
      updatedAt = now
    }

calculateDeadDistance :: Org.Organization -> Location.Location -> Flow (Maybe Float)
calculateDeadDistance organization fromLocation = do
  eres <- runSafeFlow do
    orgLocId <- organization.locationId & fromMaybeM (OrgFieldNotPresent "location_id")
    mbOrgLocation <- Loc.findLocationById orgLocId
    case mbOrgLocation of
      Nothing -> throwError LocationNotFound
      Just orgLocation -> Location.calculateDistance orgLocation fromLocation
  case eres of
    Left err -> do
      logTagWarning "calculateDeadDistance" $ "Failed to calculate distance. Reason: " +|| err ||+ ""
      pure Nothing
    Right mDistance -> return mDistance

onSearchCallback :: BaseUrl -> Case.Case -> Org.Organization -> Location.Location -> Location.Location -> Flow ()
onSearchCallback bapUri productCase transporter fromLocation toLocation = do
  let transporterId = transporter.id
  result <- runSafeFlow $ do
    vehicleVariant :: Vehicle.Variant <-
      (productCase.udf1 >>= readMaybe . T.unpack)
        & fromMaybeM (CaseFieldNotPresent "udf1")
    pool <-
      Person.calculateDriverPool (fromLocation.id) transporterId vehicleVariant
    logTagInfo "OnSearchCallback" $
      "Calculated Driver Pool for organization " +|| getId transporterId ||+ " with drivers " +| T.intercalate ", " (getId <$> pool) |+ ""
    let piStatus =
          if null pool
            then ProductInstance.OUTOFSTOCK
            else ProductInstance.INSTOCK
    price <-
      if null pool
        then return Nothing
        else Just <$> calculateFare transporterId vehicleVariant fromLocation toLocation (productCase.startTime) (productCase.udf5)
    prodInst <- mkProductInstance productCase price piStatus transporterId
    let caseStatus ProductInstance.INSTOCK = Case.CONFIRMED
        caseStatus _ = Case.CLOSED
    DB.runSqlDBTransaction $ do
      ProductInstance.create prodInst
      QCase.updateStatus (productCase.id) (caseStatus $ prodInst.status)
    pure prodInst
  case result of
    Right prodInst -> do
      let productStatus = prodInst.status
      logTagInfo "OnSearchCallback" $
        "Sending on_search callback with status " +|| productStatus ||+ " for product " +|| prodInst.id ||+ ""
      void $ sendOnSearchSuccess bapUri productCase transporter prodInst
    Left err -> do
      logTagError "OnSearchCallback" $ "Error happened when sending on_search request. Error: " +|| err ||+ ""
      void $ sendOnSearchFailed bapUri productCase transporter err

mkProductInstance :: Case.Case -> Maybe Amount -> ProductInstance.ProductInstanceStatus -> Id Org.Organization -> Flow ProductInstance.ProductInstance
mkProductInstance productCase price status transporterId = do
  productInstanceId <- Id <$> L.generateGUID
  now <- getCurrentTime
  shortId <- L.runIO $ T.pack <$> RS.randomString (RS.onlyAlphaNum RS.randomASCII) 16
  products <- SProduct.findByName $ fromMaybe "DONT MATCH" (productCase.udf1)
  let productInstance =
        ProductInstance.ProductInstance
          { id = productInstanceId,
            caseId = productCase.id,
            productId = products.id,
            personId = Nothing,
            personUpdatedAt = Nothing,
            shortId = ShortId shortId,
            entityType = ProductInstance.VEHICLE,
            entityId = Nothing,
            quantity = 1,
            _type = Case.RIDESEARCH,
            price = price,
            status = status,
            startTime = productCase.startTime,
            endTime = productCase.endTime,
            validTill = productCase.validTill,
            fromLocation = Just $ productCase.fromLocationId,
            toLocation = Just $ productCase.toLocationId,
            organizationId = transporterId,
            parentId = Nothing,
            udf1 = productCase.udf1,
            udf2 = productCase.udf2,
            udf3 = productCase.udf3,
            udf4 = productCase.udf4,
            udf5 = productCase.udf5,
            info = productCase.info,
            createdAt = now,
            updatedAt = now
          }
  pure productInstance

sendOnSearchFailed :: BaseUrl -> Case.Case -> Org.Organization -> Text -> Flow AckResponse
sendOnSearchFailed bapUri productCase transporterOrg err = do
  appEnv <- ask
  currTime <- getCurrentTime
  let context =
        Context.Context
          { domain = Domain.MOBILITY,
            country = Just "IND",
            city = Nothing,
            action = "on_search",
            core_version = Just "0.8.2",
            domain_version = Just "0.8.2",
            transaction_id = last $ T.split (== '_') . getShortId $ productCase.shortId,
            message_id = getShortId $ productCase.shortId,
            bap_uri = Just bapUri,
            bpp_uri = Just $ BP.makeBppUrl transporterOrg $ nwAddress appEnv,
            timestamp = currTime,
            ttl = Nothing
          }
  let payload =
        Callback.CallbackReq
          { context,
            contents =
              Left $
                Core.Error
                  { _type = Core.DOMAIN_ERROR,
                    code = err,
                    path = Nothing,
                    message = Nothing
                  }
          }
  let bppShortId = getShortId $ transporterOrg.shortId
  ExternalAPI.onSearch payload bppShortId
  return Ack

sendOnSearchSuccess :: BaseUrl -> Case.Case -> Org.Organization -> ProductInstance.ProductInstance -> Flow AckResponse
sendOnSearchSuccess bapUri productCase transporterOrg productInstance = do
  let piStatus = productInstance.status
  let productInstances =
        case piStatus of
          ProductInstance.OUTOFSTOCK -> []
          _ -> [productInstance]
  onSearchPayload <- mkOnSearchPayload bapUri productCase productInstances transporterOrg
  let bppShortId = getShortId $ transporterOrg.shortId
  ExternalAPI.onSearch onSearchPayload bppShortId
  return Ack

mkOnSearchPayload :: BaseUrl -> Case.Case -> [ProductInstance.ProductInstance] -> Org.Organization -> Flow API.OnSearchReq
mkOnSearchPayload bapUri productCase productInstances transporterOrg = do
  currTime <- getCurrentTime
  appEnv <- ask
  let context =
        Context.Context
          { domain = Domain.MOBILITY,
            country = Just "IND",
            city = Nothing,
            action = "on_search",
            core_version = Just "0.8.2",
            domain_version = Just "0.8.2",
            transaction_id = last $ T.split (== '_') . getShortId $ productCase.shortId,
            message_id = getShortId $ productCase.shortId,
            bap_uri = Just bapUri,
            bpp_uri = Just $ BP.makeBppUrl transporterOrg $ nwAddress appEnv,
            timestamp = currTime,
            ttl = Nothing
          }
  piCount <- MPI.getCountByStatus (transporterOrg.id) Case.RIDEORDER
  let stats = mkProviderStats piCount
  let provider = mkProviderInfo transporterOrg stats
  catalog <- ExternalAPITransform.mkCatalog productCase productInstances provider
  return
    Callback.CallbackReq
      { context,
        contents = Right $ API.OnSearchServices catalog
      }

mkProviderInfo :: Org.Organization -> APICase.ProviderStats -> APICase.ProviderInfo
mkProviderInfo org stats =
  APICase.ProviderInfo
    { id = getId $ org.id,
      name = org.name,
      stats = encodeToText stats,
      contacts = fromMaybe "" (org.mobileNumber)
    }

mkProviderStats :: [(ProductInstance.ProductInstanceStatus, Int)] -> APICase.ProviderStats
mkProviderStats piCount =
  APICase.ProviderStats
    { completed = List.lookup ProductInstance.COMPLETED piCount,
      inprogress = List.lookup ProductInstance.INPROGRESS piCount,
      confirmed = List.lookup ProductInstance.CONFIRMED piCount
    }
