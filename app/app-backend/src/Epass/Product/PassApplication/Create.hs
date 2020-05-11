module Epass.Product.PassApplication.Create where

import qualified Beckn.Types.Common                    as BTC
import qualified Beckn.Types.Storage.Case              as Case
import qualified Beckn.Types.Storage.Location          as Loc
import qualified Beckn.Types.Storage.RegistrationToken as RegistrationToken
import           Data.Aeson
import qualified Epass.Data.Accessor                   as Accessor
import qualified Epass.Storage.Queries.Customer        as Customer
import qualified Epass.Storage.Queries.CustomerDetail  as CustomerDetail
import qualified Epass.Storage.Queries.Organization    as QO
import qualified Epass.Storage.Queries.PassApplication as DB
import qualified Epass.Types.API.PassApplication       as API
import           Epass.Types.App
import           Epass.Types.Common
import qualified Epass.Types.Common                    as Location (Location (..),
                                                                    LocationType)
import qualified Epass.Types.Storage.Customer          as Customer
import qualified Epass.Types.Storage.CustomerDetail    as CD
import           Epass.Types.Storage.PassApplication
import           Epass.Utils.Common
import           Epass.Utils.Extra
import           Epass.Utils.Routes
import           Epass.Utils.Storage
import qualified EulerHS.Language                      as L
import           EulerHS.Prelude
import           Servant
import qualified Storage.Queries.Location              as QL

createPassApplication ::
  Maybe Text -> API.CreatePassApplicationReq -> FlowHandler API.PassApplicationRes
createPassApplication regToken req@API.CreatePassApplicationReq {..} = withFlowHandler $ do
  token <- verifyToken regToken

  passAppInfo <-
    case _type of
      SELF        -> selfFlow token req
      SPONSOR     -> sponsorFlow token req
      BULKSPONSOR -> bulkSponsorFlow token req
  DB.create passAppInfo
  DB.findById (_id passAppInfo)
    >>= fromMaybeM500 "Could not create PassApplication"
    >>= return . API.PassApplicationRes

bulkSponsorFlow :: RegistrationToken.RegistrationToken -> API.CreatePassApplicationReq -> L.Flow PassApplication
bulkSponsorFlow token req@API.CreatePassApplicationReq {..} = do
  when
    (isNothing _OrganizationId)
    (L.throwException $ err400 {errBody = "OrganizationId cannot be empty"})
  let organizationId = fromJust _OrganizationId

  when
    (isNothing _count || _count == Just 0)
    (L.throwException $ err400 {errBody = "Count cannot be 0"})

  QO.findOrganizationById organizationId
    >>= fromMaybeM400 "Organization does not exists"
  getPassAppInfo token req Nothing

selfFlow :: RegistrationToken.RegistrationToken -> API.CreatePassApplicationReq -> L.Flow PassApplication
selfFlow token req@API.CreatePassApplicationReq {..} = do
  when
    (isNothing _CustomerId)
    (L.throwException $ err400 {errBody = "CustomerId cannot be empty"})
  when
    (isNothing _travellerName || isNothing _travellerIDType || isNothing _travellerID)
    (L.throwException $ err400 {errBody = "travellerName, travellerIDType and travellerID cannot be empty"})
  let customerId = fromJust _CustomerId
      travellerID = fromJust _travellerID
      travellerIDType = mapIdType $ fromJust _travellerIDType

  when
    (customerId /= (CustomerId (RegistrationToken._EntityId token)))
    (L.throwException $ err400 {errBody = "CustomerId mismatch"})

  Customer.updateDetails customerId _travellerName _OrganizationId
  CustomerDetail.createIfNotExists customerId travellerIDType travellerID
  getPassAppInfo token req _CustomerId

sponsorFlow :: RegistrationToken.RegistrationToken -> API.CreatePassApplicationReq -> L.Flow PassApplication
sponsorFlow token req@API.CreatePassApplicationReq {..} = do
  when
    (isNothing _travellerName || isNothing _travellerIDType || isNothing _travellerID)
    (L.throwException $ err400 {errBody = "travellerName, travellerIDType and travellerID cannot be empty"})

  let travellerName = fromJust _travellerName
      travellerID = fromJust _travellerID
      travellerIDType = mapIdType $ fromJust _travellerIDType
  CustomerDetail.findByIdentifier travellerIDType travellerID
    >>= \case
      Just cd -> getPassAppInfo token req (Just $ CD._CustomerId cd)
      Nothing -> do
        customer <- createCustomer travellerName
        let customerId = Customer._id customer
        CustomerDetail.createIfNotExists customerId travellerIDType travellerID
        getPassAppInfo token req (Just customerId)

getPassAppInfo :: RegistrationToken.RegistrationToken -> API.CreatePassApplicationReq -> Maybe CustomerId -> L.Flow PassApplication
getPassAppInfo token API.CreatePassApplicationReq {..} mCustId = do
  id <- generateGUID
  currTime <- getCurrTime
  count <- getCount _type _count
  return $
    PassApplication
      { _id = id,
        _CustomerId = mCustId,
        _passType = getPassType _type,
        _fromLocationType = Location._type <$> _fromLocation,
        _fromLat = join (Location._lat <$> _fromLocation),
        _fromLong = join (Location._long <$> _fromLocation),
        _fromWard = join (Location._ward <$> _fromLocation),
        _fromDistrict = join (Location._district <$> _fromLocation),
        _fromCity = join (Location._city <$> _fromLocation),
        _fromState = join (Location._state <$> _fromLocation),
        _fromCountry = join (Location._country <$> _fromLocation),
        _fromPincode = join (Location._pincode <$> _fromLocation),
        _fromAddress = join (Location._address <$> _fromLocation),
        _fromBound = join (Location._bound <$> _fromLocation),
        _toLocationType = Just $ Location._type _toLocation,
        _toLat = Location._lat _toLocation,
        _toLong = Location._long _toLocation,
        _toWard = Location._ward _toLocation,
        _toDistrict = Location._district _toLocation,
        _toCity = Location._city _toLocation,
        _toState = Location._state _toLocation,
        _toCountry = Location._country _toLocation,
        _toPincode = Location._pincode _toLocation,
        _toAddress = Location._address _toLocation,
        _toBound = Location._bound _toLocation,
        _createdAt = currTime,
        _updatedAt = currTime,
        _status = PENDING,
        _CreatedBy = CustomerId (RegistrationToken._EntityId token),
        _AssignedTo = UserId "admin", -- TODO: fix this
        _count = count,
        _approvedCount = 0,
        _remarks = "",
        _info = "",
        ..
      }

getPassType :: PassApplicationType -> PassType
getPassType SELF        = INDIVIDUAL
getPassType SPONSOR     = INDIVIDUAL
getPassType BULKSPONSOR = ORGANIZATION

getCount :: PassApplicationType -> Maybe Int -> L.Flow Int
getCount SELF _ = return 1
getCount SPONSOR _ = return 1
getCount BULKSPONSOR (Just c) = return c
getCount BULKSPONSOR Nothing = L.throwException $ err400 {errBody = "Count cannot be null"}

mapIdType MOBILE  = CD.MOBILENUMBER
mapIdType AADHAAR = CD.AADHAAR


getLocation ::  API.CreatePassApplicationReq -> L.Flow (Loc.Location, Loc.Location)
getLocation  API.CreatePassApplicationReq {..} = do
  id <- BTC.generateGUID
  currTime <- getCurrTime

  let fromLocation = Loc.Location
        { _id = id
        , _locationType = Loc.PINCODE -- (Location._type <$> _fromLocation)
        , _lat = join (Location._lat <$> _fromLocation)
        , _long = join (Location._long <$> _fromLocation)
        , _ward = join (Location._ward <$> _fromLocation)
        , _district = join (Location._district <$> _fromLocation)
        , _city = join (Location._city <$> _fromLocation)
        , _state = join (Location._state <$> _fromLocation)
        , _country = join (Location._country <$> _fromLocation)
        , _pincode = Nothing -- join (Location._pincode <$> _fromLocation), type mismatch
        , _address = join (Location._address <$> _fromLocation)
        , _bound  = Nothing -- join (Location._bound <$> _fromLocation)
        , _createdAt = currTime
        , _updatedAt = currTime
        }
  let toLocation = Loc.Location
        { _id = id
        , _locationType = Loc.PINCODE -- (Location._type  _toLocation)
        , _lat = Location._lat  _toLocation
        , _long = Location._long  _toLocation
        , _ward = Location._ward  _toLocation
        , _district = Location._district  _toLocation
        , _city = Location._city  _toLocation
        , _state = Location._state  _toLocation
        , _country = Location._country  _toLocation
        , _pincode = Nothing -- join (Location._pincode  _toLocation), type mismatch
        , _address = Location._address  _toLocation
        , _bound  = Nothing -- join (Location._bound  _toLocation)
        , _createdAt = currTime
        , _updatedAt = currTime
        }
  return (fromLocation, toLocation)

getCaseInfo :: RegistrationToken.RegistrationToken -> API.CreatePassApplicationReq -> Maybe CustomerId -> L.Flow Case.Case
getCaseInfo token req@API.CreatePassApplicationReq {..} mCustId = do
  id <- BTC.generateGUID
  (fromLoc,toLoc) <- getLocation req
  QL.create fromLoc
  QL.create toLoc
  currTime <- getCurrTime
  count <- getCount _type _count
  let toLocationId = show $ Loc._id toLoc
      fromLocationId = show $ Loc._id fromLoc
      shortId = ""
  return $
    Case.Case
      { _id = id
      , _name = Nothing
      , _description = Nothing
      , _shortId = shortId
      , _industry = Case.GOVT
      , _type = Case.PASSAPPLICATION
      , _exchangeType = Case.ORDER
      , _status = Case.NEW
      , _startTime = _fromDate
      , _endTime = Just _toDate
      , _validTill = _fromDate
      , _provider = Nothing
      , _providerType = Just Case.GOVTADMIN
      , _requestor = show <$> mCustId
      , _requestorType = Just Case.CONSUMER
      , _parentCaseId = Nothing
      , _fromLocationId = fromLocationId
      , _toLocationId = toLocationId
      , _udf1 = Just $ show $ getPassType _type
      , _udf2 = Nothing
      , _udf3 = Nothing
      , _udf4 = Nothing
      , _udf5 = Nothing
      , _info = Nothing
      , _createdAt = currTime
      , _updatedAt = currTime
      }

createCustomer :: Text -> L.Flow Customer.Customer
createCustomer name = do
  id <- generateGUID
  Customer.create =<< (getCust id)
  Customer.findCustomerById id
    >>= fromMaybeM500 "Unable to create customer"
  where
    getCust id = do
      now <- getCurrentTimeUTC
      return $
        Customer.Customer
          { _id = id,
            _name = Just name,
            _OrganizationId = Nothing,
            _TenantOrganizationId = Nothing,
            _verified = False,
            _role = Customer.INDIVIDUAL,
            _info = Nothing,
            _createdAt = now,
            _updatedAt = now
          }
