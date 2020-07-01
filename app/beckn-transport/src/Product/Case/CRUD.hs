{-# LANGUAGE OverloadedLabels #-}

module Product.Case.CRUD where

import Beckn.Types.API.Search
import Beckn.Types.App
import Beckn.Types.Common as BC
import Beckn.Types.Core.Catalog
import Beckn.Types.Core.Category
import Beckn.Types.Core.Context
import Beckn.Types.Core.Item
import Beckn.Types.Core.Price
import Beckn.Types.Mobility.Service
import Beckn.Types.Storage.Case as Case
import Beckn.Types.Storage.Location as Location
import Beckn.Types.Storage.Organization as Organization
import qualified Beckn.Types.Storage.Person as SP
import Beckn.Types.Storage.ProductInstance as ProdInst
import Beckn.Types.Storage.Products as Product
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common
import Beckn.Utils.Extra
import qualified Data.Accessor as Lens
import Data.Aeson
import Data.Scientific
import qualified Data.Text as T
import Data.Time.LocalTime
import qualified EulerHS.Language as L
import EulerHS.Prelude
import External.Gateway.Flow as Gateway
import External.Gateway.Transform as GT
import Servant
import Storage.Queries.Case as Case
import Storage.Queries.Location as LQ
import Storage.Queries.Organization as OQ
import qualified Storage.Queries.Organization as OQ
import qualified Storage.Queries.Person as QP
import Storage.Queries.ProductInstance as CPQ
import Storage.Queries.Products as PQ
import qualified Storage.Queries.RegistrationToken as QR
import System.Environment
import qualified Test.RandomStrings as RS
import Types.API.Case
import qualified Types.API.ProductInstance as CPR
import Types.API.Registration
import qualified Utils.Defaults as Default

list :: RegToken -> [CaseStatus] -> CaseType -> Maybe Int -> Maybe Int -> Maybe Bool -> FlowHandler CaseListRes
list regToken status csType limitM offsetM ignoreOffered = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.verifyToken regToken
  person <- QP.findPersonById (PersonId _EntityId)
  now <- getCurrentTimeUTC
  case (person ^. #_organizationId) of
    Just orgId -> do
      org <- OQ.findOrganizationById (OrganizationId orgId)
      ignoreList <-
        if (ignoreOffered == Just True)
          then do
            resList <- CPQ.productInstanceJoinWithoutLimits csType orgId []
            let csIgnoreList = Case._id <$> (CPR._case <$> resList)
            return csIgnoreList
          else return []
      caseList <-
        if not (org ^. #_enabled)
          then Case.findAllByTypeStatusTime limit offset csType status ignoreList now $ fromMaybe now (org ^. #_fromTime)
          else Case.findAllByTypeStatuses limit offset csType status ignoreList now
      locList <- LQ.findAllByLocIds (Case._fromLocationId <$> caseList) (Case._toLocationId <$> caseList)
      return $ catMaybes $ joinByIds locList <$> caseList
    Nothing -> L.throwException $ err400 {errBody = "ORG_ID MISSING"}
  where
    limit = (toInteger $ fromMaybe Default.limit limitM)
    offset = (toInteger $ fromMaybe Default.offset offsetM)
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

-- Update Case
-- Transporter Accepts a Ride with Quote
-- TODO fromLocation toLocation getCreatedTimeFromInput
update :: RegToken -> Text -> UpdateCaseReq -> FlowHandler Case
update regToken caseId UpdateCaseReq {..} = withFlowHandler $ do
  SR.RegistrationToken {..} <- QR.verifyToken regToken
  person <- QP.findPersonById (PersonId _EntityId)
  c <- Case.findById $ CaseId caseId
  case (SP._organizationId person) of
    Just orgId -> case _transporterChoice of
      "ACCEPTED" -> do
        p <- createProduct c _quote orgId Product.INSTOCK
        cp <- createProductInstance c p ProdInst.INSTOCK
        notifyGateway c p orgId
        return c
      "DECLINED" -> do
        p <- createProduct c _quote orgId Product.OUTOFSTOCK
        cp <- createProductInstance c p ProdInst.OUTOFSTOCK
        return c
    Nothing -> L.throwException $ err400 {errBody = "ORG_ID MISSING"}

createProduct :: Case -> Maybe Double -> Text -> Product.ProductsStatus -> L.Flow Products
createProduct cs price orgId status = do
  prodId <- L.generateGUID
  (currTime :: LocalTime) <- getCurrentTimeUTC
  shortId <- L.runIO $ RS.randomString (RS.onlyAlphaNum RS.randomASCII) 16
  let product = getProduct prodId price cs currTime orgId shortId
  PQ.create product
  return $ product
  where
    getProduct prodId price cs currTime orgId shortId =
      Products
        { _id = ProductsId prodId,
          _shortId = T.pack shortId,
          _name = Case._name cs,
          _description = Case._description cs,
          _industry = Case._industry cs,
          _type = RIDE,
          _status = status,
          _startTime = Case._startTime cs,
          _endTime = Case._endTime cs,
          _validTill = Case._validTill cs,
          _price = fromFloatDigits $ fromMaybe 0 price,
          _rating = Nothing,
          _review = Nothing,
          _udf1 = Case._udf1 cs,
          _udf2 = Case._udf2 cs,
          _udf3 = Case._udf3 cs,
          _udf4 = Case._udf4 cs,
          _udf5 = Case._udf5 cs,
          _info = Case._info cs,
          _organizationId = orgId,
          _createdAt = currTime,
          _updatedAt = currTime,
          _fromLocation = Just (Case._fromLocationId cs),
          _toLocation = Just (Case._toLocationId cs),
          _assignedTo = Nothing
        }

createProductInstance :: Case -> Products -> ProdInst.ProductInstanceStatus -> L.Flow ProductInstance
createProductInstance cs prod status = do
  cpId <- L.generateGUID
  (currTime :: LocalTime) <- getCurrentTimeUTC
  let productInst = getProdInst cpId cs prod currTime
  CPQ.create productInst
  return $ productInst
  where
    getProdInst cpId cs prod currTime =
      ProductInstance
        { _id = ProductInstanceId cpId,
          _caseId = Case._id cs,
          _productId = Product._id prod,
          _personId = Nothing,
          _quantity = 1,
          _price = Product._price prod,
          _status = status,
          _info = Nothing,
          _createdAt = Case._createdAt cs,
          _updatedAt = currTime
        }

notifyGateway :: Case -> Products -> Text -> L.Flow ()
notifyGateway c p orgId = do
  L.logInfo "notifyGateway" $ show c
  cps <- CPQ.findAllByCaseId (c ^. #_id)
  L.logInfo "notifyGateway" $ show p
  orgInfo <- OQ.findOrganizationById (OrganizationId orgId)
  onSearchPayload <- mkOnSearchPayload c [p] cps orgInfo
  L.logInfo "notifyGateway Request" $ show onSearchPayload
  Gateway.onSearch onSearchPayload
  return ()

mkOnSearchPayload :: Case -> [Products] -> [ProductInstance] -> Organization -> L.Flow OnSearchReq
mkOnSearchPayload c prods cps orgInfo = do
  currTime <- getCurrentTimeUTC
  let context =
        Context
          { domain = "MOBILITY",
            action = "SEARCH",
            version = Just $ "0.1",
            transaction_id = c ^. #_shortId, -- TODO : What should be the txnId
            message_id = Nothing,
            timestamp = currTime,
            dummy = ""
          }
  service <- GT.mkServiceOffer c prods cps Nothing (Just orgInfo)
  return
    OnSearchReq
      { context,
        message = service
      }
