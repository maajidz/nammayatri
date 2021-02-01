{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Product.CustomerSupport where

import App.Types
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.App
import Beckn.Types.Storage.Case as C
import Beckn.Types.Storage.Person as SP
import Beckn.Types.Storage.ProductInstance as ProductInstance
import Beckn.Utils.Common
import EulerHS.Prelude
import Storage.Queries.Case as Case
import qualified Storage.Queries.Location as Location
import Storage.Queries.Person as Person
import Storage.Queries.ProductInstance as PI
import Types.API.CustomerSupport as T
import Types.ProductInfo as ProductInfo

listOrder :: Text -> FlowHandler [T.OrderResp]
listOrder mobileNumber = withFlowHandler $ do
  person <-
    Person.findByRoleAndMobileNumberWithoutCC SP.ADMIN mobileNumber --TODO: Change ADMIN to USER
      >>= fromMaybeM400 "Invalid MobileNumber"
  let personId = person ^. #_id
  searchcases <-
    Case.findAllByTypeAndStatuses personId C.RIDESEARCH [C.NEW, C.INPROGRESS, C.CONFIRMED, C.COMPLETED, C.CLOSED] Nothing Nothing
      >>= either DB.throwDBError pure
  -- let serachIds = map (\x-> x ^. #_id) searchcases
  -- Want to use this and create a Hashmap so we can modify in O(n)
  -- confiremedOrders <- Case.findAllByParentIdsAndCaseType serachIds C.RIDEORDER
  -- mapOrder <- foldl createhashMap  empty confiremedOrders empty
  traverse makeCaseToOrder searchcases

makeCaseToOrder :: C.Case -> Flow T.OrderResp
makeCaseToOrder C.Case {..} = do
  (confiremedOrder :: Maybe C.Case) <-
    Case.findOneByParentIdAndCaseType _id C.RIDEORDER
      >>= either DB.throwDBError pure
  let (status :: Maybe CaseStatus) = maybe Nothing (\x -> Just $ x ^. #_status) confiremedOrder <|> (Just _status)
  fromLocation <- Location.findLocationById $ LocationId _fromLocationId
  toLocation <- Location.findLocationById $ LocationId _toLocationId
  trip <- makeTripDetails confiremedOrder
  --  Info: udf1 is vechicle variant
  let details =
        T.OrderDetails
          { _id = (_getCaseId _id),
            _status = status,
            _createdAt = _createdAt,
            _updatedAt = _updatedAt,
            _startTime = _startTime,
            _endTime = _endTime,
            _fromLocation = fromLocation,
            _toLocation = toLocation,
            _vehicleVariant = _udf1, -- Note: UDF1 Contain _vehicleVariant info
            _trip = trip
          }
  pure $ T.OrderResp {_order = details}

makeTripDetails :: Maybe C.Case -> Flow (Maybe T.TripDetails)
makeTripDetails caseM = case caseM of
  Nothing -> pure Nothing
  Just C.Case {_id} -> do
    -- Note: In case of Confirmed Order only one Product Instance will be Present
    ProductInstance.ProductInstance {_status, _info, _price} <-
      head
        <$> ( PI.findAllByCaseId _id
                >>= either DB.throwDBError pure
            )
    let (mproductInfo :: Maybe ProductInfo) = decodeFromText =<< _info
        provider = maybe Nothing (\x -> x ^. #_provider) mproductInfo
        mtracker = maybe Nothing (\x -> x ^. #_tracker) mproductInfo
        mtrip = maybe Nothing (\x -> Just $ x ^. #_trip) mtracker
        driver = maybe Nothing (\x -> x ^. #driver) mtrip
        vehicle = maybe Nothing (\x -> x ^. #vehicle) mtrip
    pure $ Just $
      T.TripDetails
        { _id = _getCaseId _id,
          _status = _status,
          _driver = driver,
          _price = _price,
          _provider = provider,
          _vehicle = vehicle
        }
