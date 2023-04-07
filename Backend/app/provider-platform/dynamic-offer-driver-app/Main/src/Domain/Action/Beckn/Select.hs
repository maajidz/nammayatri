{-# LANGUAGE TypeApplications #-}
{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Domain.Action.Beckn.Select where

import qualified Beckn.Types.Core.Taxi.Common.Address as BA
import qualified Data.List.Extra as List
import qualified Data.Map as M
import qualified Data.Text as DT
import Data.Time.Clock (addUTCTime)
import qualified Domain.Types.FareParameters as DFareParams (FarePolicyType (..))
import qualified Domain.Types.FarePolicy as DFarePolicy (ExtraFee (..))
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import Domain.Types.Vehicle.Variant (Variant)
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (fromMaybeM, logDebug, logInfo)
import Lib.Scheduler.JobStorageType.DB.Queries (createJobIn)
import Lib.Scheduler.Types (ExecutionResult (ReSchedule))
import SharedLogic.Allocator
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers (sendSearchRequestToDrivers')
import qualified SharedLogic.CacheDistance as CD
import SharedLogic.DriverPool (getDriverPoolConfig)
import SharedLogic.FareCalculator
import SharedLogic.GoogleMaps
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.CachedQueries.FarePolicy as FarePolicyS
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.CachedQueries.SlabFarePolicy as SFarePolicyS
import qualified Storage.Queries.SearchRequest as QSReq
import Tools.Error (FarePolicyError (NoFarePolicy), GenericError (InternalError), MerchantError (MerchantNotFound))
import Tools.Maps as Maps

data DSelectReq = DSelectReq
  { messageId :: Text,
    transactionId :: Text,
    bapId :: Text,
    bapUri :: BaseUrl,
    pickupLocation :: LatLong,
    pickupTime :: UTCTime,
    dropLocation :: LatLong,
    pickupAddress :: Maybe BA.Address,
    dropAddrress :: Maybe BA.Address,
    variant :: Variant,
    autoAssignEnabled :: Bool,
    customerLanguage :: Maybe Maps.Language,
    customerExtraFee :: Maybe Money
  }

type LanguageDictionary = M.Map Maps.Language DSearchReq.SearchRequest

handler :: Id DM.Merchant -> DSelectReq -> Flow ()
handler merchantId sReq = do
  sessiontoken <- generateGUIDText
  org <- QMerch.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  fromLocation <- buildSearchReqLocation merchantId sessiontoken sReq.pickupAddress sReq.customerLanguage sReq.pickupLocation
  toLocation <- buildSearchReqLocation merchantId sessiontoken sReq.dropAddrress sReq.customerLanguage sReq.dropLocation
  mbDistRes <- CD.getCacheDistance sReq.transactionId
  logInfo $ "Fetching cached distance and duration" <> show mbDistRes
  (distance, duration) <-
    case mbDistRes of
      Nothing -> do
        res <-
          Maps.getDistance merchantId $
            Maps.GetDistanceReq
              { origin = fromLocation,
                destination = toLocation,
                travelMode = Just Maps.CAR
              }
        pure (res.distance, res.duration)
      Just distRes -> pure distRes
  (fareParams, driverExtraFare) <- case org.farePolicyType of
    DFareParams.SLAB -> do
      slabFarePolicy <- SFarePolicyS.findByMerchantIdAndVariant org.id sReq.variant >>= fromMaybeM (InternalError "Slab fare policy not found")
      let driverExtraFare = DFarePolicy.ExtraFee {minFee = 0, maxFee = 0}
      fareParams <- calculateFare merchantId (Right slabFarePolicy) distance sReq.pickupTime Nothing sReq.customerExtraFee
      pure (fareParams, driverExtraFare)
    DFareParams.NORMAL -> do
      farePolicy <- FarePolicyS.findByMerchantIdAndVariant org.id sReq.variant (Just distance) >>= fromMaybeM NoFarePolicy
      fareParams <- calculateFare merchantId (Left farePolicy) distance sReq.pickupTime Nothing sReq.customerExtraFee
      pure (fareParams, farePolicy.driverExtraFee)
  searchReq <- buildSearchRequest fromLocation toLocation merchantId sReq distance duration sReq.customerExtraFee
  let estimateFare = fareSum fareParams
  logDebug $
    "search request id=" <> show searchReq.id
      <> "; estimated distance = "
      <> show distance
      <> "; estimated base fare:"
      <> show estimateFare
  merchant <- QMerch.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  driverPoolConfig <- getDriverPoolConfig merchantId distance
  let inTime = fromIntegral driverPoolConfig.singleBatchProcessTime
  Esq.runTransaction $ do
    QSReq.create searchReq

  res <- sendSearchRequestToDrivers' driverPoolConfig searchReq merchant estimateFare driverExtraFare.minFee driverExtraFare.maxFee
  case res of
    ReSchedule _ -> do
      maxShards <- asks (.maxShards)
      Esq.runTransaction $ do
        createJobIn @_ @'SendSearchRequestToDriver inTime maxShards $
          SendSearchRequestToDriverJobData
            { requestId = searchReq.id,
              baseFare = estimateFare,
              estimatedRideDistance = distance,
              customerExtraFee = sReq.customerExtraFee,
              driverMinExtraFee = driverExtraFare.minFee,
              driverMaxExtraFee = driverExtraFare.maxFee
            }
    _ -> return ()

buildSearchRequest ::
  ( MonadTime m,
    MonadGuid m,
    MonadReader r m,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime
  ) =>
  DLoc.SearchReqLocation ->
  DLoc.SearchReqLocation ->
  Id DM.Merchant ->
  DSelectReq ->
  Meters ->
  Seconds ->
  Maybe Money ->
  m DSearchReq.SearchRequest
buildSearchRequest from to merchantId sReq distance duration customerExtraFee = do
  now <- getCurrentTime
  id_ <- Id <$> generateGUID
  searchRequestExpirationSeconds <- asks (.searchRequestExpirationSeconds)
  let validTill_ = searchRequestExpirationSeconds `addUTCTime` now
  pure
    DSearchReq.SearchRequest
      { id = id_,
        transactionId = sReq.transactionId,
        messageId = sReq.messageId,
        startTime = sReq.pickupTime,
        validTill = validTill_,
        providerId = merchantId,
        fromLocation = from,
        toLocation = to,
        bapId = sReq.bapId,
        bapUri = sReq.bapUri,
        estimatedDistance = distance,
        estimatedDuration = duration,
        customerExtraFee,
        createdAt = now,
        vehicleVariant = sReq.variant,
        status = DSearchReq.ACTIVE,
        autoAssignEnabled = sReq.autoAssignEnabled,
        searchRepeatCounter = 0,
        updatedAt = now
      }

buildSearchReqLocation :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => Id DM.Merchant -> Text -> Maybe BA.Address -> Maybe Maps.Language -> LatLong -> m DLoc.SearchReqLocation
buildSearchReqLocation merchantId sessionToken address customerLanguage latLong@Maps.LatLong {..} = do
  (Address {..}, isNearBySearchAPIUsed) <- case address of
    Just loc
      | customerLanguage == Just Maps.ENGLISH && isJust loc.ward ->
        pure
          ( Address
              { areaCode = loc.area_code,
                street = loc.street,
                city = loc.city,
                state = loc.state,
                country = loc.country,
                building = loc.building,
                area = loc.ward,
                full_address = decodeAddress loc
              },
            False
          )
    _ -> do
      add <- getAddressByGetPlaceName merchantId sessionToken latLong
      if (DT.length <$> add.area) == Just 0
        then (,True) <$> getAddressByNearBySearch merchantId add latLong
        else pure ((,False) add)
  id <- Id <$> generateGUID
  now <- getCurrentTime
  let createdAt = now
      updatedAt = now
  pure DLoc.SearchReqLocation {..}

getAddressByGetPlaceName :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => Id DM.Merchant -> Text -> LatLong -> m Address
getAddressByGetPlaceName merchantId sessionToken latLong = do
  pickupRes <-
    Maps.getPlaceName merchantId $
      Maps.GetPlaceNameReq
        { getBy = Maps.ByLatLong latLong,
          sessionToken = Just sessionToken,
          language = Nothing
        }
  mkLocation pickupRes

getAddressByNearBySearch :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, CoreMetrics m) => Id DM.Merchant -> Address -> LatLong -> m Address
getAddressByNearBySearch merchantId add latLong = do
  getNearBySearchResp <-
    Maps.getNearBySearch merchantId $
      Maps.GetNearBySearchReq
        { location = getLocation latLong,
          rankby = "distance"
        }
  let results = listToMaybe getNearBySearchResp.results
  let area = case results of
        Just result -> DT.intercalate "," $ List.takeEnd 2 $ List.dropEnd 1 (DT.split (== ',') result.vicinity)
        Nothing -> ""
  pure
    Address
      { street = add.street,
        city = add.city,
        state = add.state,
        country = add.country,
        areaCode = add.areaCode,
        building = add.building,
        area = Just area,
        full_address = getfullAddress (Just area) add.city add.state add.country
      }

decodeAddress :: BA.Address -> Maybe Text
decodeAddress BA.Address {..} = do
  let strictFields = catMaybes $ filter (not . isEmpty) [door, building, street, locality, city, state, area_code, country]
  if null strictFields
    then Nothing
    else Just $ DT.intercalate ", " strictFields

isEmpty :: Maybe Text -> Bool
isEmpty = maybe True (DT.null . DT.replace " " "")

getLocation :: LatLong -> Text
getLocation latlong = show latlong.lat <> "," <> show latlong.lon

getfullAddress :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text
getfullAddress area city state country = Just $ DT.intercalate ", " $ catMaybes [area, city, state, country]
