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

module Domain.Action.Beckn.Select
  ( DSelectReq (..),
    validateRequest,
    handler,
  )
where

import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.FarePolicy as DFarePolicy
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (addUTCTime, fromMaybeM, logDebug, throwError)
import Lib.Scheduler.JobStorageType.DB.Queries (createJobIn')
import Lib.Scheduler.Types (ExecutionResult (ReSchedule))
import SharedLogic.Allocator
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers (sendSearchRequestToDrivers')
import SharedLogic.DriverPool (getDriverPoolConfig)
import SharedLogic.FareCalculator
import qualified Storage.CachedQueries.FarePolicy as FarePolicyS
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.Queries.Estimate as QEst
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchTry as QST
import Tools.Error

data DSelectReq = DSelectReq
  { messageId :: Text,
    transactionId :: Text,
    estimateId :: Id DEst.Estimate,
    bapId :: Text,
    bapUri :: BaseUrl,
    pickupTime :: UTCTime,
    estimateId :: Id DEst.Estimate,
    autoAssignEnabled :: Maybe Bool,
    customerExtraFee :: Maybe Money
  }

handler :: DM.Merchant -> DSelectReq -> DEst.Estimate -> Flow ()
handler merchant sReq estimate = do
  let merchantId = merchant.id
  searchReq <- QSR.findById estimate.requestId >>= fromMaybeM (SearchRequestNotFound estimate.requestId.getId)
  farePolicy <- FarePolicyS.findByMerchantIdAndVariant merchant.id estimate.vehicleVariant >>= fromMaybeM NoFarePolicy

  searchTry <- createNewSearchTry farePolicy searchReq
  driverPoolConfig <- getDriverPoolConfig merchantId searchReq.estimatedDistance
  let inTime = fromIntegral driverPoolConfig.singleBatchProcessTime

  let driverExtraFeeBounds = DFarePolicy.findDriverExtraFeeBoundsByDistance searchReq.estimatedDistance <$> farePolicy.driverExtraFeeBounds
  res <- sendSearchRequestToDrivers' driverPoolConfig searchReq searchTry merchant driverExtraFeeBounds
  case res of
    ReSchedule _ -> do
      maxShards <- asks (.maxShards)
      -- Esq.runTransaction $ do
      --   whenJust sReq.autoAssignEnabled $ QSR.updateAutoAssign searchReq.id
      --   createJobIn @_ @'SendSearchRequestToDriver inTime maxShards $
      --     SendSearchRequestToDriverJobData
      --       { searchTryId = searchTry.id,
      --         estimatedRideDistance = searchReq.estimatedDistance,
      --         driverExtraFeeBounds = driverExtraFeeBounds
      --       }
      Esq.runTransaction $ whenJust sReq.autoAssignEnabled $ QSR.updateAutoAssign searchReq.id
      createJobIn' @_ @'SendSearchRequestToDriver inTime maxShards $
        SendSearchRequestToDriverJobData
          { searchTryId = searchTry.id,
            estimatedRideDistance = searchReq.estimatedDistance,
            driverExtraFeeBounds = driverExtraFeeBounds
          }
    _ -> return ()
  where
    createNewSearchTry :: DFP.FarePolicy -> DSR.SearchRequest -> Flow DST.SearchTry
    createNewSearchTry farePolicy searchReq = do
      mbLastSearchTry <- QST.findLastByRequestId searchReq.id
      fareParams <-
        calculateFareParameters
          CalculateFareParametersParams
            { farePolicy = farePolicy,
              distance = searchReq.estimatedDistance,
              rideTime = sReq.pickupTime,
              waitingTime = Nothing,
              driverSelectedFare = Nothing,
              customerExtraFee = sReq.customerExtraFee
            }
      let estimatedFare = fareSum fareParams
          pureEstimatedFare = pureFareSum fareParams
      searchTry <- case mbLastSearchTry of
        Nothing -> do
          searchTry <- buildSearchTry merchant.id searchReq.id estimate sReq estimatedFare searchReq.estimatedDistance searchReq.estimatedDuration 0 DST.INITIAL
          -- Esq.runTransaction $ do
          _ <- QST.create searchTry
          return searchTry
        Just oldSearchTry -> do
          let searchRepeatType = if oldSearchTry.status == DST.ACTIVE then DST.CANCELLED_AND_RETRIED else DST.RETRIED
          -- hack check, i think we should store whole breakup instead of  single baseFare value
          unless (pureEstimatedFare == oldSearchTry.baseFare - fromMaybe 0 oldSearchTry.customerExtraFee) $
            throwError SearchTryEstimatedFareChanged
          searchTry <- buildSearchTry merchant.id searchReq.id estimate sReq estimatedFare searchReq.estimatedDistance searchReq.estimatedDuration (oldSearchTry.searchRepeatCounter + 1) searchRepeatType
          -- Esq.runTransaction $ do
          when (oldSearchTry.status == DST.ACTIVE) $ QST.updateStatus oldSearchTry.id DST.CANCELLED
          _ <- QST.create searchTry
          return searchTry

      logDebug $
        "search try id=" <> show searchTry.id
          <> "; estimated distance = "
          <> show searchReq.estimatedDistance
          <> "; estimated base fare:"
          <> show estimatedFare
      return searchTry

buildSearchTry ::
  ( MonadTime m,
    MonadGuid m,
    MonadReader r m,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime
  ) =>
  Id DM.Merchant ->
  Id DSR.SearchRequest ->
  DEst.Estimate ->
  DSelectReq ->
  Money ->
  Meters ->
  Seconds ->
  Int ->
  DST.SearchRepeatType ->
  m DST.SearchTry
buildSearchTry merchantId searchReqId estimate sReq baseFare distance duration searchRepeatCounter searchRepeatType = do
  now <- getCurrentTime
  id_ <- Id <$> generateGUID
  searchRequestExpirationSeconds <- asks (.searchRequestExpirationSeconds)
  let validTill_ = searchRequestExpirationSeconds `addUTCTime` now
      customerExtraFee = sReq.customerExtraFee
  pure
    DST.SearchTry
      { id = id_,
        requestId = searchReqId,
        estimateId = estimate.id,
        messageId = sReq.messageId,
        startTime = sReq.pickupTime,
        validTill = validTill_,
        vehicleVariant = estimate.vehicleVariant,
        status = DST.ACTIVE,
        createdAt = now,
        updatedAt = now,
        ..
      }

validateRequest :: Id DM.Merchant -> DSelectReq -> Flow (DM.Merchant, DEst.Estimate)
validateRequest merchantId sReq = do
  merchant <- QMerch.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  estimate <- QEst.findById sReq.estimateId >>= fromMaybeM (EstimateDoesNotExist sReq.estimateId.getId)
  return (merchant, estimate)
