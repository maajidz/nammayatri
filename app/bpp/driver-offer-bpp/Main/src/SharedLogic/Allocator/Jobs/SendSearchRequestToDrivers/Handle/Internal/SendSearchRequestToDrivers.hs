module SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.SendSearchRequestToDrivers
  ( sendSearchRequestToDrivers,
  )
where

import qualified Data.Map as M
import qualified Domain.Types.Driver.DriverFlowStatus as DDFS
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.SearchRequest.SearchReqLocation as DLoc
import Domain.Types.SearchRequestForDriver
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (addUTCTime, logInfo)
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Config (HasSendSearchRequestJobConfig)
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers.Handle.Internal.DriverPool (getPoolBatchNum)
import SharedLogic.DriverPool
import qualified SharedLogic.DriverPool.Config as DP
import SharedLogic.GoogleTranslate
import Storage.CachedQueries.CacheConfig (CacheFlow)
import qualified Storage.Queries.Driver.DriverFlowStatus as QDFS
import qualified Storage.Queries.SearchRequestForDriver as QSRD
import Tools.Maps as Maps
import qualified Tools.Notifications as Notify

type LanguageDictionary = M.Map Maps.Language DSearchReq.SearchRequest

sendSearchRequestToDrivers ::
  ( Log m,
    EsqDBFlow m r,
    TranslateFlow m r,
    CacheFlow m r,
    DP.HasDriverPoolConfig r,
    EncFlow m r,
    Redis.HedisFlow m r,
    HasSendSearchRequestJobConfig r
  ) =>
  DSR.SearchRequest ->
  Money ->
  Money ->
  Money ->
  [DriverPoolWithActualDistResult] ->
  m ()
sendSearchRequestToDrivers searchReq baseFare driverMinExtraFee driverMaxExtraFee driverPool = do
  logInfo $ "Send search requests to driver pool batch-" <> show driverPool
  validTill <- getSearchRequestValidTill
  batchNumber <- getPoolBatchNum searchReq.id
  searchRequestsForDrivers <- mapM (buildSearchRequestForDriver batchNumber searchReq baseFare validTill driverMinExtraFee driverMaxExtraFee) driverPool
  languageDictionary <- foldM (addLanguageToDictionary searchReq) M.empty driverPool
  let driverPoolZipSearchRequests = zip driverPool searchRequestsForDrivers
  Esq.runTransaction $ do
    QSRD.setInactiveByRequestId searchReq.id -- inactive previous request by drivers so that they can make new offers.
    QSRD.createMany searchRequestsForDrivers
    forM_ driverPoolZipSearchRequests $ \(_, sReqFD) -> do
      QDFS.updateStatus sReqFD.driverId DDFS.GOT_SEARCH_REQUEST {requestId = sReqFD.searchRequestId, validTill = sReqFD.searchRequestValidTill}

  forM_ driverPoolZipSearchRequests $ \(dPoolRes, sReqFD) -> do
    incrementTotalQuotesCount searchReq.providerId sReqFD.driverId searchReq.id validTill
    let language = fromMaybe Maps.ENGLISH dPoolRes.driverPoolResult.language
    let translatedSearchReq = fromMaybe searchReq $ M.lookup language languageDictionary
    let entityData = makeSearchRequestForDriverAPIEntity sReqFD translatedSearchReq dPoolRes.rideRequestPopupDelayDuration
    Notify.notifyOnNewSearchRequestAvailable searchReq.providerId sReqFD.driverId dPoolRes.driverPoolResult.driverDeviceToken entityData
  where
    getSearchRequestValidTill = do
      now <- getCurrentTime
      singleBatchProcessTime <- fromIntegral <$> asks (.sendSearchRequestJobCfg.singleBatchProcessTime)
      return $ singleBatchProcessTime `addUTCTime` now
    buildSearchRequestForDriver ::
      (MonadFlow m) =>
      Int ->
      DSearchReq.SearchRequest ->
      Money ->
      UTCTime ->
      Money ->
      Money ->
      DriverPoolWithActualDistResult ->
      m SearchRequestForDriver
    buildSearchRequestForDriver batchNumber searchRequest baseFare_ validTill driverMinExtraCharge driverMaxExtraCharge dpwRes = do
      guid <- generateGUID
      now <- getCurrentTime
      let dpRes = dpwRes.driverPoolResult
      let searchRequestForDriver =
            SearchRequestForDriver
              { id = guid,
                searchRequestId = searchRequest.id,
                startTime = searchRequest.startTime,
                searchRequestValidTill = validTill,
                driverId = cast dpRes.driverId,
                vehicleVariant = dpRes.variant,
                actualDistanceToPickup = dpwRes.actualDistanceToPickup,
                straightLineDistanceToPickup = dpRes.distanceToPickup,
                durationToPickup = dpwRes.actualDurationToPickup,
                status = Active,
                lat = Just dpRes.lat,
                lon = Just dpRes.lon,
                baseFare = baseFare_,
                createdAt = now,
                response = Nothing,
                driverMinExtraFee = driverMinExtraCharge,
                driverMaxExtraFee = driverMaxExtraCharge,
                rideRequestPopupDelayDuration = dpwRes.rideRequestPopupDelayDuration,
                isPartOfIntelligentPool = dpwRes.isPartOfIntelligentPool,
                acceptanceRatio = dpwRes.acceptanceRatio,
                cancellationRatio = dpwRes.cancellationRatio,
                driverAvailableTime = dpwRes.driverAvailableTime,
                parallelSearchRequestCount = Just dpwRes.driverPoolResult.parallelSearchRequestCount,
                ..
              }
      pure searchRequestForDriver

buildTranslatedSearchReqLocation :: (TranslateFlow m r, EsqDBFlow m r, CacheFlow m r) => DLoc.SearchReqLocation -> Maybe Maps.Language -> m DLoc.SearchReqLocation
buildTranslatedSearchReqLocation DLoc.SearchReqLocation {..} mbLanguage = do
  areaRegional <- case mbLanguage of
    Nothing -> return area
    Just lang -> do
      mAreaObj <- translate ENGLISH lang `mapM` area
      let translation = (\areaObj -> listToMaybe areaObj._data.translations) =<< mAreaObj
      return $ (.translatedText) <$> translation
  pure
    DLoc.SearchReqLocation
      { area = areaRegional,
        ..
      }

translateSearchReq ::
  ( TranslateFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  DSearchReq.SearchRequest ->
  Maps.Language ->
  m DSearchReq.SearchRequest
translateSearchReq DSearchReq.SearchRequest {..} language = do
  from <- buildTranslatedSearchReqLocation fromLocation (Just language)
  to <- buildTranslatedSearchReqLocation toLocation (Just language)
  pure
    DSearchReq.SearchRequest
      { fromLocation = from,
        toLocation = to,
        ..
      }

addLanguageToDictionary ::
  ( TranslateFlow m r,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  DSearchReq.SearchRequest ->
  LanguageDictionary ->
  DriverPoolWithActualDistResult ->
  m LanguageDictionary
addLanguageToDictionary searchReq dict dPoolRes = do
  let language = fromMaybe Maps.ENGLISH dPoolRes.driverPoolResult.language
  if isJust $ M.lookup language dict
    then return dict
    else do
      translatedSearchReq <- translateSearchReq searchReq language
      pure $ M.insert language translatedSearchReq dict
