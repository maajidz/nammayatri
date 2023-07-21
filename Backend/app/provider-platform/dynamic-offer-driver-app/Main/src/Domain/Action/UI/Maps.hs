{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Maps
  ( Maps.GetPlaceNameReq,
    Maps.GetPlaceNameResp,
    getPlaceName,
  )
where

import qualified Data.Geohash as DG
import Data.Text (pack)
import Domain.Types.Maps.PlaceNameCache as DTM
import qualified Domain.Types.Merchant as DMerchant
import qualified Kernel.External.Maps.Interface.Types as MIT
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Maps.PlaceNameCache as CM
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Tools.Maps as Maps
import Tools.Metrics (CoreMetrics)

getPlaceName :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r, CoreMetrics m) => Id DMerchant.Merchant -> Maps.GetPlaceNameReq -> m Maps.GetPlaceNameResp
getPlaceName merchantId req = do
  merchant <- QMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  case req.getBy of
    MIT.ByLatLong (Maps.LatLong lat lon) -> do
      let myGeohash = DG.encode merchant.geoHashPrecisionValue (lat, lon)
      case myGeohash of
        Just geoHash -> do
          placeNameCache <- CM.findPlaceByGeoHash (pack geoHash)
          if null placeNameCache
            then callMapsApi merchantId req merchant.geoHashPrecisionValue
            else pure $ map convertToGetPlaceNameResp placeNameCache
        Nothing -> callMapsApi merchantId req merchant.geoHashPrecisionValue
    MIT.ByPlaceId placeId -> do
      placeNameCache <- CM.findPlaceByPlaceId placeId
      if null placeNameCache
        then callMapsApi merchantId req merchant.geoHashPrecisionValue
        else pure $ map convertToGetPlaceNameResp placeNameCache

callMapsApi :: (EncFlow m r, EsqDBFlow m r, CacheFlow m r, CoreMetrics m) => Id DMerchant.Merchant -> Maps.GetPlaceNameReq -> Int -> m Maps.GetPlaceNameResp
callMapsApi merchantId req geoHashPrecisionValue = do
  res <- Maps.getPlaceName merchantId req
  let firstElement = listToMaybe res
  case firstElement of
    Just element -> do
      let (latitude, longitude) = case req.getBy of
            MIT.ByLatLong (Maps.LatLong lat lon) -> (lat, lon)
            _ -> (element.location.lat, element.location.lon)
      placeNameCache <- convertResultsRespToPlaceNameCache element latitude longitude geoHashPrecisionValue
      Esq.runTransaction $ CM.create placeNameCache
      case (placeNameCache.placeId, placeNameCache.geoHash) of
        (Just placeId, Just geoHash) -> do
          CM.cachedPlaceByPlaceId placeId [placeNameCache]
          CM.cachedPlaceByGeoHash geoHash [placeNameCache]
        (Just placeId, Nothing) -> do
          CM.cachedPlaceByPlaceId placeId [placeNameCache]
        (Nothing, Just geoHash) -> do
          CM.cachedPlaceByGeoHash geoHash [placeNameCache]
        _ -> pure ()
    Nothing -> pure ()
  return res

convertToGetPlaceNameResp :: PlaceNameCache -> Maps.PlaceName
convertToGetPlaceNameResp placeNameCache =
  MIT.PlaceName
    { formattedAddress = placeNameCache.formattedAddress,
      addressComponents = map (\DTM.AddressResp {..} -> MIT.AddressResp {..}) placeNameCache.addressComponents,
      plusCode = placeNameCache.plusCode,
      location = LatLong {lat = placeNameCache.lat, lon = placeNameCache.lon},
      placeId = placeNameCache.placeId
    }

convertResultsRespToPlaceNameCache :: MonadFlow m => MIT.PlaceName -> Double -> Double -> Int -> m DTM.PlaceNameCache
convertResultsRespToPlaceNameCache resultsResp latitude longitude geoHashPrecisionValue = do
  id <- generateGUID
  now <- getCurrentTime
  let res =
        DTM.PlaceNameCache
          { id,
            formattedAddress = resultsResp.formattedAddress,
            addressComponents = map (\MIT.AddressResp {..} -> DTM.AddressResp {..}) resultsResp.addressComponents,
            plusCode = resultsResp.plusCode,
            lat = resultsResp.location.lat,
            lon = resultsResp.location.lon,
            placeId = resultsResp.placeId,
            geoHash = pack <$> DG.encode geoHashPrecisionValue (latitude, longitude),
            createdAt = now
          }
  return res
