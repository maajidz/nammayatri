{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DriverPool.Config where

import Data.Map as HM hiding (foldr)
import Domain.Types.Merchant
import Domain.Types.Merchant.DriverPoolConfig
import qualified Domain.Types.Vehicle.Variant as Variant
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import Kernel.Utils.Error
import qualified Storage.CachedQueries.Merchant.DriverPoolConfig as CDP

data CancellationScoreRelatedConfig = CancellationScoreRelatedConfig
  { popupDelayToAddAsPenalty :: Maybe Seconds,
    thresholdCancellationScore :: Maybe Int,
    minRidesForCancellationScore :: Maybe Int
  }
  deriving (Generic)

getDriverPoolConfig ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id Merchant ->
  Variant.Variant ->
  Meters ->
  m DriverPoolConfig
getDriverPoolConfig merchantId vehicleVariant dist = do
  configs <- CDP.findAllByMerchantId merchantId
  let applicableConfig = find filterByDistAndDveh configs
  case configs of
    [] -> throwError $ InvalidRequest "DriverPoolConfig not found"
    (config : _) -> pure $ maybe config identity applicableConfig
  where
    filterByDistAndDveh cfg = dist >= cfg.tripDistance && cfg.vehicleVariant == vehicleVariant

getDriverPoolConfigs ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id Merchant ->
  Meters ->
  m [DriverPoolConfig]
getDriverPoolConfigs merchantId dist = do
  configs <- CDP.findAllByMerchantId merchantId
  let variantConfigsMap =
        foldr
          ( \config acc ->
              case HM.lookup config.vehicleVariant acc of
                Just ls -> HM.insert config.vehicleVariant (config : ls) acc
                Nothing -> HM.insert config.vehicleVariant [config] acc
          )
          HM.empty
          configs
  let applicableConfigs = catMaybes $ HM.elems $ HM.map (\variantConfigs -> find filterByDist variantConfigs) variantConfigsMap
  case applicableConfigs of
    [] -> throwError $ InvalidRequest "DriverPoolConfig not found"
    _ -> pure applicableConfigs
  where
    filterByDist cfg = dist >= cfg.tripDistance
