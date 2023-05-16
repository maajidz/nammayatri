{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Vehicle where

-- import qualified Data.Text as T
import qualified Debug.Trace as T
import Domain.Types.Merchant
import Domain.Types.Person
import Domain.Types.Vehicle
import qualified Domain.Types.Vehicle.Variant as Variant
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Sequelize as Se
import qualified Storage.Tabular.VechileNew as VN
import Storage.Tabular.Vehicle

create :: Vehicle -> SqlDB ()
create = Esq.create

upsert :: Vehicle -> SqlDB ()
upsert a@Vehicle {..} =
  Esq.upsert
    a
    [ VehicleDriverId =. val (toKey driverId),
      VehicleCapacity =. val capacity,
      VehicleCategory =. val category,
      VehicleMake =. val make,
      VehicleModel =. val model,
      VehicleSize =. val size,
      VehicleVariant =. val variant,
      VehicleColor =. val color,
      VehicleEnergyType =. val energyType,
      VehicleRegistrationCategory =. val registrationCategory,
      VehicleUpdatedAt =. val updatedAt
    ]

findById ::
  (MonadFlow m) =>
  Id Person ->
  m (Maybe Vehicle)
findById (Id driverId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformVechileNewToVechile <$>) <$> KV.findWithKVConnector dbCOnf' VN.meshConfig [Se.Is VN.driverId $ Se.Eq driverId]
    Nothing -> pure Nothing

updateVehicleRec :: Vehicle -> SqlDB ()
updateVehicleRec vehicle = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ VehicleCapacity =. val vehicle.capacity,
        VehicleCategory =. val vehicle.category,
        VehicleMake =. val vehicle.make,
        VehicleModel =. val vehicle.model,
        VehicleSize =. val vehicle.size,
        VehicleVariant =. val vehicle.variant,
        VehicleColor =. val vehicle.color,
        VehicleEnergyType =. val vehicle.energyType,
        VehicleRegistrationNo =. val vehicle.registrationNo,
        VehicleRegistrationCategory =. val vehicle.registrationCategory,
        VehicleUpdatedAt =. val now
      ]
    where_ $ tbl ^. VehicleTId ==. val (toKey vehicle.driverId)

deleteById :: Id Person -> SqlDB ()
deleteById = Esq.deleteByKey @VehicleT

findByAnyOf :: Transactionable m => Maybe Text -> Maybe (Id Person) -> m (Maybe Vehicle)
findByAnyOf registrationNoM vehicleIdM =
  Esq.findOne $ do
    vehicle <- from $ table @VehicleT
    where_ $
      whenJust_ vehicleIdM (\vehicleId -> vehicle ^. VehicleTId ==. val (toKey vehicleId))
        &&. whenJust_ registrationNoM (\regNum -> vehicle ^. VehicleRegistrationNo ==. val regNum)
    return vehicle

findAllByVariantRegNumMerchantId ::
  Transactionable m =>
  Maybe Variant.Variant ->
  Maybe Text ->
  Integer ->
  Integer ->
  Id Merchant ->
  m [Vehicle]
findAllByVariantRegNumMerchantId variantM mbRegNum limit' offset' merchantId = do
  let limitVal = fromIntegral limit'
      offsetVal = fromIntegral offset'
  Esq.findAll $ do
    vehicle <- from $ table @VehicleT
    where_ $
      vehicle ^. VehicleMerchantId ==. val (toKey merchantId)
        &&. whenJust_ variantM (\variant -> vehicle ^. VehicleVariant ==. val variant)
        &&. whenJust_ mbRegNum (\regNum -> vehicle ^. VehicleRegistrationNo `ilike` (%) ++. val regNum ++. (%))
    orderBy [desc $ vehicle ^. VehicleCreatedAt]
    limit limitVal
    offset offsetVal
    return vehicle

findByRegistrationNo ::
  Transactionable m =>
  Text ->
  m (Maybe Vehicle)
findByRegistrationNo registrationNo =
  Esq.findOne $ do
    vehicle <- from $ table @VehicleT
    where_ $ vehicle ^. VehicleRegistrationNo ==. val registrationNo
    return vehicle

transformVechileNewToVechile :: VN.VechileNew -> Vehicle
transformVechileNewToVechile VN.VechileNew {..} =
  Vehicle
    { driverId = Id driverId,
      merchantId = Id merchantId,
      variant = variant,
      model = model,
      color = color,
      registrationNo = registrationNo,
      capacity = capacity,
      category = category,
      make = make,
      size = size,
      energyType = energyType,
      registrationCategory = registrationCategory,
      vehicleClass = vehicleClass,
      createdAt = createdAt,
      updatedAt = updatedAt
    }
