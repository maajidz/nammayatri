{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Driver.GoHomeFeature.DriverHomeLocation where

import Domain.Types.Driver.GoHomeFeature.DriverHomeLocation
-- import qualified EulerHS.Language as L
import qualified Domain.Types.Driver.GoHomeFeature.DriverHomeLocation as Domain
import Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.App (MonadFlow)
import Kernel.Types.Id (Id (..))
import Lib.Utils ()
import qualified Sequelize as Se
import Storage.Beam.Driver.GoHomeFeature.DriverHomeLocation as BeamDHL

create :: MonadFlow m => Domain.DriverHomeLocation -> m ()
create = createWithKV

-- findById ::
--   Transactionable m =>
--   Id DriverHomeLocation ->
--   m (Maybe DriverHomeLocation)
-- findById = Esq.findById

findById :: MonadFlow m => Id Domain.DriverHomeLocation -> m (Maybe Domain.DriverHomeLocation)
findById (Kernel.Types.Id.Id driverHomeLocId) = findOneWithKV [Se.Is BeamDHL.id $ Se.Eq driverHomeLocId]

-- findAllByDriverId :: Transactionable m => Id Driver -> m [DriverHomeLocation]
-- findAllByDriverId driverId = do
--   Esq.findAll $ do
--     driverHomeLocation <- from $ table @DriverHomeLocationT
--     where_ $ driverHomeLocation ^. DriverHomeLocationDriverId ==. val (toKey driverId)
--     return driverHomeLocation

findAllByDriverId :: MonadFlow m => Id Driver -> m [Domain.DriverHomeLocation]
findAllByDriverId (Kernel.Types.Id.Id driverId) = findAllWithKV [Se.Is BeamDHL.driverId $ Se.Eq driverId]

-- deleteById :: Id DriverHomeLocation -> SqlDB ()
-- deleteById = deleteByKey @DriverHomeLocationT

deleteById :: MonadFlow m => Id Domain.DriverHomeLocation -> m ()
deleteById (Kernel.Types.Id.Id driverHomeLocId) = deleteWithKV [Se.Is BeamDHL.id $ Se.Eq driverHomeLocId]

-- deleteByDriverId :: Id Driver -> SqlDB ()
-- deleteByDriverId driverId =
--   Esq.delete $ do
--     driverHomeLocation <- from $ table @DriverHomeLocationT
--     where_ $ driverHomeLocation ^. DriverHomeLocationDriverId ==. val (toKey $ cast driverId)

deleteByDriverId :: MonadFlow m => Id Driver -> m ()
deleteByDriverId (Kernel.Types.Id.Id driverId) = deleteWithKV [Se.Is BeamDHL.driverId $ Se.Eq driverId]

instance FromTType' BeamDHL.DriverHomeLocation Domain.DriverHomeLocation where
  fromTType' BeamDHL.DriverHomeLocationT {..} = do
    pure $
      Just
        Domain.DriverHomeLocation
          { id = Kernel.Types.Id.Id id,
            driverId = Kernel.Types.Id.Id driverId,
            ..
          }

instance ToTType' BeamDHL.DriverHomeLocation Domain.DriverHomeLocation where
  toTType' Domain.DriverHomeLocation {..} =
    BeamDHL.DriverHomeLocationT
      { id = id.getId,
        driverId = getId driverId,
        ..
      }
