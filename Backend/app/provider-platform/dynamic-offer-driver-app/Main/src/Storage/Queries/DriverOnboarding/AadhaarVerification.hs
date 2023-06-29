{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.DriverOnboarding.AadhaarVerification where

import Domain.Types.DriverOnboarding.AadhaarVerification
import Domain.Types.Person (Person)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Storage.Beam.DriverOnboarding.AadhaarVerification as BeamAV
import Storage.Tabular.DriverOnboarding.AadhaarVerification

create :: AadhaarVerification -> Esq.SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id AadhaarVerification ->
  m (Maybe AadhaarVerification)
findById = Esq.findById

findByDriverId ::
  Transactionable m =>
  Id Person ->
  m (Maybe AadhaarVerification)
findByDriverId driverId = do
  findOne $ do
    aadhaar <- from $ table @AadhaarVerificationT
    where_ $ aadhaar ^. AadhaarVerificationDriverId ==. val (toKey driverId)
    return aadhaar

transformBeamAadhaarVerificationToDomain :: BeamAV.AadhaarVerification -> AadhaarVerification
transformBeamAadhaarVerificationToDomain BeamAV.AadhaarVerificationT {..} = do
  AadhaarVerification
    { id = Id id,
      driverId = Id driverId,
      driverName = driverName,
      driverGender = driverGender,
      driverDob = driverDob,
      driverImage = driverImage,
      createdAt = createdAt
    }

transformDomainAadhaarVerificationToBeam :: AadhaarVerification -> BeamAV.AadhaarVerification
transformDomainAadhaarVerificationToBeam AadhaarVerification {..} =
  BeamAV.defaultAadhaarVerification
    { BeamAV.id = getId id,
      BeamAV.driverId = getId driverId,
      BeamAV.driverName = driverName,
      BeamAV.driverGender = driverGender,
      BeamAV.driverDob = driverDob,
      BeamAV.driverImage = driverImage,
      BeamAV.createdAt = createdAt
    }
