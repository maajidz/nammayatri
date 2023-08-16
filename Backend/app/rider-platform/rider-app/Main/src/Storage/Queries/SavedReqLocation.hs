{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.SavedReqLocation where

import Domain.Types.Person (Person)
import Domain.Types.SavedReqLocation
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Logging (Log)
import qualified Sequelize as Se
import qualified Storage.Beam.SavedReqLocation as BeamSRL

create :: (L.MonadFlow m, Log m) => SavedReqLocation -> m ()
create = createWithKV

-- findAllByRiderId :: Transactionable m => Id Person -> m [SavedReqLocation]
-- findAllByRiderId perId =
--   Esq.findAll $ do
--     saveReqLocation <- from $ table @SavedReqLocationT
--     where_ $
--       saveReqLocation ^. SavedReqLocationRiderId ==. val (toKey perId)
--     orderBy [desc $ saveReqLocation ^. SavedReqLocationUpdatedAt]
--     return saveReqLocation

findAllByRiderId :: (L.MonadFlow m, Log m) => Id Person -> m [SavedReqLocation]
findAllByRiderId perId = findAllWithOptionsKV [Se.Is BeamSRL.riderId $ Se.Eq (getId perId)] (Se.Desc BeamSRL.updatedAt) Nothing Nothing

-- deleteByRiderIdAndTag :: Id Person -> Text -> SqlDB ()
-- deleteByRiderIdAndTag perId addressTag = do
--   Esq.delete $ do
--     saveReqLocation <- from $ table @SavedReqLocationT
--     where_ $
--       (saveReqLocation ^. SavedReqLocationRiderId ==. val (toKey perId))
--         &&. (saveReqLocation ^. SavedReqLocationTag ==. val addressTag)

deleteByRiderIdAndTag :: (L.MonadFlow m, Log m) => Id Person -> Text -> m ()
deleteByRiderIdAndTag perId addressTag = deleteWithKV [Se.And [Se.Is BeamSRL.riderId (Se.Eq (getId perId)), Se.Is BeamSRL.tag (Se.Eq addressTag)]]

-- findAllByRiderIdAndTag :: Transactionable m => Id Person -> Text -> m [SavedReqLocation]
-- findAllByRiderIdAndTag perId addressTag =
--   Esq.findAll $ do
--     saveReqLocation <- from $ table @SavedReqLocationT
--     where_ $
--       (saveReqLocation ^. SavedReqLocationRiderId ==. val (toKey perId))
--         &&. (saveReqLocation ^. SavedReqLocationTag ==. val addressTag)
--     return saveReqLocation

findAllByRiderIdAndTag :: (L.MonadFlow m, Log m) => Id Person -> Text -> m [SavedReqLocation]
findAllByRiderIdAndTag perId addressTag = findAllWithKV [Se.And [Se.Is BeamSRL.riderId (Se.Eq (getId perId)), Se.Is BeamSRL.tag (Se.Eq addressTag)]]

-- findByLatLonAndRiderId :: Transactionable m => Id Person -> LatLong -> m (Maybe SavedReqLocation)
-- findByLatLonAndRiderId personId LatLong {..} = Esq.findOne $ do
--   saveReqLocation <- from $ table @SavedReqLocationT
--   where_ $
--     saveReqLocation ^. SavedReqLocationLat ==. val lat
--       &&. saveReqLocation ^. SavedReqLocationLon ==. val lon
--       &&. saveReqLocation ^. SavedReqLocationRiderId ==. val (toKey personId)
--   return saveReqLocation

findByLatLonAndRiderId :: (L.MonadFlow m, Log m) => Id Person -> LatLong -> m (Maybe SavedReqLocation)
findByLatLonAndRiderId personId LatLong {..} = findOneWithKV [Se.And [Se.Is BeamSRL.lat (Se.Eq lat), Se.Is BeamSRL.lon (Se.Eq lon), Se.Is BeamSRL.riderId (Se.Eq (getId personId))]]

-- deleteAllByRiderId :: Id Person -> SqlDB ()
-- deleteAllByRiderId personId = do
--   Esq.delete $ do
--     saveReqLocation <- from $ table @SavedReqLocationT
--     where_ (saveReqLocation ^. SavedReqLocationRiderId ==. val (toKey personId))

-- countAllByRiderId :: Transactionable m => Id Person -> m Int
-- countAllByRiderId perId =
--   fromMaybe 0
--     <$> Esq.findAll
--       ( do
--           saveReqLocation <- from $ table @SavedReqLocationT
--           where_ $ saveReqLocation ^. SavedReqLocationRiderId ==. val (toKey perId)
--           return (countRows :: SqlExpr (Esq.Value Int))
--       )

countAllByRiderId :: (L.MonadFlow m, Log m) => Id Person -> m Int
countAllByRiderId perId = findAllWithKV [Se.Is BeamSRL.riderId $ Se.Eq (getId perId)] <&> length

deleteAllByRiderId :: (L.MonadFlow m, Log m) => Id Person -> m ()
deleteAllByRiderId personId = deleteWithKV [Se.Is BeamSRL.riderId (Se.Eq (getId personId))]

instance FromTType' BeamSRL.SavedReqLocation SavedReqLocation where
  fromTType' BeamSRL.SavedReqLocationT {..} = do
    pure $
      Just
        SavedReqLocation
          { id = Id id,
            lat = lat,
            lon = lon,
            street = street,
            door = door,
            city = city,
            state = state,
            country = country,
            building = building,
            areaCode = areaCode,
            area = area,
            createdAt = createdAt,
            updatedAt = updatedAt,
            tag = tag,
            riderId = Id riderId,
            placeId = placeId,
            ward = ward,
            isMoved = isMoved
          }

instance ToTType' BeamSRL.SavedReqLocation SavedReqLocation where
  toTType' SavedReqLocation {..} = do
    BeamSRL.SavedReqLocationT
      { BeamSRL.id = getId id,
        BeamSRL.lat = lat,
        BeamSRL.lon = lon,
        BeamSRL.street = street,
        BeamSRL.door = door,
        BeamSRL.city = city,
        BeamSRL.state = state,
        BeamSRL.country = country,
        BeamSRL.building = building,
        BeamSRL.areaCode = areaCode,
        BeamSRL.area = area,
        BeamSRL.createdAt = createdAt,
        BeamSRL.updatedAt = updatedAt,
        BeamSRL.tag = tag,
        BeamSRL.riderId = getId riderId,
        BeamSRL.placeId = placeId,
        BeamSRL.ward = ward,
        BeamSRL.isMoved = isMoved
      }
