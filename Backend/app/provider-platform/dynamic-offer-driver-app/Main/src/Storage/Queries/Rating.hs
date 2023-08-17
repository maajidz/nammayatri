{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Rating where

import qualified Database.Beam as B
import Domain.Types.Person
import Domain.Types.Rating as DR
import Domain.Types.Ride
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Beam.Rating as BeamR

create :: (L.MonadFlow m, Log m) => DR.Rating -> m ()
create = createWithKV

updateRating :: (L.MonadFlow m, MonadTime m, Log m) => Id Rating -> Id Person -> Int -> Maybe Text -> m ()
updateRating (Id ratingId) (Id driverId) newRatingValue newFeedbackDetails = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamR.ratingValue newRatingValue,
      Se.Set BeamR.feedbackDetails newFeedbackDetails,
      Se.Set BeamR.updatedAt now
    ]
    [Se.And [Se.Is BeamR.id (Se.Eq ratingId), Se.Is BeamR.driverId (Se.Eq driverId)]]

findAllRatingsForPerson :: (L.MonadFlow m, Log m) => Id Person -> m [Rating]
findAllRatingsForPerson driverId = findAllWithDb [Se.Is BeamR.driverId $ Se.Eq $ getId driverId]

findRatingForRide :: (L.MonadFlow m, Log m) => Id Ride -> m (Maybe Rating)
findRatingForRide (Id rideId) = findOneWithKV [Se.Is BeamR.rideId $ Se.Eq rideId]

-- findAllRatingUsersCountByPerson :: (L.MonadFlow m, Log m) => Id Person -> m Int
-- findAllRatingUsersCountByPerson (Id driverId) = findAllWithKV [Se.Is BeamR.driverId $ Se.Eq driverId] <&> length

findAllRatingUsersCountByPerson :: (L.MonadFlow m, Log m) => Id Person -> m Int
findAllRatingUsersCountByPerson (Id driverId) = do
  dbConf <- getMasterBeamConfig
  res <- L.runDB dbConf $
    L.findRows $
      B.select $
        B.aggregate_ (\_ -> B.as_ @Int B.countAll_) $
          B.filter_'
            (\rating' -> BeamR.driverId rating' B.==?. B.val_ driverId)
            do
              B.all_ (BeamCommon.rating BeamCommon.atlasDB)
  pure $ either (const 0) (\r -> if null r then 0 else head r) res

instance FromTType' BeamR.Rating Rating where
  fromTType' BeamR.RatingT {..} = do
    pure $
      Just
        Rating
          { id = Id id,
            rideId = Id rideId,
            driverId = Id driverId,
            ratingValue = ratingValue,
            feedbackDetails = feedbackDetails,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamR.Rating Rating where
  toTType' Rating {..} = do
    BeamR.RatingT
      { BeamR.id = getId id,
        BeamR.rideId = getId rideId,
        BeamR.driverId = getId driverId,
        BeamR.ratingValue = ratingValue,
        BeamR.feedbackDetails = feedbackDetails,
        BeamR.createdAt = createdAt,
        BeamR.updatedAt = updatedAt
      }
