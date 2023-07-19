{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.DriverStats where

import Domain.Types.DriverStats as Domain
import Domain.Types.Person (Driver)
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Utils
  ( FromTType' (fromTType'),
    ToTType' (toTType'),
    createWithKV,
    deleteWithKV,
    findAllWithKV,
    findAllWithOptionsKV,
    findOneWithKV,
    updateWithKV,
  )
import qualified Sequelize as Se
import qualified Storage.Beam.DriverStats as BeamDS

-- createInitialDriverStats :: Id Driver -> SqlDB ()
-- createInitialDriverStats driverId = do
--   now <- getCurrentTime
--   Esq.create $
--     DriverStats
--       { driverId = driverId,
--         idleSince = now,
--         totalRides = 0,
--         totalDistance = 0
--       }

-- create :: DriverStats -> SqlDB ()
-- create = Esq.create

create :: (L.MonadFlow m, Log m) => DriverStats -> m ()
create = createWithKV

createInitialDriverStats :: (L.MonadFlow m, MonadTime m, Log m) => Id Driver -> m ()
createInitialDriverStats driverId = do
  now <- getCurrentTime
  let dStats =
        DriverStats
          { driverId = driverId,
            idleSince = now,
            totalRides = 0,
            totalDistance = 0,
            ridesCancelled = Just 0,
            totalRidesAssigned = Just 0
          }
  createWithKV dStats

-- getTopDriversByIdleTime :: Transactionable m => Int -> [Id Driver] -> m [Id Driver]
-- getTopDriversByIdleTime count_ ids =
--   Esq.findAll $ do
--     driverStats <- from $ table @DriverStatsT
--     where_ $ driverStats ^. DriverStatsDriverId `in_` valList (toKey . cast <$> ids)
--     orderBy [asc $ driverStats ^. DriverStatsIdleSince]
--     limit $ fromIntegral count_
--     return $ driverStats ^. DriverStatsTId

getTopDriversByIdleTime :: (L.MonadFlow m, Log m) => Int -> [Id Driver] -> m [Id Driver]
getTopDriversByIdleTime count_ ids = findAllWithOptionsKV [Se.Is BeamDS.driverId $ Se.In (getId <$> ids)] (Se.Asc BeamDS.idleSince) (Just count_) Nothing <&> (Domain.driverId <$>)

-- updateIdleTime :: Id Driver -> SqlDB ()
-- updateIdleTime driverId = updateIdleTimes [driverId]

updateIdleTime :: (L.MonadFlow m, MonadTime m, Log m) => Id Driver -> m ()
updateIdleTime driverId = updateIdleTimes [driverId]

-- updateIdleTimes :: [Id Driver] -> SqlDB ()
-- updateIdleTimes driverIds = do
--   now <- getCurrentTime
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ DriverStatsIdleSince =. val now
--       ]
--     where_ $ tbl ^. DriverStatsDriverId `in_` valList (toKey . cast <$> driverIds)

updateIdleTimes :: (L.MonadFlow m, MonadTime m, Log m) => [Id Driver] -> m ()
updateIdleTimes driverIds = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamDS.idleSince now
    ]
    [Se.Is BeamDS.driverId (Se.In (getId <$> driverIds))]

-- fetchAll :: Transactionable m => m [DriverStats]
-- fetchAll = Esq.findAll $ from $ table @DriverStatsT

fetchAll :: (L.MonadFlow m, Log m) => m [DriverStats]
fetchAll = findAllWithKV [Se.Is BeamDS.driverId $ Se.Not $ Se.Eq $ getId ""]

-- findById :: Transactionable m => Id Driver -> m (Maybe DriverStats)
-- findById = Esq.findById

findById :: (L.MonadFlow m, Log m) => Id Driver -> m (Maybe DriverStats)
findById (Id driverId) = findOneWithKV [Se.Is BeamDS.driverId $ Se.Eq driverId]

-- deleteById :: Id Driver -> SqlDB ()
-- deleteById = Esq.deleteByKey @DriverStatsT

deleteById :: (L.MonadFlow m, Log m) => Id Driver -> m ()
deleteById (Id driverId) = deleteWithKV [Se.Is BeamDS.driverId (Se.Eq driverId)]

-- incrementTotalRidesAndTotalDist :: Id Driver -> Meters -> SqlDB ()
-- incrementTotalRidesAndTotalDist driverId rideDist = do
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ DriverStatsTotalRides =. (tbl ^. DriverStatsTotalRides) +. val 1,
--         DriverStatsTotalDistance =. (tbl ^. DriverStatsTotalDistance) +. val rideDist
--       ]
--     where_ $ tbl ^. DriverStatsDriverId ==. val (toKey $ cast driverId)

findTotalRides :: (L.MonadFlow m, Log m) => Id Driver -> m (Int, Meters)
findTotalRides (Id driverId) = maybe (pure (0, 0)) (pure . (Domain.totalRides &&& Domain.totalDistance)) =<< findOneWithKV [Se.Is BeamDS.driverId (Se.Eq driverId)]

incrementTotalRidesAndTotalDist :: (L.MonadFlow m, Log m) => Id Driver -> Meters -> m ()
incrementTotalRidesAndTotalDist (Id driverId') rideDist = do
  findTotalRides (Id driverId') >>= \(rides, distance) ->
    updateWithKV
      [ Se.Set (\BeamDS.DriverStatsT {..} -> totalRides) (rides + 1),
        Se.Set BeamDS.totalDistance (rideDist + distance)
      ]
      [Se.Is BeamDS.driverId (Se.Eq driverId')]

-- incrementTotalRidesAssigned :: Id Driver -> Int -> SqlDB ()
-- incrementTotalRidesAssigned driverId number = do
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ DriverStatsTotalRidesAssigned =. just (Esq.coalesceDefault [tbl ^. DriverStatsTotalRidesAssigned] (val 0) +. val number)
--       ]
--     where_ $ tbl ^. DriverStatsDriverId ==. val (toKey $ cast driverId)

findTotalRidesAssigned :: (L.MonadFlow m, Log m) => Id Driver -> m (Maybe Int)
findTotalRidesAssigned (Id driverId) = (Domain.totalRidesAssigned =<<) <$> findOneWithKV [Se.Is BeamDS.driverId (Se.Eq driverId)]

incrementTotalRidesAssigned :: (L.MonadFlow m, Log m) => Id Driver -> Int -> m ()
incrementTotalRidesAssigned (Id driverId') number = do
  findTotalRidesAssigned (Id driverId') >>= \case
    Nothing -> updateWithKV [Se.Set BeamDS.totalRidesAssigned (Just number)] [Se.Is BeamDS.driverId (Se.Eq driverId')]
    Just newRides -> do
      updateWithKV [Se.Set BeamDS.totalRidesAssigned (Just (newRides + number))] [Se.Is BeamDS.driverId (Se.Eq driverId')]

-- setCancelledRidesCount :: Id Driver -> Int -> SqlDB ()
-- setCancelledRidesCount driverId cancelledCount = do
--   Esq.update $ \tbl -> do
--     set
--       tbl
--       [ DriverStatsRidesCancelled =. val (Just cancelledCount)
--       ]
--     where_ $ tbl ^. DriverStatsDriverId ==. val (toKey $ cast driverId)

setCancelledRidesCount :: (L.MonadFlow m, Log m) => Id Driver -> Int -> m ()
setCancelledRidesCount (Id driverId') cancelledCount = updateWithKV [Se.Set BeamDS.ridesCancelled (Just cancelledCount)] [Se.Is BeamDS.driverId (Se.Eq driverId')]

-- getDriversSortedOrder :: Transactionable m => Maybe Integer -> m [DriverStats]
-- getDriversSortedOrder mbLimitVal =
--   Esq.findAll $ do
--     driverStats <- from $ table @DriverStatsT
--     orderBy [desc (driverStats ^. DriverStatsTotalRides), desc (driverStats ^. DriverStatsTotalDistance)]
--     limit $ maybe 10 fromIntegral mbLimitVal
--     return driverStats

getDriversSortedOrder :: (L.MonadFlow m, Log m) => Maybe Integer -> m [DriverStats]
getDriversSortedOrder mbLimitVal = findAllWithOptionsKV [] (Se.Desc BeamDS.totalRides) (Just $ maybe 10 fromInteger mbLimitVal) Nothing

instance FromTType' BeamDS.DriverStats DriverStats where
  fromTType' BeamDS.DriverStatsT {..} = do
    pure $
      Just
        DriverStats
          { driverId = Id driverId,
            idleSince = idleSince,
            totalRides = totalRides,
            totalDistance = totalDistance,
            ridesCancelled = ridesCancelled,
            totalRidesAssigned = totalRidesAssigned
          }

instance ToTType' BeamDS.DriverStats DriverStats where
  toTType' DriverStats {..} = do
    BeamDS.DriverStatsT
      { BeamDS.driverId = getId driverId,
        BeamDS.idleSince = idleSince,
        BeamDS.totalRides = totalRides,
        BeamDS.totalDistance = totalDistance,
        BeamDS.ridesCancelled = ridesCancelled,
        BeamDS.totalRidesAssigned = totalRidesAssigned
      }
