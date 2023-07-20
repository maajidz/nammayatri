{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.DriverQuote where

import qualified Data.Time as T
import Domain.Types.DriverQuote
import qualified Domain.Types.DriverQuote as Domain
import Domain.Types.Person
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common (addUTCTime, secondsToNominalDiffTime)
import Lib.Utils (FromTType' (fromTType'), ToTType' (toTType'), createWithKV, deleteWithKV, findAllWithKV, findAllWithKvInReplica, findOneWithKV, findOneWithKvInReplica, updateWithKV)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverQuote as BeamDQ
import Storage.Queries.FareParameters as BeamQFP
import qualified Storage.Queries.FareParameters as SQFP

-- create :: Domain.DriverQuote -> SqlDB ()
-- create dQuote = Esq.runTransaction $
--   withFullEntity dQuote $ \(dQuoteT, (fareParams', fareParamsDetais)) -> do
--     Esq.create' fareParams'
--     case fareParamsDetais of
--       FareParamsT.ProgressiveDetailsT fppdt -> Esq.create' fppdt
--       FareParamsT.SlabDetailsT -> return ()
--     Esq.create' dQuoteT

create :: (L.MonadFlow m, Log m) => Domain.DriverQuote -> m ()
create dQuote = SQFP.create dQuote.fareParams >> createWithKV dQuote

-- baseDriverQuoteQuery ::
--   From
--     ( SqlExpr (Entity DriverQuoteT)
--         :& SqlExpr (Entity Fare.FareParametersT)
--     )
-- baseDriverQuoteQuery =
--   table @DriverQuoteT
--     `innerJoin` table @Fare.FareParametersT
--       `Esq.on` ( \(rb :& farePars) ->
--                    rb ^. DriverQuoteFareParametersId ==. farePars ^. Fare.FareParametersTId
--                )

findById :: (L.MonadFlow m, Log m) => Id Domain.DriverQuote -> m (Maybe Domain.DriverQuote)
findById (Id driverQuoteId) = findOneWithKV [Se.Is BeamDQ.id $ Se.Eq driverQuoteId]

findByIdInReplica :: (L.MonadFlow m, Log m) => Id Domain.DriverQuote -> m (Maybe Domain.DriverQuote)
findByIdInReplica (Id driverQuoteId) = findOneWithKvInReplica [Se.Is BeamDQ.id $ Se.Eq driverQuoteId]

setInactiveBySTId :: (L.MonadFlow m, Log m) => Id DST.SearchTry -> m ()
setInactiveBySTId (Id searchTryId) = updateWithKV [Se.Set BeamDQ.status Domain.Inactive] [Se.Is BeamDQ.searchTryId $ Se.Eq searchTryId]

-- setInactiveBySRId :: Id DSR.SearchRequest -> SqlDB ()
-- setInactiveBySRId searchReqId = Esq.update $ \p -> do
--   set p [DriverQuoteStatus =. val Domain.Inactive]
--   where_ $ p ^. DriverQuoteRequestId ==. val (toKey searchReqId)

setInactiveBySRId :: (L.MonadFlow m, Log m) => Id DSR.SearchRequest -> m ()
setInactiveBySRId (Id searchReqId) = updateWithKV [Se.Set BeamDQ.status Domain.Inactive] [Se.Is BeamDQ.requestId $ Se.Eq searchReqId]

findActiveQuotesByDriverId :: (L.MonadFlow m, MonadTime m, Log m) => Id Person -> Seconds -> m [Domain.DriverQuote]
findActiveQuotesByDriverId (Id driverId) driverUnlockDelay = do
  now <- getCurrentTime
  let delayToAvoidRaces = secondsToNominalDiffTime . negate $ driverUnlockDelay
  findAllWithKV [Se.And [Se.Is BeamDQ.status $ Se.Eq Domain.Active, Se.Is BeamDQ.id $ Se.Eq driverId, Se.Is BeamDQ.validTill $ Se.GreaterThan (T.utcToLocalTime T.utc $ addUTCTime delayToAvoidRaces now)]]

findDriverQuoteBySTId :: (L.MonadFlow m, Log m) => Id DST.SearchTry -> m (Maybe Domain.DriverQuote)
findDriverQuoteBySTId (Id searchTryId) = findOneWithKV [Se.Is BeamDQ.searchTryId $ Se.Eq searchTryId]

deleteByDriverId :: (L.MonadFlow m, Log m) => Id Person -> m ()
deleteByDriverId (Id driverId) = deleteWithKV [Se.Is BeamDQ.driverId (Se.Eq driverId)]

findAllBySTId :: (L.MonadFlow m, Log m) => Id DST.SearchTry -> m [Domain.DriverQuote]
findAllBySTId (Id searchTryId) = findAllWithKV [Se.And [Se.Is BeamDQ.searchTryId $ Se.Eq searchTryId, Se.Is BeamDQ.status $ Se.Eq Domain.Active]]

countAllBySTId :: (L.MonadFlow m, Log m) => Id DST.SearchTry -> m Int
countAllBySTId searchTId = findAllWithKV [Se.And [Se.Is BeamDQ.searchTryId $ Se.Eq (getId searchTId)]] <&> length

countAllBySTIdInReplica :: (L.MonadFlow m, Log m) => Id DST.SearchTry -> m Int
countAllBySTIdInReplica searchTId = findAllWithKvInReplica [Se.And [Se.Is BeamDQ.searchTryId $ Se.Eq (getId searchTId)]] <&> length

instance FromTType' BeamDQ.DriverQuote DriverQuote where
  fromTType' BeamDQ.DriverQuoteT {..} = do
    fp <- BeamQFP.findById (Id fareParametersId)
    if isJust fp
      then
        pure $
          Just
            Domain.DriverQuote
              { id = Id id,
                requestId = Id requestId,
                searchTryId = Id searchTryId,
                searchRequestForDriverId = Id <$> searchRequestForDriverId,
                driverId = Id driverId,
                driverName = driverName,
                driverRating = driverRating,
                status = status,
                vehicleVariant = vehicleVariant,
                distance = distance,
                distanceToPickup = distanceToPickup,
                durationToPickup = durationToPickup,
                createdAt = T.localTimeToUTC T.utc createdAt,
                updatedAt = T.localTimeToUTC T.utc updatedAt,
                validTill = T.localTimeToUTC T.utc validTill,
                estimatedFare = estimatedFare,
                fareParams = fromJust fp, -- this should take a default value?
                providerId = Id providerId,
                specialLocationTag = specialLocationTag
              }
      else pure Nothing

transformBeamDriverQuoteToDomain :: (L.MonadFlow m, Log m) => BeamDQ.DriverQuote -> m (Maybe Domain.DriverQuote)
transformBeamDriverQuoteToDomain BeamDQ.DriverQuoteT {..} = do
  fp <- BeamQFP.findById (Id fareParametersId)
  if isJust fp
    then
      pure $
        Just
          Domain.DriverQuote
            { id = Id id,
              requestId = Id requestId,
              searchTryId = Id searchTryId,
              searchRequestForDriverId = Id <$> searchRequestForDriverId,
              driverId = Id driverId,
              driverName = driverName,
              driverRating = driverRating,
              status = status,
              vehicleVariant = vehicleVariant,
              distance = distance,
              distanceToPickup = distanceToPickup,
              durationToPickup = durationToPickup,
              createdAt = T.localTimeToUTC T.utc createdAt,
              updatedAt = T.localTimeToUTC T.utc updatedAt,
              validTill = T.localTimeToUTC T.utc validTill,
              estimatedFare = estimatedFare,
              fareParams = fromJust fp, -- this should take a default value?
              providerId = Id providerId,
              specialLocationTag = specialLocationTag
            }
    else pure Nothing

instance ToTType' BeamDQ.DriverQuote DriverQuote where
  toTType' DriverQuote {..} = do
    BeamDQ.DriverQuoteT
      { BeamDQ.id = getId id,
        BeamDQ.requestId = getId requestId,
        BeamDQ.searchTryId = getId searchTryId,
        BeamDQ.searchRequestForDriverId = getId <$> searchRequestForDriverId,
        BeamDQ.driverId = getId driverId,
        BeamDQ.driverName = driverName,
        BeamDQ.driverRating = driverRating,
        BeamDQ.status = status,
        BeamDQ.vehicleVariant = vehicleVariant,
        BeamDQ.distance = distance,
        BeamDQ.distanceToPickup = distanceToPickup,
        BeamDQ.durationToPickup = durationToPickup,
        BeamDQ.createdAt = T.utcToLocalTime T.utc createdAt,
        BeamDQ.updatedAt = T.utcToLocalTime T.utc updatedAt,
        BeamDQ.validTill = T.utcToLocalTime T.utc validTill,
        BeamDQ.estimatedFare = estimatedFare,
        BeamDQ.fareParametersId = getId fareParams.id,
        BeamDQ.providerId = getId providerId,
        BeamDQ.specialLocationTag = specialLocationTag
      }
