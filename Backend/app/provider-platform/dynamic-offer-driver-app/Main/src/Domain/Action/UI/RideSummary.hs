{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Action.UI.RideSummary where

-- import Data.List (groupBy)
import Data.Time (Day, UTCTime (UTCTime), addDays, addUTCTime, utctDay)
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as Ride
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Storage.Queries.Ride as QRide

data DriverRideSummaryResp = DriverRideSummaryResp
  { earnings :: Maybe Money,
    rideDistance :: HighPrecMeters,
    rideDate :: UTCTime,
    noOfRides :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype DriverRideSummaryListResp = DriverRideSummaryListResp
  { list :: [DriverRideSummaryResp]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

listDriverRidesSummary :: MonadFlow m => Id DP.Person -> Id Merchant.Merchant -> Ride.RideStatus -> Day -> Day -> m DriverRideSummaryListResp
listDriverRidesSummary personId _ rideStatus fromDay toDay = do
  rideSummaryList <- QRide.findRidesWithinDates personId rideStatus fromDay toDay
  list <- mkRideSummaryList rideSummaryList fromDay toDay
  return $ DriverRideSummaryListResp {..}

mkRideSummaryList :: MonadFlow m => [Ride.Ride] -> Day -> Day -> m [DriverRideSummaryResp]
mkRideSummaryList rideSummaryList fromDay toDay = do
  list <- forM rideSummaryList getRidesList
  let transformedList = summarizeRides list fromDay toDay
  return transformedList

getRidesList :: MonadFlow m => Ride.Ride -> m DriverRideSummaryResp
getRidesList item =
  pure $
    DriverRideSummaryResp
      { earnings = item.fare,
        rideDistance = item.traveledDistance,
        rideDate = addUTCTime 19800 item.updatedAt,
        noOfRides = 1
      }

datesBetween :: Day -> Day -> [Day]
datesBetween start end = takeWhile (<= end) $ iterate (addDays 1) start

summarizeRides :: [DriverRideSummaryResp] -> Day -> Day -> [DriverRideSummaryResp]
summarizeRides rides startDate endDate = map toCummilative allDates
  where
    allDates = datesBetween startDate endDate
    toCummilative date =
      let matchingRides = filter (\r -> utctDay (rideDate r) == date) rides
       in DriverRideSummaryResp
            { earnings = sumMaybes $ map earnings matchingRides,
              rideDistance = sum $ map rideDistance matchingRides,
              rideDate = UTCTime date 0,
              noOfRides = sum $ map noOfRides matchingRides
            }

sumMaybes :: [Maybe Money] -> Maybe Money
sumMaybes ms = if null summed then 0 else Just (sum summed)
  where
    summed = catMaybes ms
