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

import Data.List (groupBy)
import Data.Time (Day, addUTCTime, utctDay)
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
    rideDate :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype DriverRideSummaryListResp = DriverRideSummaryListResp
  { list :: [DriverRideSummaryResp]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

listDriverRidesSummary :: MonadFlow m => Id DP.Person -> Id Merchant.Merchant -> Maybe Ride.RideStatus -> Maybe Day -> Maybe Day -> m DriverRideSummaryListResp
listDriverRidesSummary personId _ mbRideStatus mbFromDay mbToDay = do
  rideSummaryList <- QRide.findRidesWithinDates personId mbRideStatus mbFromDay mbToDay
  list <- mkRideSummaryList rideSummaryList
  return $ DriverRideSummaryListResp {..}

mkRideSummaryList :: MonadFlow m => [Ride.Ride] -> m [DriverRideSummaryResp]
mkRideSummaryList rideSummaryList = forM rideSummaryList somefunc

somefunc :: MonadFlow m => Ride.Ride -> m DriverRideSummaryResp
somefunc item =
  pure $
    DriverRideSummaryResp
      { earnings = item.fare,
        rideDistance = item.traveledDistance,
        rideDate = addUTCTime 19800 item.updatedAt
      }

summarizeRides :: [DriverRideSummaryResp] -> [DriverRideSummaryResp]
summarizeRides rides = map toCummilative grouped
  where
    -- Sort by rideDate
    -- sorted = sortBy (compare `on` rideDate) rides

    -- Group by rideDate
    grouped = groupBy ((==) `on` (utctDay . rideDate)) rides

    -- Convert each group to the cumulative response
    toCummilative grp =
      DriverRideSummaryResp
        { earnings = sumMaybes $ map earnings grp,
          rideDistance = sum $ map rideDistance grp,
          rideDate = rideDate (head grp)
        }

    -- Helper function to sum Maybe values
    sumMaybes :: [Maybe Money] -> Maybe Money
    sumMaybes ms = if null summed then Nothing else Just (sum summed)
      where
        summed = catMaybes ms
