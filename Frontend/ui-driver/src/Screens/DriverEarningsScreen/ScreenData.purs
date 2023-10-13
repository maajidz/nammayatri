{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DriverEarningsScreen.ScreenData where

import Data.Maybe
import Foreign.Object (empty)
import Prelude ((-), negate)
import Resource.Constants (tripDatesCount)
import Services.API (RidesSummary)
import Screens.Types (AnimationState(..), DriverEarningsScreenState, DriverEarningsSubView(..))

initData :: DriverEarningsScreenState
initData = {
  data : {
    coinHistoryItems : [{
      event : Just "Ride COMPLETED",
      destination : Just "Kempagowda Airport",
      timestamp : "31/5/2022 7:45pm",
      coins : Just 1,
      earnings : Just 150,
      status : Just "COMPLETED",
      tagImages : []
    }, {
      event : Just "Ride CANCELLED",
      destination : Just "Kempagowda Airport",
      timestamp : "31/5/2022 7:45pm",
      coins : Just (-2),
      earnings : Nothing,
      status : Just "CANCELLED",
      tagImages : []
    }, {
      event : Just "Ride COMPLETED",
      destination : Just "Kempagowda Airport",
      timestamp : "31/5/2022 7:45pm",
      coins : Just 1,
      earnings : Just 85,
      status : Just "COMPLETED",
      tagImages : []
    }, {
      event : Just "Ride CANCELLED",
      destination : Just "Kempagowda Airport",
      timestamp : "31/5/2022 7:45pm",
      coins : Just (-2),
      earnings : Just 200,
      status : Just "CANCELLED",
      tagImages : []
    }
    ],
    earningHistoryItems : [{
      event : Just "Ride COMPLETED",
      destination : Just "Kempagowda Airport",
      timestamp : "31/5/2022 7:45pm",
      coins : Just 1,
      earnings : Just 150,
      status : Just "COMPLETED",
      tagImages : ["ny_ic_tip_ride_tag", "ny_ic_goto_home_tag",  "ny_ic_disability_tag", "ny_ic_special_location_tag"]
    }, {
      event : Just "Ride CANCELLED",
      destination : Just "Kempagowda Airport",
      timestamp : "31/5/2022 7:45pm",
      coins : Just (-2),
      earnings : Nothing,
      status : Just "CANCELLED",
      tagImages : ["ny_ic_tip_ride_tag", "ny_ic_goto_home_tag",  "ny_ic_disability_tag", "ny_ic_special_location_tag"]
    }, {
      event : Just "Ride COMPLETED",
      destination : Just "Kempagowda Airport",
      timestamp : "31/5/2022 7:45pm",
      coins : Just 1,
      earnings : Just 85,
      status : Just "COMPLETED",
      tagImages : ["ny_ic_tip_ride_tag", "ny_ic_goto_home_tag",  "ny_ic_disability_tag", "ny_ic_special_location_tag"]
    }, {
      event : Just "Ride CANCELLED",
      destination : Just "Kempagowda Airport",
      timestamp : "31/5/2022 7:45pm",
      coins : Just (-2),
      earnings : Just 200,
      status : Just "CANCELLED",
      tagImages : ["ny_ic_tip_ride_tag", "ny_ic_goto_home_tag",  "ny_ic_disability_tag", "ny_ic_special_location_tag"]
    }
    ],
    usageHistoryItems : [],
    planItems : [
      {
        name : "Daily Unlimited Plan",
        coins : 100
      },
      {
        name : "Weekly Unlimited Plan",
        coins : 600
      }
    ],
    -- weeklyEarningData : [50.0,20.0,30.0,70.0,50.0,60.0,10.0, 80.0, 60.0, 65.0, 25.0, 40.0, 10.0, 45.0],
    weeklyEarningData : [
      {
        earnings : Just 100,
        rideDistance : 8,
        rideDate : "2023-12-24",
        noOfRides : 3,
        percentLength : 50.0
      },
      {
        earnings : Just 25,
        rideDistance : 8,
        rideDate : "2023-12-24",
        noOfRides : 3,
        percentLength : 20.0
      },
      {
        earnings : Just 150,
        rideDistance : 8,
        rideDate : "2023-12-24",
        noOfRides : 3,
        percentLength : 70.0
      },
      {
        earnings : Just 80,
        rideDistance : 8,
        rideDate : "2023-12-24",
        noOfRides : 6,
        percentLength : 40.0
      },
      {
        earnings : Just 0,
        rideDistance : 0,
        rideDate : "2023-12-24",
        noOfRides : 5,
        percentLength : 0.0
      },
      {
        earnings : Just 25,
        rideDistance : 8,
        rideDate : "2023-12-24",
        noOfRides : 3,
        percentLength : 60.0
      },
      {
        earnings : Just 11,
        rideDistance : 8,
        rideDate : "2023-12-24",
        noOfRides : 5,
        percentLength : 10.0
      },
      {
        earnings : Just 40,
        rideDistance : 8,
        rideDate : "2023-12-24",
        noOfRides : 3,
        percentLength : 30.0
      },
      {
        earnings : Just 76,
        rideDistance : 8,
        rideDate : "2023-12-24",
        noOfRides : 3,
        percentLength : 70.0
      },
      {
        earnings : Just 45,
        rideDistance : 8,
        rideDate : "2023-12-24",
        noOfRides : 3,
        percentLength : 100.0
      },
      {
        earnings : Just 1,
        rideDistance : 8,
        rideDate : "2023-12-24",
        noOfRides : 3,
        percentLength : 50.0
      },
      {
        earnings : Just 70,
        rideDistance : 8,
        rideDate : "2023-12-24",
        noOfRides : 3,
        percentLength : 66.0
      },
      {
        earnings : Just 50,
        rideDistance : 8,
        rideDate : "2023-12-24",
        noOfRides : 3,
        percentLength : 34.0
      },
      {
        earnings : Just 20,
        rideDistance : 8,
        rideDate : "2023-12-24",
        noOfRides : 3,
        percentLength : 58.0
      },
      {
        earnings : Just 10,
        rideDistance : 8,
        rideDate : "2023-12-24",
        noOfRides : 3,
        percentLength : 97.0
      }
    ],
    tagImages : ["ny_ic_tip_ride_tag", "ny_ic_goto_home_tag",  "ny_ic_disability_tag", "ny_ic_special_location_tag"],
    anyRidesAssignedEver : true
  }
  , props : {
    subView : EARNINGS_VIEW,
    selectedPlanIndex : 0,
    selectedPlanQuantity : 0,
    selectedBarIndex : -1,
    weekIndex : 0,
    totalEarningsData : {
      fromDate : "",
      toDate : "",
      totalEarnings : 0,
      totalRides : 0,
      totalDistanceTravelled : 0
    },
    currWeekData : [
      {
        earnings : Just 100,
        rideDistance : 8,
        rideDate : "2023-12-24",
        noOfRides : 3,
        percentLength : 50.0
      },
      {
        earnings : Just 25,
        rideDistance : 8,
        rideDate : "2023-12-24",
        noOfRides : 3,
        percentLength : 20.0
      },
      {
        earnings : Just 150,
        rideDistance : 8,
        rideDate : "2023-12-24",
        noOfRides : 3,
        percentLength : 70.0
      },
      {
        earnings : Just 80,
        rideDistance : 8,
        rideDate : "2023-12-24",
        noOfRides : 6,
        percentLength : 40.0
      },
      {
        earnings : Just 0,
        rideDistance : 0,
        rideDate : "2023-12-24",
        noOfRides : 5,
        percentLength : 0.0
      },
      {
        earnings : Just 25,
        rideDistance : 8,
        rideDate : "2023-12-24",
        noOfRides : 3,
        percentLength : 60.0
      },
      {
        earnings : Just 11,
        rideDistance : 8,
        rideDate : "2023-12-24",
        noOfRides : 5,
        percentLength : 10.0
      },
      {
        earnings : Just 40,
        rideDistance : 8,
        rideDate : "2023-12-24",
        noOfRides : 3,
        percentLength : 30.0
      }
    ]
  }
  , datePickerState : {
    activeIndex : tripDatesCount - 1 -- based on no of dates we are showing
  , selectedItem : {
      date : 0
    , month : ""
    , year : 0
    , utcDate : ""
  }
  }
}