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
    weeklyEarningData : [50.0,20.0,30.0,1.0,50.0,60.0,10.0],
    tagImages : ["ny_ic_tip_ride_tag", "ny_ic_goto_home_tag",  "ny_ic_disability_tag", "ny_ic_special_location_tag"],
    anyRidesAssignedEver : true
  }
  , props : {
    subView : EARNINGS_VIEW,
    selectedPlanIndex : 0,
    selectedPlanQuantity : 0,
    selectedBarIndex : -1
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