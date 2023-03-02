{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.RideHistoryScreen.Controller where

import Log

import Components.BottomNavBar.Controller (Action(..)) as BottomNavBar
import Components.ErrorModal as ErrorModalController
import Components.IndividualRideCard.Controller as IndividualRideCardController
import Data.Array (union, (!!), filter, length)
import Data.Int (ceil)
import Data.Int (fromString, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString) as NUM
import Data.Show (show)
import Data.String (Pattern(..), split)
import Engineering.Helpers.Commons (getNewIDWithTag, strToBool)
import Helpers.Utils (convertUTCtoISC)
import Helpers.Utils (setRefreshing, setEnabled, parseFloat)
import Language.Strings (getString)
import Language.Types (STR(..))
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress)
import Prelude (class Show, pure, unit, ($), map, (==), not, bind, (&&), (<>), (+), (*), (/=), discard, (/))
import PrestoDOM (Eval, continue, exit, ScrollState(..))
import PrestoDOM.Types.Core (class Loggable)
import PrestoDOM.Types.Core (toPropValue)
import Resource.Constants (decodeAddress)
import Screens (ScreenName(..), getScreen)
import Screens.Types (AnimationState(..), IndividualRideCardState(..), ItemState(..), RideHistoryScreenState, RideTabs(..))
import Services.APITypes (RidesInfo(..), Status(..))
import Storage (setValueToLocalNativeStore, KeyStore(..))
import JBridge (firebaseLogEvent)
import Styles.Colors as Color

instance showAction :: Show Action where 
  show _ = ""

instance loggableAction :: Loggable Action where 
  performLog action appId = case action of 
    AfterRender -> trackAppScreenRender appId "screen" (getScreen RIDE_HISTORY_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen RIDE_HISTORY_SCREEN)
      trackAppEndScreen appId (getScreen RIDE_HISTORY_SCREEN)
    OnFadeComplete str -> trackAppActionClick appId (getScreen RIDE_HISTORY_SCREEN) "in_screen" "on_fade_complete"
    Refresh -> trackAppActionClick appId (getScreen RIDE_HISTORY_SCREEN) "in_screen" "refresh"
    SelectTab str -> trackAppActionClick appId (getScreen RIDE_HISTORY_SCREEN) "in_screen" "select_tab"
    BottomNavBarAction (BottomNavBar.OnNavigate item) -> do
      trackAppActionClick appId (getScreen RIDE_HISTORY_SCREEN) "bottom_nav_bar" "on_navigate"
      trackAppEndScreen appId (getScreen RIDE_HISTORY_SCREEN)
    IndividualRideCardAction (IndividualRideCardController.Select index)-> do
      trackAppActionClick appId (getScreen RIDE_HISTORY_SCREEN) "individual_ride_card_action" "select"
      trackAppEndScreen appId (getScreen RIDE_HISTORY_SCREEN)
    Loader -> trackAppActionClick appId (getScreen RIDE_HISTORY_SCREEN) "in_screen" "load_more"
    Scroll str -> trackAppActionClick appId (getScreen RIDE_HISTORY_SCREEN) "in_screen" "scroll_event"
    ScrollStateChanged scrollState -> trackAppActionClick appId (getScreen RIDE_HISTORY_SCREEN) "in_screen" "scroll_state_changed"
    RideHistoryAPIResponseAction resp status -> trackAppScreenEvent appId (getScreen RIDE_HISTORY_SCREEN) "in_screen" "ride_history_response_action"
    ErrorModalActionController action -> trackAppScreenEvent appId (getScreen RIDE_HISTORY_SCREEN) "in_screen" "error_modal_action"
    Dummy -> trackAppScreenEvent appId (getScreen RIDE_HISTORY_SCREEN) "in_screen" "dummy_action"
    NoAction -> trackAppScreenEvent appId (getScreen RIDE_HISTORY_SCREEN) "in_screen" "no_action"
    
data ScreenOutput = GoBack 
                    | RideHistoryScreen RideHistoryScreenState 
                    | HomeScreen 
                    | ProfileScreen 
                    | GoToTripDetails RideHistoryScreenState 
                    | RefreshScreen RideHistoryScreenState 
                    | LoaderOutput RideHistoryScreenState   
                    | GoToNotification 
                    | GoToReferralScreen
                    | SelectTabOutput RideHistoryScreenState

data Action = Dummy 
            | OnFadeComplete String 
            | Refresh 
            | BackPressed
            | SelectTab RideTabs
            | BottomNavBarAction BottomNavBar.Action 
            | IndividualRideCardAction IndividualRideCardController.Action
            | RideHistoryAPIResponseAction (Array RidesInfo) RideTabs
            | Loader
            | Scroll String
            | ErrorModalActionController ErrorModalController.Action
            | NoAction
            | AfterRender
            | ScrollStateChanged ScrollState

eval :: Action -> RideHistoryScreenState -> Eval Action ScreenOutput RideHistoryScreenState
eval AfterRender state = continue state
eval BackPressed state = exit GoBack
eval (OnFadeComplete _ ) state = if (not state.recievedResponse) then continue state else 
  continue state { shimmerLoader = case state.shimmerLoader of
                              AnimatedIn ->AnimatedOut
                              AnimatingOut -> AnimatedOut
                              a -> a  }
                                      
eval Refresh state = do
  exit $ RefreshScreen state

eval (ScrollStateChanged scrollState) state = do 
  _ <- if scrollState == (SCROLL_STATE_FLING) then (pure $ setEnabled "2000030" false)
          else pure unit 
  continue state

eval (SelectTab tab) state = exit $ SelectTabOutput state {currentTab = tab}

eval (BottomNavBarAction (BottomNavBar.OnNavigate screen)) state = do
  case screen of
    "Home" -> exit $ HomeScreen
    "Profile" -> exit $ ProfileScreen
    "Alert" -> do
      _ <- pure $ setValueToLocalNativeStore ALERT_RECEIVED "false"
      _ <- pure $ firebaseLogEvent "ny_driver_alert_click"
      exit $ GoToNotification
    "Contest" -> do
      _ <- pure $ setValueToLocalNativeStore REFERRAL_ACTIVATED "false"
      exit $ GoToReferralScreen
    _ -> continue state

eval (IndividualRideCardAction (IndividualRideCardController.Select index)) state = exit $ GoToTripDetails state { 
      selectedItem = fromMaybe dummyCard ((if state.currentTab == Completed then state.completedRidesTuple.listArrayItems else state.cancelledRidesTuple.listArrayItems) !! index)}
eval Loader state = do
  exit $ LoaderOutput state

eval (RideHistoryAPIResponseAction rideList status) state = do
  _ <- pure $ setRefreshing "2000030" false
  let bufferCardDataPrestoList = rideHistoryListTransformer rideList
  let rideListArray = rideListResponseTransformer rideList
  let newState = state {shimmerLoader = AnimatedOut, recievedResponse = true, loaderButtonVisibility = length rideList == 0}
  case status of
    Completed -> continue $ newState {
      completedRidesTuple {prestoListArrayItems = union state.completedRidesTuple.prestoListArrayItems bufferCardDataPrestoList,
      listArrayItems = union state.completedRidesTuple.listArrayItems rideListArray}}
    Cancelled -> continue $ newState {
      cancelledRidesTuple {prestoListArrayItems = union state.cancelledRidesTuple.prestoListArrayItems bufferCardDataPrestoList, 
      listArrayItems = union state.cancelledRidesTuple.listArrayItems rideListArray}}


eval (Scroll value) state = do
  -- TODO : LOAD MORE FUNCTIONALITY
  let firstIndex = fromMaybe 0 (fromString (fromMaybe "0"((split (Pattern ",")(value))!!0)))
  let visibleItems = fromMaybe 0 (fromString (fromMaybe "0"((split (Pattern ",")(value))!!1)))
  let totalItems = fromMaybe 0 (fromString (fromMaybe "0"((split (Pattern ",")(value))!!2)))
  let canScrollUp = fromMaybe true (strToBool (fromMaybe "true" ((split (Pattern ",")(value))!!3)))
  let loadMoreButton = if (totalItems == (firstIndex + visibleItems) && totalItems /= 0 && totalItems /= visibleItems) then true else false
  _ <- if canScrollUp then (pure $ setEnabled "2000030" false) else  (pure $ setEnabled "2000030" true)
  continue state { loaderButtonVisibility = loadMoreButton}
eval _ state = continue state

rideHistoryListTransformer :: Array RidesInfo -> Array ItemState
rideHistoryListTransformer list = (map (\(RidesInfo ride) -> {
    date : toPropValue (convertUTCtoISC (ride.createdAt) "D MMM"),
    time : toPropValue (convertUTCtoISC (ride.createdAt )"h:mm A"),
    total_amount : toPropValue (case (ride.status) of 
                    "CANCELLED" -> 0
                    _ -> fromMaybe ride.estimatedBaseFare ride.computedFare),
    card_visibility : toPropValue "visible",
    shimmer_visibility : toPropValue "gone",
    rideDistance : toPropValue $ (parseFloat (toNumber (fromMaybe 0 ride.chargeableDistance) / 1000.0) 2) <> " km " <> (getString RIDE),
    ride_distance_visibility : toPropValue (case (ride.status) of
                            "CANCELLED" -> "gone"
                            _ -> "visible"),
    status :  toPropValue (ride.status),
    vehicleModel : toPropValue ride.vehicleModel ,
    shortRideId : toPropValue ride.shortRideId  ,
    vehicleNumber :  toPropValue ride.vehicleNumber  ,
    driverName : toPropValue ride.driverName  ,
    driverSelectedFare : toPropValue ride.driverSelectedFare  ,
    vehicleColor : toPropValue ride.vehicleColor  ,
    id : toPropValue ride.shortRideId,
    updatedAt : toPropValue ride.updatedAt,
    source : toPropValue (decodeAddress (ride.fromLocation) ),
    destination : toPropValue (decodeAddress (ride.toLocation) ),
    amountColor: toPropValue (case (ride.status) of
                  "COMPLETED" -> Color.black800
                  "CANCELLED" -> Color.red
                  _ -> Color.black800)
}) list )

rideListResponseTransformer :: Array RidesInfo -> Array IndividualRideCardState
rideListResponseTransformer list = (map (\(RidesInfo ride) -> {
    date : (convertUTCtoISC (ride.createdAt) "D MMM"),
    time : (convertUTCtoISC (ride.createdAt )"h:mm A"),
    total_amount : (case (ride.status) of 
                    "CANCELLED" -> 0
                    _ -> fromMaybe ride.estimatedBaseFare ride.computedFare),
    card_visibility : (case (ride.status) of 
                        "CANCELLED" -> "gone"
                        _ -> "visible"),
    shimmer_visibility : "gone",
    rideDistance :  parseFloat (toNumber (fromMaybe 0 ride.chargeableDistance) / 1000.0) 2,
    status :  (ride.status),
    vehicleModel : ride.vehicleModel ,
    shortRideId : ride.shortRideId  ,
    vehicleNumber :  ride.vehicleNumber  ,
    driverName : ride.driverName  ,
    driverSelectedFare : ride.driverSelectedFare  ,
    vehicleColor : ride.vehicleColor  ,
    id : ride.shortRideId,
    updatedAt : ride.updatedAt,
    source : (decodeAddress (ride.fromLocation) ),
    destination : (decodeAddress (ride.toLocation) )

}) list )

dummyCard :: IndividualRideCardState
dummyCard =  {
    date : "",
    time : "",
    total_amount : 0,
    card_visibility : "",
    shimmer_visibility : "",
    rideDistance : "",
    status : "",
    vehicleModel : "",
    shortRideId : "",
    vehicleNumber : "",
    driverName : "",
    driverSelectedFare : 0,
    vehicleColor : "",
    id : "",
    updatedAt : "",
    source : "",
    destination : ""
  }