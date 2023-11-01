{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.HomeScreen.Controller where

import Common.Types.App (OptionButtonList, APIPaymentStatus(..), PaymentStatus(..)) as Common
import Components.BottomNavBar as BottomNavBar
import Components.SelectListModal as SelectListModal
import Components.Banner as Banner
import Components.ChatView as ChatView
import Components.InAppKeyboardModal as InAppKeyboardModal
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButtonController
import Components.RideActionModal as RideActionModal
import Components.MakePaymentModal as MakePaymentModal
import Components.ChatView as ChatView
import Components.StatsModel.Controller as StatsModelController
import Components.RequestInfoCard as RequestInfoCard
import Control.Monad.State (state)
import Data.Array as Array
import Data.Int (round, toNumber, fromString)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString) as Number
import Data.String (Pattern(..), Replacement(..), drop, length, take, trim, replaceAll, toLower)
import Effect (Effect)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (clearTimer, getCurrentUTC, getNewIDWithTag, convertUTCtoISC)
import Helpers.Utils (currentPosition, differenceBetweenTwoUTC, getDistanceBwCordinates, parseFloat,setText,getTime, differenceBetweenTwoUTC, getCurrentUTC)
import JBridge (animateCamera, enableMyLocation, firebaseLogEvent, getCurrentPosition, getHeightFromPercent, hideKeyboardOnNavigation, isLocationEnabled, isLocationPermissionEnabled, minimizeApp, openNavigation, removeAllPolylines, requestLocation, showDialer, showMarker, toast, firebaseLogEventWithTwoParams,sendMessage, stopChatListenerService, getSuggestionfromKey, scrollToEnd, waitingCountdownTimer, getChatMessages, cleverTapCustomEvent)
import Language.Strings (getString, getEN)
import Language.Types (STR(..))
import Log (printLog, trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Prelude (class Show, Unit, bind, discard, map, not, pure, show, unit, void, ($), (&&), (*), (+), (-), (/), (/=), (<), (<>), (==), (>), (||), (<=),(>=), when)
import PrestoDOM (Eval, continue, continueWithCmd, exit, updateAndExit, updateWithCmdAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Resource.Constants (decodeAddress)
import Screens (ScreenName(..), getScreen)
import Screens.Types as ST
import Services.API (GetRidesHistoryResp, RidesInfo(..), Status(..))
import Services.Accessor (_lat, _lon)
import Services.Config (getCustomerNumber)
import Storage (KeyStore(..), deleteValueFromLocalStore, getValueToLocalNativeStore, getValueToLocalStore, setValueToLocalNativeStore, setValueToLocalStore)
import Types.App (FlowBT, GlobalState(..), HOME_SCREENOUTPUT(..), ScreenType(..))
import Types.ModifyScreenState (modifyScreenState)
import Engineering.Helpers.Suggestions (getMessageFromKey, getSuggestionsfromKey)
import Engineering.Helpers.LogEvent (logEvent,logEventWithTwoParams)
import Effect.Unsafe (unsafePerformEffect)
import Components.RateCard as RateCard
import Common.Styles.Colors as Color

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen HOME_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen HOME_SCREEN)
      trackAppEndScreen appId (getScreen HOME_SCREEN)
    ScreenClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "screen_click"
    Notification _ -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "notification_page"
    ChangeStatus status -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "change_status"
    GoOffline status -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "go_offline"
    CancelGoOffline -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "cancell_go_offline"
    ShowMap key lat lon -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "show_map"
    BottomNavBarAction (BottomNavBar.OnNavigate item) -> do
      trackAppActionClick appId (getScreen HOME_SCREEN) "bottom_nav_bar" "on_navigate"
      trackAppEndScreen appId (getScreen HOME_SCREEN)
    RideActionModalAction act -> pure unit-- case act of
      -- RideActionModal.StartRide -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_action_modal" "start_ride"
      -- RideActionModal.EndRide -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_action_modal" "end_ride"
      -- RideActionModal.SelectListModal -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_action_modal" "cancel_ride"
      -- RideActionModal.OnNavigate -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_action_modal" "on_navigate"
      -- RideActionModal.CallCustomer -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_action_modal" "call_customer"
      -- RideActionModal.LocationTracking -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_action_modal" "location_tracking"
      -- RideActionModal.NotifyCustomer -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_action_modal" "notify_driver"
      -- RideActionModal.ButtonTimer seconds id status timerID -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_action_modal" "button_timer"
      -- RideActionModal.MessageCustomer -> trackAppActionClick appId (getScreen HOME_SCREEN) "ride_action_modal" "message_customer"
      -- _ -> pure unit

    InAppKeyboardModalAction act -> pure unit--case act of
      -- InAppKeyboardModal.OnSelection key index -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_otp_modal" "on_selection"
      -- InAppKeyboardModal.OnClickBack text -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_otp_modal" "on_click_back"
      -- InAppKeyboardModal.OnclickTextBox index -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_otp_modal" "on_click_text_box"
      -- InAppKeyboardModal.BackPressed -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_otp_modal" "on_backpressed"
      -- InAppKeyboardModal.OnClickDone text -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_otp_modal" "on_click_done"
      -- _ -> pure unit

    CountDown seconds -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "count_down"
    PopUpModalAction act -> pure unit --case act of
      -- PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_end_ride" "go_back_onclick"
      -- PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_end_ride" "end_ride_onclick"
      -- PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_end_ride" "no_action"
      -- PopUpModal.ETextController act-> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_end_ride_text_changed" "primary_edit_text"
      -- PopUpModal.CountDown seconds id status timerID -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_end_ride" "countdown_updated"
      -- PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_end_ride" "image_onclick"
      -- PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "tip_clicked"
      -- PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "popup_dismissed"
    PopUpModalCancelConfirmationAction act -> pure unit -- case act of
      -- PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "continue_onclick"
      -- PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "go_back_onclick"
      -- PopUpModal.CountDown seconds id status timerID -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "countdown_updated"
      -- PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "no_action"
      -- PopUpModal.ETextController act-> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation_text_changed" "primary_edit_text"
      -- PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "image_onclick"
      -- PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "tip_clicked"
      -- PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "popup_dismissed"
    CancelRideModalAction act -> pure unit -- case act of
      -- SelectListModal.Button1 act -> case act of
      --   PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride" "primary_btn_go_back_onclick"
      --   PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride" "primary_btn_go_back_no_action"
      -- SelectListModal.Button2 act -> case act of
      --   PrimaryButtonController.OnClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride" "primary_btn_cancel_ride_onclick"
      --   PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride" "primary_btn_cancel_ride_no_action"
      -- SelectListModal.UpdateIndex indexValue -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride" "update_index_onclick"
      -- SelectListModal.TextChanged  valId newVal -> trackAppTextInput appId (getScreen HOME_SCREEN) "reason_text_changed" "cancel_ride"
      -- SelectListModal.OnGoBack -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride" "go_back_onclick"
      -- SelectListModal.ClearOptions -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride" "clear_options_onclick"
      -- SelectListModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "cancel_ride" "no_action"
    GenderBannerModal act -> pure unit
    RetryTimeUpdate -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "retry_time_update_onclick"
    RideActiveAction activeRide -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "ride_active_action"
    StatsModelAction act -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "stats_model_action"
    TimeUpdate time lat lng -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "time_update"
    ModifyRoute lat lon -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "modify_route"
    SetToken id -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "set_token"
    Cancel -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "cancel"
    CurrentLocation lat lng -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "current_location"
    ActiveRideAPIResponseAction resp -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "active_ride_api_response"
    RecenterButtonAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "recenter_btn"
    HelpAndSupportScreen -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "help_and_support_btn"
    NoAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "no_action"
    UpdateMessages msg sender timeStamp size -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_messages"
    OpenChatScreen -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "open_chat"
    InitializeChat -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "initialize_chat"
    RemoveChat -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "remove_chat"
    UpdateInChat -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "update_in_chat"
    ScrollToBottom -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "scroll_to_bottom"
    ChatViewActionController act -> pure unit -- case act of
      -- ChatView.SendMessage -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "send_message"
      -- ChatView.SendSuggestion suggestion -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "send_suggestion"
      -- ChatView.BackPressed -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "back_pressed"
      -- ChatView.TextChanged input -> trackAppTextInput appId (getScreen HOME_SCREEN) "in_app_messaging" "text_changed"
      -- ChatView.Call -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "call_driver"
      -- ChatView.Navigate -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_app_messaging" "navigate_to_google_maps"
      -- ChatView.NoAction -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_app_messaging" "no_action"
    SwitchDriverStatus status -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "in_screen" "SwitchDriverStatus"
    GoToProfile -> do
      trackAppActionClick appId (getScreen HOME_SCREEN) "bottom_nav_bar" "on_navigate"
      trackAppEndScreen appId (getScreen HOME_SCREEN)
    LinkAadhaarPopupAC act -> pure unit --case act of
    PopUpModalSilentAction act -> pure unit --case act of
      -- PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_silent_confirmation" "go_offline_onclick"
      -- PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_silent_confirmation" "go_silent_onclick"
      -- PopUpModal.CountDown seconds id status timerID -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_silent_confirmation" "countdown_updated"
      -- PopUpModal.NoAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_silent_confirmation" "no_action"
      -- PopUpModal.ETextController act-> trackAppTextInput appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation_text_changed" "primary_edit_text"
      -- PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen HOME_SCREEN) "popup_modal_cancel_confirmation" "image_onclick"
      -- PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "tip_clicked"
      -- PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen HOME_SCREEN) "popup_modal_action" "popup_dismissed"
    ClickAddAlternateButton -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "add-alternate_btn"
    ZoneOtpAction -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "zone_otp"
    TriggerMaps -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "trigger_maps"
    RemoveGenderBanner -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "gender_banner"
    RequestInfoCardAction act -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "request_info_card"
    WaitTimerCallback id min sec -> trackAppActionClick appId (getScreen HOME_SCREEN) "in_screen" "wait_timer_callBack" 
    MakePaymentModalAC act -> pure unit
    RateCardAC act -> pure unit
    PaymentBannerAC act -> pure unit
    PaymentStatusAction _ -> pure unit
    RemovePaymentBanner -> pure unit
    OfferPopupAC _ -> pure unit


data ScreenOutput =   Refresh ST.HomeScreenState
                    | GoToHelpAndSupportScreen ST.HomeScreenState
                    | GoToProfileScreen ST.HomeScreenState
                    | GoToRidesScreen ST.HomeScreenState
                    | GoToReferralScreen
                    | StartRide ST.HomeScreenState
                    | EndRide ST.HomeScreenState
                    | SelectListModal ST.HomeScreenState
                    | DriverAvailabilityStatus ST.HomeScreenState ST.DriverStatus
                    | UpdatedState ST.HomeScreenState
                    | UpdateRoute ST.HomeScreenState
                    | FcmNotification String ST.HomeScreenState
                    | NotifyDriverArrived ST.HomeScreenState
                    | UpdateStage ST.HomeScreenStage ST.HomeScreenState
                    | GoToNotifications ST.HomeScreenState
                    | AddAlternateNumber ST.HomeScreenState
                    | StartZoneRide ST.HomeScreenState
                    | CallCustomer ST.HomeScreenState
                    | GotoEditGenderScreen
                    | OpenPaymentPage ST.HomeScreenState
                    | AadhaarVerificationFlow ST.HomeScreenState
                    | SubscriptionScreen ST.HomeScreenState

data Action = NoAction
            | BackPressed
            | ScreenClick
            | Notification String
            | ChangeStatus Boolean
            | GoOffline Boolean
            | CancelGoOffline
            | AfterRender
            | ShowMap String String String
            | BottomNavBarAction BottomNavBar.Action
            | RideActionModalAction RideActionModal.Action
            | InAppKeyboardModalAction InAppKeyboardModal.Action
            | CountDown Int
            | CurrentLocation String String
            | ActiveRideAPIResponseAction (Array RidesInfo)
            | PopUpModalAction PopUpModal.Action
            | PopUpModalCancelConfirmationAction PopUpModal.Action
            | CancelRideModalAction SelectListModal.Action
            | Cancel
            | SetToken String
            | ModifyRoute String String
            | RetryTimeUpdate
            | TimeUpdate String String String
            | StatsModelAction StatsModelController.Action
            | RideActiveAction RidesInfo
            | RecenterButtonAction
            | ChatViewActionController ChatView.Action
            | UpdateMessages String String String String
            | InitializeChat
            | OpenChatScreen
            | RemoveChat
            | UpdateInChat
            | HelpAndSupportScreen
            | SwitchDriverStatus ST.DriverStatus
            | PopUpModalSilentAction PopUpModal.Action
            | LinkAadhaarPopupAC PopUpModal.Action
            | GoToProfile
            | ClickAddAlternateButton
            | ZoneOtpAction
            | TriggerMaps
            | GenderBannerModal Banner.Action
            | PaymentBannerAC Banner.Action
            | RemoveGenderBanner
            | RequestInfoCardAction RequestInfoCard.Action
            | ScrollToBottom
            | WaitTimerCallback String String Int
            | MakePaymentModalAC MakePaymentModal.Action
            | RateCardAC RateCard.Action
            | PaymentStatusAction Common.APIPaymentStatus
            | RemovePaymentBanner
            | OfferPopupAC PopUpModal.Action


eval :: Action -> ST.HomeScreenState -> Eval Action ScreenOutput ST.HomeScreenState

eval AfterRender state = do
  continue state{props{mapRendered= true}}
eval BackPressed state = do
  if state.props.enterOtpModal then do
    continue state { props = state.props { rideOtp = "", enterOtpFocusIndex = 0, enterOtpModal = false, rideActionModal = true } }
    else if (state.props.currentStage == ST.ChatWithCustomer) then do
      _ <- pure $ setValueToLocalStore LOCAL_STAGE (show ST.RideAccepted)
      continue state{props{currentStage = ST.RideAccepted}}
      else if state.props.cancelRideModalShow then do
        continue state { data { cancelRideModal {activeIndex = Nothing, selectedReasonCode = "", selectedReasonDescription = ""}} ,props{ cancelRideModalShow = false, cancelConfirmationPopup = false}}
          else if state.props.cancelConfirmationPopup then do
            _ <- pure $ clearTimer state.data.cancelRideConfirmationPopUp.timerID
            continue state {props{cancelConfirmationPopup = false}, data{cancelRideConfirmationPopUp{timerID = "" , continueEnabled=false, enableTimer=false}}}
              else if state.props.showBonusInfo then do
                continue state { props { showBonusInfo = false } }
                  else if state.data.paymentState.showRateCard then
                    continue state { data { paymentState{ showRateCard = false } } }
                      else if state.props.endRidePopUp then continue state{props {endRidePopUp = false}}
                        else if (state.props.showlinkAadhaarPopup && state.props.showAadharPopUp) then continue state {props{showAadharPopUp = false}}
                          else do
                            _ <- pure $ minimizeApp ""
                            continue state

eval TriggerMaps state = continueWithCmd state[ do
  _ <- pure $ openNavigation 0.0 0.0 state.data.activeRide.dest_lat state.data.activeRide.dest_lon
  _ <- pure $ setValueToLocalStore TRIGGER_MAPS "false"
  pure NoAction
  ]
-- eval (ChangeStatus status) state = -- TODO:: DEPRECATE AFTER 19th April
--   if (getValueToLocalStore IS_DEMOMODE_ENABLED == "true") then
--         continueWithCmd state [ do
--           _ <- pure $ setValueToLocalStore IS_DEMOMODE_ENABLED "false"
--           _ <- pure $ toast (getString DEMO_MODE_DISABLED)
--           _ <- pure $  deleteValueFromLocalStore IS_DEMOMODE_ENABLED
--           _ <- pure $  deleteValueFromLocalStore DEMO_MODE_PASSWORD
--           _ <- getCurrentPosition (showDriverMarker state "ny_ic_auto") constructLatLong
--           pure NoAction
--           ]
--     else if  status then exit (DriverAvailabilityStatus state status)
--       else continue state { props { goOfflineModal = true }}

eval (Notification notificationType) state = do
  _ <- pure $ printLog "notificationType" notificationType
  if (checkNotificationType notificationType ST.DRIVER_REACHED && (state.props.currentStage == ST.RideAccepted || state.props.currentStage == ST.ChatWithCustomer) && (not state.data.activeRide.notifiedCustomer)) then do
    _ <- pure $ setValueToLocalStore IS_DRIVER_AT_PICKUP "true"
    continue state{data{activeRide{isDriverArrived = true}}}
    else if (Array.any ( _ == notificationType) [show ST.CANCELLED_PRODUCT, show ST.DRIVER_ASSIGNMENT, show ST.RIDE_REQUESTED, show ST.DRIVER_REACHED]) then do
      exit $ FcmNotification notificationType state
      else continue state

eval CancelGoOffline state = do
  continue state { props = state.props { goOfflineModal = false } }

eval (GoOffline status) state = exit (DriverAvailabilityStatus state { props = state.props { goOfflineModal = false }} ST.Offline)

eval (ShowMap key lat lon) state = continueWithCmd state [ do
  id <- checkPermissionAndUpdateDriverMarker state
  pure AfterRender
  ]
eval (BottomNavBarAction (BottomNavBar.OnNavigate item)) state = do
  case item of
    "Rides" -> exit $ GoToRidesScreen state
    "Profile" -> exit $ GoToProfileScreen state
    "Alert" -> do
      _ <- pure $ setValueToLocalNativeStore ALERT_RECEIVED "false"
      let _ = unsafePerformEffect $ logEvent state.data.logField "ny_driver_alert_click"
      exit $ GoToNotifications state
    "Rankings" -> do
      _ <- pure $ setValueToLocalNativeStore REFERRAL_ACTIVATED "false"
      exit $ GoToReferralScreen
    "Join" -> do
      let driverSubscribed = getValueToLocalNativeStore DRIVER_SUBSCRIBED == "true"
      _ <- pure $ cleverTapCustomEvent if driverSubscribed then "ny_driver_myplan_option_clicked" else "ny_driver_plan_option_clicked"
      exit $ SubscriptionScreen state
    _ -> continue state

eval (OfferPopupAC PopUpModal.OnButton1Click) state = do
  _ <- pure $ setValueToLocalNativeStore SHOW_JOIN_NAMMAYATRI "__failed"
  _ <- pure $ cleverTapCustomEvent "ny_driver_in_app_popup_join_now"
  exit $ SubscriptionScreen state {props { showOffer = false }}

eval (OfferPopupAC PopUpModal.DismissPopup) state = do
  _ <- pure $ setValueToLocalNativeStore SHOW_JOIN_NAMMAYATRI "__failed"
  continue state {props { showOffer = false }}

eval (InAppKeyboardModalAction (InAppKeyboardModal.OnSelection key index)) state = do
  let
    rideOtp = if (index + 1) > (length state.props.rideOtp) then ( take 4 (state.props.rideOtp <> key)) else (take index (state.props.rideOtp)) <> key <> (take 4 (drop (index+1) state.props.rideOtp))
    focusIndex = length rideOtp
    newState = state { props = state.props { rideOtp = rideOtp, enterOtpFocusIndex = focusIndex ,otpIncorrect = false} }
    exitAction = if state.props.zoneRideBooking then StartZoneRide newState else StartRide newState
  if ((length rideOtp) >= 4  && (not state.props.otpAttemptsExceeded)) then updateAndExit newState exitAction
  else continue newState
eval (InAppKeyboardModalAction (InAppKeyboardModal.OnClickBack text)) state = do
  let
    rideOtp = (if length( text ) > 0 then (take (length ( text ) - 1 ) text) else "" )
    focusIndex = length rideOtp
  continue state { props = state.props { rideOtp = rideOtp, enterOtpFocusIndex = focusIndex, otpIncorrect = false } }
eval (InAppKeyboardModalAction (InAppKeyboardModal.OnclickTextBox index)) state = do
  let focusIndex = if index > (length state.props.rideOtp) then (length state.props.rideOtp) else index
  let rideOtp = take index state.props.rideOtp
  continue state { props = state.props { enterOtpFocusIndex = focusIndex, rideOtp = rideOtp, otpIncorrect = false } }
eval (InAppKeyboardModalAction (InAppKeyboardModal.BackPressed)) state = do
  continue state { props = state.props { rideOtp = "", enterOtpFocusIndex = 0, enterOtpModal = false} }
eval (InAppKeyboardModalAction (InAppKeyboardModal.OnClickDone text)) state = do
    let exitState = if state.props.zoneRideBooking then StartZoneRide state else StartRide state
    exit exitState
eval (RideActionModalAction (RideActionModal.StartRide)) state = do
  continue state { props = state.props { enterOtpModal = true, rideOtp = "", enterOtpFocusIndex = 0, otpIncorrect = false, zoneRideBooking = false } }
eval (RideActionModalAction (RideActionModal.EndRide)) state = do
  continue $ (state {props {endRidePopUp = true}, data {route = []}})
eval (RideActionModalAction (RideActionModal.OnNavigate)) state = do
  _ <- pure $ setValueToLocalStore TRIGGER_MAPS "false"
  let lat = if (state.props.currentStage == ST.RideAccepted || state.props.currentStage == ST.ChatWithCustomer) then state.data.activeRide.src_lat else state.data.activeRide.dest_lat
      lon = if (state.props.currentStage == ST.RideAccepted || state.props.currentStage == ST.ChatWithCustomer) then state.data.activeRide.src_lon else state.data.activeRide.dest_lon
  void $ pure $ openNavigation 0.0 0.0 lat lon
  continue state
eval (RideActionModalAction (RideActionModal.CancelRide)) state = do
  continue state{ data {cancelRideConfirmationPopUp{delayInSeconds = 5,  continueEnabled=false}}, props{cancelConfirmationPopup = true}}
eval (RideActionModalAction (RideActionModal.CallCustomer)) state = continueWithCmd state [ do
  _ <- pure $ showDialer (if (take 1 state.data.activeRide.exoPhone) == "0" || (take 3 state.data.activeRide.exoPhone) == "+91" then state.data.activeRide.exoPhone else "0" <> state.data.activeRide.exoPhone) false -- TODO: FIX_DIALER
  _ <- logEventWithTwoParams state.data.logField "call_customer" "trip_id" (state.data.activeRide.id) "user_id" (getValueToLocalStore DRIVER_ID)
  pure NoAction
  ]

eval (MakePaymentModalAC (MakePaymentModal.PrimaryButtonActionController PrimaryButtonController.OnClick)) state = exit $ OpenPaymentPage state

eval (MakePaymentModalAC (MakePaymentModal.Cancel)) state = continue state{data { paymentState {makePaymentModal = false}}}

eval (MakePaymentModalAC (MakePaymentModal.Info)) state = continue state{data { paymentState {showRateCard = true}}}

eval (RateCardAC (RateCard.PrimaryButtonAC PrimaryButtonController.OnClick)) state = continue state{data { paymentState {showRateCard = false}}}

------------------------------- ChatService - Start --------------------------

eval (OpenChatScreen) state = do
  if not state.props.chatcallbackInitiated then continue state else do
    continueWithCmd state{props{openChatScreen = false}} [do
      pure $ (RideActionModalAction (RideActionModal.MessageCustomer))
    ]

eval (RideActionModalAction (RideActionModal.MessageCustomer)) state = do
  if not state.props.chatcallbackInitiated then continue state else do
    _ <- pure $ setValueToLocalStore LOCAL_STAGE (show ST.ChatWithCustomer)
    _ <- pure $ setValueToLocalNativeStore READ_MESSAGES (show (Array.length state.data.messages))
    let allMessages = getChatMessages ""
    continue state{data{messages = allMessages}, props{currentStage = ST.ChatWithCustomer, sendMessageActive = false, unReadMessages = false, isChatOpened = true}}

eval (UpdateInChat) state = continue state {props{updatedArrivalInChat = true}}

eval (InitializeChat ) state = continue state {props{chatcallbackInitiated = true}}

eval RemoveChat state = do
  continueWithCmd state {props{chatcallbackInitiated = false}} [ do
    _ <- stopChatListenerService
    _ <- pure $ setValueToLocalNativeStore READ_MESSAGES "0"
    pure $ NoAction
  ]

eval (UpdateMessages message sender timeStamp size) state = do
  if not state.props.chatcallbackInitiated then continue state {props {canSendSuggestion = true}} else do
    continueWithCmd state{data{messagesSize = size}, props {canSendSuggestion = true}} [do
      pure $ (RideActionModalAction (RideActionModal.LoadMessages))
    ]
    
eval (RideActionModalAction (RideActionModal.LoadMessages)) state = do
  let allMessages = getChatMessages ""
  case (Array.last allMessages) of
      Just value -> if value.message == "" then continue state {data { messagesSize = show (fromMaybe 0 (fromString state.data.messagesSize) + 1)}, props {canSendSuggestion = true}} else
                      if value.sentBy == "Driver" then updateMessagesWithCmd state {data {messages = allMessages, suggestionsList = []}, props {canSendSuggestion = true}}
                      else do
                        let readMessages = fromMaybe 0 (fromString (getValueToLocalNativeStore READ_MESSAGES))
                        let unReadMessages = (if (readMessages == 0 && state.props.currentStage /= ST.ChatWithCustomer) then true else (if (readMessages < (Array.length allMessages) && state.props.currentStage /= ST.ChatWithCustomer) then true else false))
                        let suggestions = getSuggestionsfromKey value.message
                        updateMessagesWithCmd state {data {messages = allMessages, suggestionsList = suggestions }, props {unReadMessages = unReadMessages, canSendSuggestion = true}}
      Nothing -> continue state {props {canSendSuggestion = true}}

eval ScrollToBottom state = do
  _ <- pure $ scrollToEnd (getNewIDWithTag "ChatScrollView") true
  continue state

eval (ChatViewActionController (ChatView.TextChanged value)) state = continue state{data{messageToBeSent = (trim value)},props{sendMessageActive = (length (trim value)) >= 1}}

eval(ChatViewActionController (ChatView.Call)) state = continueWithCmd state [ do
  _ <- pure $ showDialer (if (take 1 state.data.activeRide.exoPhone) == "0" then state.data.activeRide.exoPhone else "0" <> state.data.activeRide.exoPhone) false -- TODO: FIX_DIALER
  _ <- logEventWithTwoParams state.data.logField "call_customer" "trip_id" state.data.activeRide.id "user_id" (getValueToLocalStore DRIVER_ID)
  pure NoAction
  ]

eval (ChatViewActionController (ChatView.SendMessage)) state = do
  if state.data.messageToBeSent /= ""
  then
   continueWithCmd state{data{messageToBeSent = ""},props {sendMessageActive = false}} [do
      _ <- pure $ sendMessage state.data.messageToBeSent
      _ <- pure $ setText (getNewIDWithTag "ChatInputEditText") ""
      pure NoAction
   ]
  else
    continue state

eval (ChatViewActionController (ChatView.SendSuggestion chatSuggestion)) state = do
  if state.props.canSendSuggestion then do
    let message = getMessageFromKey chatSuggestion "EN_US"
    _ <- pure $ sendMessage message
    let _ = unsafePerformEffect $ logEvent state.data.logField $ toLower $ (replaceAll (Pattern "'") (Replacement "") (replaceAll (Pattern ",") (Replacement "") (replaceAll (Pattern " ") (Replacement "_") chatSuggestion)))
    continue state{props {canSendSuggestion = false}}
  else continue state

eval (ChatViewActionController (ChatView.BackPressed)) state = do
  _ <- pure $ hideKeyboardOnNavigation true
  continueWithCmd state [do
      pure $ BackPressed
    ]

eval (ChatViewActionController (ChatView.Navigate)) state = do
  _ <- pure $ hideKeyboardOnNavigation true
  continueWithCmd state [do
    pure $ RideActionModalAction (RideActionModal.OnNavigate)
  ]

------------------------------- ChatService - End --------------------------

eval (RideActionModalAction (RideActionModal.LocationTracking)) state = do
  let newState = state {props {showDottedRoute = not state.props.showDottedRoute} }
  updateAndExit newState $ UpdateRoute newState

eval (RideActionModalAction (RideActionModal.NotifyCustomer)) state =do
  _ <- pure $ setValueToLocalStore LOCAL_STAGE (show ST.ChatWithCustomer)
  let newState = state{props{currentStage = ST.ChatWithCustomer, sendMessageActive = false, unReadMessages = false}}
  updateAndExit newState $ NotifyDriverArrived newState

eval (RideActionModalAction (RideActionModal.ButtonTimer seconds id status timerID)) state = do
  if status == "EXPIRED"
    then do
      _ <- pure $ clearTimer timerID
      _ <- pure $ setValueToLocalStore IS_DRIVER_AT_PICKUP "false"
      continue state{data{activeRide{isDriverArrived = false}}}
    else
      continue state

eval (RideActionModalAction (RideActionModal.WaitingInfo)) state = do
  continue state {data{activeRide {waitTimeInfo = true }}}

eval (RideActionModalAction (RideActionModal.TimerCallback timerID timeInMinutes seconds)) state = continueWithCmd state [do pure $ (WaitTimerCallback timerID timeInMinutes seconds)]

eval (WaitTimerCallback timerID timeInMinutes seconds) state = do
      if (getValueToLocalStore IS_WAIT_TIMER_STOP) == "Stop" || (getValueToLocalStore IS_WAIT_TIMER_STOP) == "NoView" then do
        _ <- pure $ clearTimer timerID
        _ <- pure $ setValueToLocalStore SET_WAITING_TIME timeInMinutes
        pure unit
      else do
        _ <- pure $ setValueToLocalStore IS_WAIT_TIMER_STOP (show ST.Triggered)
        pure unit
      continue state { data {activeRide { waitingTime = timeInMinutes} } ,props {timerRefresh = false} }

eval (PopUpModalAction (PopUpModal.OnButton1Click)) state = continue $ (state {props {endRidePopUp = false}})
eval (PopUpModalAction (PopUpModal.OnButton2Click)) state = do
  _ <- pure $ removeAllPolylines ""
  updateAndExit state {props {endRidePopUp = false, rideActionModal = false}} $ EndRide state {props {endRidePopUp = false, rideActionModal = false, zoneRideBooking = true}}

eval (CancelRideModalAction (SelectListModal.UpdateIndex indexValue)) state = continue state { data = state.data { cancelRideModal  { activeIndex = Just indexValue, selectedReasonCode = (fromMaybe dummyCancelReason $ state.data.cancelRideModal.selectionOptions Array.!!indexValue).reasonCode } } }
eval (CancelRideModalAction (SelectListModal.TextChanged  valId newVal)) state = continue state { data {cancelRideModal { selectedReasonDescription = newVal, selectedReasonCode = "OTHER"}}}
eval (CancelRideModalAction (SelectListModal.Button1 PrimaryButtonController.OnClick)) state = do
  pure $ hideKeyboardOnNavigation true
  continue state { data{cancelRideModal {activeIndex = Nothing, selectedReasonCode = "", selectedReasonDescription = ""}} ,props {cancelRideModalShow = false, cancelConfirmationPopup=false } }
eval (CancelRideModalAction (SelectListModal.OnGoBack)) state = continue state { data { cancelRideModal {activeIndex = Nothing, selectedReasonCode = "", selectedReasonDescription = ""}} ,props{ cancelRideModalShow = false, cancelConfirmationPopup = false}}
eval (CancelRideModalAction (SelectListModal.ClearOptions)) state = continue state {data {cancelRideModal {activeIndex = Nothing, selectedReasonCode = "", selectedReasonDescription = ""}}}
eval (CancelRideModalAction (SelectListModal.Button2 PrimaryButtonController.OnClick)) state = do
    pure $ hideKeyboardOnNavigation true
    let cancelReasonSelected = case state.data.cancelRideModal.activeIndex of
                                  Just index -> (state.data.cancelRideModal.selectionOptions Array.!! (index))
                                  Nothing    -> Nothing
    _ <- pure $ printLog "cancelReasonSelected" cancelReasonSelected
    case cancelReasonSelected of
      Just reason -> do
        _ <- pure $ printLog "inside Just" reason.reasonCode
        if (reason.reasonCode == "OTHER") then exit $ SelectListModal state { props = state.props { cancelRideModalShow = false , cancelConfirmationPopup = false, zoneRideBooking = true} } else do
          let newState = state { data = state.data {cancelRideModal = state.data.cancelRideModal { selectedReasonCode = reason.reasonCode , selectedReasonDescription = reason.description  } }, props = state.props { cancelRideModalShow = false, otpAttemptsExceeded = false, cancelConfirmationPopup = false, zoneRideBooking = true } }
          exit $ SelectListModal newState
      Nothing -> do
        _ <- pure $ printLog "inside Nothing" "."
        continue state


eval (PopUpModalCancelConfirmationAction (PopUpModal.OnButton2Click)) state = do
  _ <- pure $ clearTimer state.data.cancelRideConfirmationPopUp.timerID
  continue state {props{cancelConfirmationPopup = false}, data{cancelRideConfirmationPopUp{timerID = "" , continueEnabled=false, enableTimer=false}}}

eval (PopUpModalCancelConfirmationAction (PopUpModal.OnButton1Click)) state = continue state {props {cancelRideModalShow = true, cancelConfirmationPopup = false},data {cancelRideConfirmationPopUp{enableTimer = false}, cancelRideModal {activeIndex=Nothing, selectedReasonCode="", selectionOptions = cancellationReasons "" }}}

eval (PopUpModalCancelConfirmationAction (PopUpModal.CountDown seconds id status timerID)) state = do
  if status == "EXPIRED" && seconds == 0 then do
    _ <- pure $ clearTimer timerID
    continue state { data { cancelRideConfirmationPopUp{delayInSeconds = 0, timerID = "", continueEnabled = true}}}
    else continue state { data {cancelRideConfirmationPopUp{delayInSeconds = (seconds+1), timerID = timerID, continueEnabled = false}}}

eval (CancelRideModalAction SelectListModal.NoAction) state = do
  _ <- pure $ printLog "CancelRideModalAction NoAction" state.data.cancelRideModal.selectionOptions
  continue state
eval (SetToken id )state = do
  _ <-  pure $ setValueToLocalNativeStore FCM_TOKEN  id
  continue state
eval (CurrentLocation lat lng) state = do
  let newState = state{data{ currentDriverLat = getLastKnownLocValue ST.LATITUDE lat,  currentDriverLon = getLastKnownLocValue ST.LONGITUDE lng }}
  exit $ UpdatedState newState
eval (ModifyRoute lat lon) state = do
  let newState = state { data = state.data {currentDriverLat = getLastKnownLocValue ST.LATITUDE lat, currentDriverLon = getLastKnownLocValue ST.LONGITUDE lon} }
  exit $ UpdateRoute newState

eval RetryTimeUpdate state = do
  _ <-  pure $ setValueToLocalNativeStore REGISTERATION_TOKEN (getValueToLocalStore REGISTERATION_TOKEN)
  (updateAndExit state { data = state.data { locationLastUpdatedTime = "" }, props = state.props {refreshAnimation = true}} $ Refresh state { data = state.data { locationLastUpdatedTime = "" }, props = state.props {refreshAnimation = true}})

eval (TimeUpdate time lat lng) state = do
  let isDriverNearBy = ((getDistanceBwCordinates (getLastKnownLocValue ST.LATITUDE lat) (getLastKnownLocValue ST.LONGITUDE lng)  state.data.activeRide.src_lat state.data.activeRide.src_lon) < 0.05)
      newState = state { data = state.data { activeRide{isDriverArrived = if ((state.props.currentStage == ST.RideAccepted || state.props.currentStage == ST.ChatWithCustomer) && not state.data.activeRide.notifiedCustomer) then isDriverNearBy else state.data.activeRide.isDriverArrived},currentDriverLat= getLastKnownLocValue ST.LATITUDE lat,  currentDriverLon = getLastKnownLocValue ST.LONGITUDE lng, locationLastUpdatedTime = (convertUTCtoISC time "hh:mm a") }}
  _ <- pure $ setValueToLocalStore IS_DRIVER_AT_PICKUP (show (newState.data.activeRide.isDriverArrived || newState.data.activeRide.notifiedCustomer))
  _ <- pure $ setValueToLocalStore LOCATION_UPDATE_TIME (convertUTCtoISC time "hh:mm a")
  continueWithCmd newState [ do
    _ <- if (getValueToLocalNativeStore IS_RIDE_ACTIVE == "false") then checkPermissionAndUpdateDriverMarker newState else pure unit
    pure AfterRender
    ]

eval (RideActiveAction activeRide) state = updateAndExit state { data {activeRide = activeRideDetail state activeRide}} $ UpdateStage ST.RideAccepted state { data {activeRide = activeRideDetail state activeRide}}

eval RecenterButtonAction state = continue state

eval (SwitchDriverStatus status) state = do
  if ((getValueToLocalStore IS_DEMOMODE_ENABLED) == "true") then do
    continueWithCmd state [ do
          _ <- pure $ setValueToLocalStore IS_DEMOMODE_ENABLED "false"
          _ <- pure $ toast (getString DEMO_MODE_DISABLED)
          _ <- pure $  deleteValueFromLocalStore DEMO_MODE_PASSWORD
          _ <- getCurrentPosition (showDriverMarker state "ny_ic_auto") constructLatLong
          pure NoAction
          ]
  else if state.props.driverStatusSet == status then continue state
    else
      case status of
        ST.Online -> exit (DriverAvailabilityStatus state status)
        ST.Silent -> exit (DriverAvailabilityStatus state status)
        ST.Offline ->
          do
            let checkIfLastWasSilent = state.props.driverStatusSet == ST.Silent
            continue state { props { goOfflineModal = checkIfLastWasSilent, silentPopUpView = not checkIfLastWasSilent }}

eval (PopUpModalSilentAction (PopUpModal.OnButton1Click)) state = exit (DriverAvailabilityStatus state{props{silentPopUpView = false}} ST.Offline)
eval (PopUpModalSilentAction (PopUpModal.OnButton2Click)) state = exit (DriverAvailabilityStatus state{props{silentPopUpView = false}} ST.Silent)

eval GoToProfile state =  do
  _ <- pure $ setValueToLocalNativeStore PROFILE_DEMO "false"
  _ <- pure $ hideKeyboardOnNavigation true
  exit $ GoToProfileScreen state
eval ClickAddAlternateButton state = do
    if state.props.showlinkAadhaarPopup then
      exit $ AadhaarVerificationFlow state
    else do
      let curr_time = getCurrentUTC ""
      let last_attempt_time = getValueToLocalStore SET_ALTERNATE_TIME
      let time_diff = differenceBetweenTwoUTC curr_time last_attempt_time
      if(time_diff <= 600) then do
        pure $ toast $ getString TOO_MANY_ATTEMPTS_PLEASE_TRY_AGAIN_LATER
        continue state
      else do
        exit $ AddAlternateNumber state


eval ZoneOtpAction state = do
  continue state { props = state.props { enterOtpModal = true, rideOtp = "", enterOtpFocusIndex = 0, otpIncorrect = false } }

eval HelpAndSupportScreen state = exit $ GoToHelpAndSupportScreen state

eval (GenderBannerModal (Banner.OnClick)) state = do
  _ <- pure $ firebaseLogEvent "ny_driver_gender_banner_click"
  exit $ GotoEditGenderScreen

eval (StatsModelAction StatsModelController.OnIconClick) state = continue state { data {activeRide {waitTimeInfo =false}}, props { showBonusInfo = not state.props.showBonusInfo } }

eval (RequestInfoCardAction RequestInfoCard.Close) state = continue state { data {activeRide {waitTimeInfo =false}}, props { showBonusInfo = false } }

eval (RequestInfoCardAction RequestInfoCard.BackPressed) state = continue state { data {activeRide {waitTimeInfo =false}}, props { showBonusInfo = false } }

eval (RequestInfoCardAction RequestInfoCard.NoAction) state = continue state

eval (GenderBannerModal (Banner.OnClick)) state = exit $ GotoEditGenderScreen

eval RemovePaymentBanner state = if state.data.paymentState.blockedDueToPayment then
                                                  continue state else continue state {data { paymentState {paymentStatusBanner = false}}}
eval (LinkAadhaarPopupAC PopUpModal.OnButton1Click) state = exit $ AadhaarVerificationFlow state

eval (LinkAadhaarPopupAC PopUpModal.DismissPopup) state = continue state {props{showAadharPopUp = false}}

eval RemoveGenderBanner state = do
  _ <- pure $ setValueToLocalStore IS_BANNER_ACTIVE "False"
  continue state { props = state.props{showGenderBanner = false}}

eval (PaymentStatusAction status) state =
  case status of
    Common.CHARGED -> continue state { data { paymentState { paymentStatusBanner = false}}}
    _ -> continue state { data { paymentState {
                  paymentStatus = Common.Failed,
                  bannerBG = Color.pearl,
                  bannerTitle = getString YOUR_PREVIOUS_PAYMENT_IS_PENDING,
                  bannerTitleColor = Color.dustyRed,
                  banneActionText = getString CONTACT_SUPPORT,
                  bannerImage = "ny_ic_payment_failed_banner," }}}

eval _ state = continue state

checkPermissionAndUpdateDriverMarker :: ST.HomeScreenState -> Effect Unit
checkPermissionAndUpdateDriverMarker state = do
  conditionA <- isLocationPermissionEnabled unit
  conditionB <- isLocationEnabled unit
  if conditionA && conditionB then do
    _ <- pure $ printLog "update driver location" "."
    _ <- getCurrentPosition (showDriverMarker state "ic_vehicle_side") constructLatLong
    pure unit
    else do
      _ <- requestLocation unit
      pure unit

showDriverMarker :: ST.HomeScreenState -> String -> ST.Location -> Effect Unit
showDriverMarker state marker location = do
  case (getValueToLocalStore DEMO_MODE_PASSWORD) of
    "7891234" -> updateAutoIcon 13.311895563147432 76.93981481869986
    "8917234" -> updateAutoIcon 13.260559676317829 76.4785809882692
    "9178234" -> updateAutoIcon 13.160550263780683 76.66727044721313
    "1789234" -> updateAutoIcon 12.522069908884921 76.89518072273476
    _ -> do
      _ <- pure $ enableMyLocation true
      animateCamera location.lat location.lon 17 "ZOOM"

updateAutoIcon :: Number -> Number -> Effect Unit
updateAutoIcon lat lng = do
  _ <- showMarker "ic_vehicle_side" lat lng 100 0.5 0.5
  _ <- pure $ enableMyLocation true
  animateCamera lat lng 17 "ZOOM"

constructLatLong :: String -> String -> ST.Location
constructLatLong lat lon =
  { lat: fromMaybe 0.0 (Number.fromString lat)
  , lon : fromMaybe 0.0 (Number.fromString lon)
  , place : ""
  }

activeRideDetail :: ST.HomeScreenState -> RidesInfo -> ST.ActiveRide
activeRideDetail state (RidesInfo ride) = {
  id : ride.id,
  source : (decodeAddress ride.fromLocation true),
  destination : (decodeAddress ride.toLocation true),
  src_lat :  ((ride.fromLocation) ^. _lat),
  src_lon :  ((ride.fromLocation) ^. _lon),
  dest_lat: ((ride.toLocation) ^. _lat),
  dest_lon: ((ride.toLocation) ^. _lon),
  actualRideDistance : fromMaybe 0.0 (Number.fromString (parseFloat (toNumber( fromMaybe 0 ride.chargeableDistance)) 2)),
  status : case ride.status of
              "NEW" -> NEW
              "INPROGRESS" -> INPROGRESS
              "COMPLETED" -> COMPLETED
              "CANCELLED" -> CANCELLED
              _ -> COMPLETED,
  distance : (toNumber ride.estimatedDistance),
  duration : state.data.activeRide.duration,
  riderName : fromMaybe "" ride.riderName,
  estimatedFare : ride.driverSelectedFare + ride.estimatedBaseFare,
  isDriverArrived : state.data.activeRide.isDriverArrived,
  notifiedCustomer : if (differenceBetweenTwoUTC ride.updatedAt ride.createdAt) == 0 then false else true,
  exoPhone : ride.exoPhone,
  specialLocationTag : ride.specialLocationTag,
  waitingTime : if (getValueToLocalStore IS_WAIT_TIMER_STOP) == "Stop" && state.props.timerRefresh then (getValueToLocalStore SET_WAITING_TIME) else state.data.activeRide.waitingTime,
  rideCreatedAt : ride.createdAt,
  waitTimeInfo : state.data.activeRide.waitTimeInfo,
  requestedVehicleVariant : ride.requestedVehicleVariant
}

cancellationReasons :: String -> Array Common.OptionButtonList
cancellationReasons dummy = [
        {
          reasonCode: "VEHICLE_ISSUE"
        , description: (getString VEHICLE_ISSUE)
        , textBoxRequired : false
        , subtext: Nothing
        },
        {
          reasonCode: "PICKUP_TOO_FAR"
        , description: (getString PICKUP_TOO_FAR)
        , textBoxRequired : false
        , subtext: Nothing
        },
        {
          reasonCode: "CUSTOMER_NOT_PICKING_CALL"
        , description: (getString CUSTOMER_NOT_PICKING_CALL)
        , textBoxRequired : false
        , subtext: Nothing
        },
        {
          reasonCode: "TRAFFIC_JAM"
        , description: (getString TRAFFIC_JAM)
        , textBoxRequired : false
        , subtext: Nothing
        },
        {
          reasonCode: "CUSTOMER_WAS_RUDE"
        , description: (getString CUSTOMER_WAS_RUDE)
        , textBoxRequired : false
        , subtext: Nothing
        },
        {
          reasonCode: "OTHER"
        , description: (getString OTHER)
        , textBoxRequired : true
        , subtext: Nothing
        }
]

dummyCancelReason :: Common.OptionButtonList
dummyCancelReason =  {
        reasonCode : ""
        , description :""
        , textBoxRequired : false
        , subtext : Nothing
        }

checkNotificationType :: String -> ST.NotificationType -> Boolean
checkNotificationType currentNotification requiredNotification = (show requiredNotification) == currentNotification

type RideRequestPollingData = {
    duration :: Int ,
    delay :: Number
  }

getLastKnownLocValue :: ST.LocationType -> String -> Number
getLastKnownLocValue lType val =
  let lastKnownValue = fromMaybe 0.0 $ Number.fromString $ getValueToLocalNativeStore $ if lType == ST.LATITUDE then LAST_KNOWN_LAT else LAST_KNOWN_LON
      currentVal = if (fromMaybe 0.0 (Number.fromString val)) == 0.0 then Nothing else (Number.fromString val)
    in fromMaybe lastKnownValue currentVal

updateMessagesWithCmd :: ST.HomeScreenState -> Eval Action ScreenOutput ST.HomeScreenState
updateMessagesWithCmd state =
  continueWithCmd state [ do
    if(state.props.currentStage == ST.ChatWithCustomer) then do
      _ <- pure $ setValueToLocalNativeStore READ_MESSAGES (show (Array.length state.data.messages))
      pure unit
    else
      pure unit
    pure NoAction
    ]
