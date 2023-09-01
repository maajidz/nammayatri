{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.NammaSafetyScreen.View
  where

import Screens.NammaSafetyScreen.ComponentConfig

import Animation (screenAnimation)
import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.GenericHeader as GenericHeader
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.StepsHeaderModel as StepsHeaderModel
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Transformers.Back.Trans (runBackT)
import Data.Array (any, length, mapWithIndex, null, take, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as DS
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (countDown, getNewIDWithTag, os, safeMarginBottom, safeMarginTop, screenWidth)
import Engineering.Helpers.Commons as EHC
import Engineering.Helpers.Utils (loaderText, toggleLoader)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getAssetStoreLink, setRefreshing, storeCallBackContacts)
import JBridge (getLayoutBounds, setupCamera, startRecord, startTimerWithTime)
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.DefaultConfig as DC
import Prelude (Unit, bind, const, discard, map, pure, show, unit, ($), (&&), (-), (/), (/=), (<<<), (<>), (==), (||), (*), (<))
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, Visibility(..), afterRender, alignParentBottom, background, color, cornerRadius, editText, fontStyle, frameLayout, gravity, height, hint, id, imageUrl, imageView, imageWithFallback, linearLayout, margin, onBackPressed, onChange, onClick, onRefresh, onScroll, onScrollStateChange, orientation, padding, pattern, relativeLayout, scrollView, stroke, swipeRefreshLayout, text, textFromHtml, textSize, textView, visibility, weight, width)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Elements.Keyed as Keyed
import PrestoDOM.List as PrestoList
import Screens.NammaSafetyScreen.Controller (Action(..), ScreenOutput, contactColorsList, eval)
import Screens.Types (NammaSafetyStage(..), NewContacts, RecordingState(..), StepsHeaderModelState, NammaSafetyScreenState)
import Services.API (GetSosDetailsRes(..))
import Services.Backend as Remote
import Storage (getValueToLocalStore, KeyStore(..))
import Styles.Colors as Color
import Types.App (defaultGlobalState)

screen :: NammaSafetyScreenState -> PrestoList.ListItem -> Screen Action NammaSafetyScreenState ScreenOutput
screen initialState listItemm =
  { initialState
  , view : view listItemm
  , name : "NammaSafetyScreen"
  , globalEvents : [(\push -> do
                      _ <- launchAff $ EHC.flowRunner defaultGlobalState $ runExceptT $ runBackT $ do
                        lift $ lift $ loaderText (getString LOADING) (getString PLEASE_WAIT_WHILE_IN_PROGRESS)
                        lift $ lift $ toggleLoader true
                        
                        (GetSosDetailsRes sosDetails) <- Remote.getSosDetails initialState.data.rideId
                        case sosDetails.sosId of
                            Just id -> do
                                lift $ lift $ doAff do liftEffect $ push $ UpdateSosId id
                                pure unit
                            Nothing -> do
                                response <- Remote.getEmergencySettingsBT ""
                                lift $ lift $ doAff do liftEffect $ push $ UpdateEmergencySettings response
                                pure unit
                        
                        lift $ lift $ toggleLoader false  
                        
                        if initialState.data.sosId /= "" then lift $ lift $ doAff do liftEffect $ push $ SwitchToStage TriggeredNammaSafety else pure unit
                        pure unit
                      pure $ pure unit
                    )
                  ]
  , eval : \action state -> do
    --   let _ = spy "NammaSafety action " action
    --   let _ = spy "NammaSafety state " state
      eval action state
  }

listItem1 :: NewContacts
listItem1 = {
  name: "",
  number: "",
  isSelected: false
}

view
  :: forall w. PrestoList.ListItem -> (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
view listItemm push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , orientation VERTICAL
, background if any (_ == state.props.currentStage) [ActivateNammaSafety, TriggeredNammaSafety, NammaSafetyVideoRecord]
                then Color.black900
             else Color.white900
    ]
    [ headerView state push
    , frameLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , onBackPressed push (const BackPressed)
      ]
      [ 
        -- case state.props.currentStage of
        --     NammaSafetyDashboard -> dashboardView state push
        --     EmergencyContactsStage -> emergencyContactsView listItemm push state
        --     AboutNammaSafety -> aboutNammaSafetyView state push
        --     SetTriggerCustomerSupport -> settingUpView state push
        --     SetNightTimeSafetyAlert -> settingUpView state push
        --     SetDefaultEmergencyContacts -> settingUpView state push
        --     SetPersonalSafetySettings -> settingUpView state push
        --     EduNammaSafetyMeasures -> educationView state
        --     EduNammaSafetyGuidelines -> educationView state
        --     EduNammaSafetyAboutSOS -> educationView state
        --     ActivateNammaSafety -> activateNammaSafetyView state push
        --     TriggeredNammaSafety -> sosActiveView state push
        --     NammaSafetyVideoRecord -> if state.props.recordingState == SHARED
        --                                 then videoSharedView push state
        --                               else videoRecordSOSView state push
        if state.props.currentStage == NammaSafetyDashboard then dashboardView state push else linearLayout [] []
        , if state.props.currentStage == EmergencyContactsStage then emergencyContactsView listItemm push state else linearLayout [] []
        , if state.props.currentStage == AboutNammaSafety then aboutNammaSafetyView state push else linearLayout [] []
        , if state.props.currentStage == SetTriggerCustomerSupport || state.props.currentStage == SetNightTimeSafetyAlert || state.props.currentStage == SetDefaultEmergencyContacts || state.props.currentStage == SetPersonalSafetySettings then settingUpView state push else linearLayout [] []
        , if state.props.currentStage == EduNammaSafetyMeasures || state.props.currentStage == EduNammaSafetyGuidelines || state.props.currentStage == EduNammaSafetyAboutSOS then educationView state else linearLayout [] []
        , if state.props.currentStage == ActivateNammaSafety then activateNammaSafetyView state push else linearLayout [] []
        , if state.props.currentStage == TriggeredNammaSafety then sosActiveView state push else linearLayout [] []
        , if state.props.currentStage == NammaSafetyVideoRecord then do
                  if state.props.recordingState == SHARED
                    then videoSharedView push state
                    else videoRecordSOSView state push
          else linearLayout [] []
        , if state.props.emergencyContactsProps.showInfoPopUp == true then removeContactPopUpView push state else emptyTextView state
        , if state.props.confirmPopup then PopUpModal.view (push <<< ConfirmSOSActivate) (confirmPopUpModelConfig state) else emptyTextView state
      ]
    ]

-- ---------------------------------- dashboardView -----------------------------------

dashboardView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
dashboardView state push =
  Anim.screenAnimation $ relativeLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , background Color.white900
    , orientation VERTICAL
    ]
    [ linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ]
      [ linearLayout
        [ height $ V 1
        , width MATCH_PARENT
        , background Color.greySmoke
        ]
        []
      , if state.data.hasCompletedSafetySetup == false && state.props.onRide == false
        then nammaSafetyFeaturesView state push
        else userSettingsView state push
      ]
    ]

-- ---------------------------------- headerView -----------------------------------

headerView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
headerView state push =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , visibility $ if any (_ == state.props.currentStage) [SetTriggerCustomerSupport, SetNightTimeSafetyAlert, SetDefaultEmergencyContacts, SetPersonalSafetySettings]
                   || state.props.recordingState == SHARED
                   then GONE
                   else VISIBLE
    ]
    [ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER_VERTICAL
      ]
      [ linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , weight 1.0
        ]
        [ GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig (getHeaderTitle state.props.currentStage) state)
        ]
      , textView
      [ text $ getString LEARN_MORE
      , visibility $ if (state.props.currentStage == NammaSafetyDashboard && state.data.hasCompletedSafetySetup == true || state.props.currentStage == ActivateNammaSafety)
                      then VISIBLE
                      else GONE
      , color Color.blue900
      , gravity RIGHT
      , margin $ MarginRight 16
      , onClick push $ const $ SwitchToStage AboutNammaSafety
      ]
    ]
    , linearLayout
      [ height $ V 1
      , width MATCH_PARENT
      , background Color.greySmoke
      ]
      []
    ]

-- ---------------------------------- aboutNammaSafetyView -----------------------------------

aboutNammaSafetyView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
aboutNammaSafetyView state push =
  relativeLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ]
    [ linearLayout
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , orientation VERTICAL
      ]
      [ textView
        [ text $ getString LEARN_ABOUT_NAMMA_SAFETY
        , width MATCH_PARENT
        , height WRAP_CONTENT
        , color Color.black700
        , background Color.blue600
        , gravity LEFT
        , padding $ Padding 12 16 12 16
        ]
      , cardView state (getCardViewData 0) push
      , cardView state (getCardViewData 1) push
      , cardView state (getCardViewData 2) push
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      , background Color.white900
      , alignParentBottom "true,-1"
      ]
      [ PrimaryButton.view (push <<< StartNammaSafetyOnboarding) (startNSOnboardingButtonConfig state) ]
    ]

-- ---------------------------------- nammaSafetyFeaturesView -----------------------------------

nammaSafetyFeaturesView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
nammaSafetyFeaturesView state push =
  relativeLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ]
    [ featuresView state push
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation HORIZONTAL
      , background Color.white900
      , alignParentBottom "true,-1"
      ]
      [ PrimaryButton.view (push <<< StartNammaSafetyOnboarding) (startNSOnboardingButtonConfig state) ]
    ]


featuresView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
featuresView state push = 
    linearLayout[
      width MATCH_PARENT
      , height WRAP_CONTENT
      , background if state.props.currentStage == ActivateNammaSafety then "#373A45" else Color.blue600
      , gravity CENTER
      , orientation VERTICAL
      , cornerRadius 12.0
      , margin $ Margin 16 20 16 0
      , stroke $ if state.props.currentStage == ActivateNammaSafety then "1," <> Color.black700 else "1," <> Color.blue600
    ][ imageView [
            imageWithFallback "ny_ic_safety_shield, https://assets.juspay.in/nammayatri/images/common/ny_ic_circle.png"
            , width $ V 220
            , height $ V 114
        ]
        , textView $ [
                text $ getString NAMMA_SAFETY_WILL_ENABLE_ACCESS
                , margin $ Margin 16 20 16 4
                , color if state.props.currentStage == ActivateNammaSafety then Color.white900 else Color.black800
            ] <> FontStyle.subHeading1 TypoGraphy
        , imageWithTextView (getString SHARE_LOCATION_AND_RIDE_DETAILS_WITH_NAMMA_YATRI_SUPPORT_TEAM) true state.props.currentStage
        , imageWithTextView (getString ACTIVATE_LIVE_VIDEO_RECORDING_FEATURES) true state.props.currentStage
        , imageWithTextView (getString GET_OPTIONS_TO_DIRECTLY_CALL_SUPPORT) true state.props.currentStage
        , imageWithTextView (getString SHARE_LOCATION_AND_RIDE_DETAILS_EMERGENCY_CONTACT) (state.data.shareToEmergencyContacts || state.props.currentStage /= ActivateNammaSafety) state.props.currentStage
        , linearLayout [
            height WRAP_CONTENT
            , width MATCH_PARENT
            , visibility if state.props.currentStage == ActivateNammaSafety && state.data.shareToEmergencyContacts then VISIBLE else GONE
            , gravity LEFT
            , margin $ MarginVertical 11 0
            , padding $ PaddingLeft 44
            ](mapWithIndex (\index item -> contactCircleView item index) state.data.emergencyContactsData.contactsList)
        , linearLayout [
            height $ V 1
            , width MATCH_PARENT
            , margin $ Margin 16 12 16 16
            , background Color.black700
        ][]
        , linearLayout [
            height MATCH_PARENT
            , width MATCH_PARENT
        ][ textView $ [
            textFromHtml if state.props.currentStage == ActivateNammaSafety then "<u>" <> getString EDIT_ACTIONS <>"</u>" else "<u>"<> getString LEARN_MORE <>"</u>"
            , color Color.blue800
            , margin $ MarginBottom 20
            , onClick push $ const $ SwitchToStage $ if state.props.currentStage == ActivateNammaSafety then NammaSafetyDashboard else AboutNammaSafety
        ] <> FontStyle.body1 TypoGraphy
        ]
    ]

imageWithTextView :: String -> Boolean -> NammaSafetyStage -> forall w . PrestoDOM (Effect Unit) w
imageWithTextView text' isActive stage = 
        linearLayout [
            height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation HORIZONTAL
            , padding $ PaddingHorizontal 16 16
            , margin $ MarginVertical 16 0
        ][ imageView [
                imageWithFallback if isActive then "ny_ic_check, https://assets.juspay.in/nammayatri/images/common/ny_ic_circle.png" else "ny_ic_ellipse_outline_grey, https://assets.juspay.in/nammayatri/images/common/ny_ic_circle.png"
                , height $ V 20
                , width $ V 20
                , margin $ MarginRight 8
            ]
            , textView $ [
                text text'
                , color if stage == ActivateNammaSafety && isActive then Color.white900 
                        else if stage == ActivateNammaSafety then Color.black700
                        else Color.black800
            ] <> FontStyle.tags TypoGraphy
        ]

activateNammaSafetyView :: NammaSafetyScreenState  -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
activateNammaSafetyView state push = 
    relativeLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ][ featuresView state push
        , linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , orientation VERTICAL
              , alignParentBottom "true,-1"
              ][  PrimaryButton.view (push <<< ActivateSOS) (activateSoSButtonConfig state),
                PrimaryButton.view (push <<< DismissSOS) (dismissSoSButtonConfig state) ]
    ]

cardView:: NammaSafetyScreenState  -> CardViewDataType -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
cardView state cardData push = 
  linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation HORIZONTAL
    , padding (Padding 16 16 16 16)
    , margin (Margin 16 16 16 0)
    , stroke ("1,"<>Color.grey900)
    , cornerRadius 8.0
    , gravity CENTER_VERTICAL
    , onClick push $ const $ SwitchToStage cardData.stage
    ][ 
            imageView
            [ imageWithFallback cardData.image
            , height $ V 60
            , margin $ MarginRight 14
            , width $ V 60
            ]
            , textView $ [
                text cardData.text
                , gravity CENTER_VERTICAL
                , color Color.black800
            ] <> FontStyle.body6 TypoGraphy
    ]

type CardViewDataType = {
    stage :: NammaSafetyStage,
    image :: String,
    text :: String
}

getCardViewData :: Int -> CardViewDataType
getCardViewData index = case index of
    0 -> {stage : EduNammaSafetyMeasures , image : "ny_ic_namma_safety_measures, https://assets.juspay.in/nammayatri/images/common/ny_ic_circle.png", text : getString NAMMA_SAFETY_MEASURES}
    1 -> {stage : EduNammaSafetyGuidelines, image : "ny_ic_namma_safety_guidlines, https://assets.juspay.in/nammayatri/images/common/ny_ic_circle.png", text : getString SAFETY_GUIDELINES_FOR_YOU}
    2 -> {stage : EduNammaSafetyAboutSOS, image : "ny_ic_about_sos_icon, https://assets.juspay.in/nammayatri/images/common/ny_ic_circle.png", text : getString ABOUT_SOS}
    _ -> {stage : EduNammaSafetyMeasures , image : "ny_ic_namma_safety_measures, https://assets.juspay.in/nammayatri/images/common/ny_ic_circle.png", text : getString NAMMA_SAFETY_MEASURES}

userSettingsView:: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
userSettingsView state push=
    relativeLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ][ 
    linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        
        ][ 
            linearLayout [
            width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL, gravity LEFT
            , padding $ Padding 16 16 16 16]
            [
                textView $ [ text $ getString EMERGENCY_ACTIONS
                , color Color.black900
                ] <> FontStyle.subHeading1 TypoGraphy
                , textView [ text $ getString WHEN_YOU_START_EMERGENCY_SOS
                , color Color.black700
                , textSize FontSize.a_12
                , fontStyle $ FontStyle.semiBold LanguageStyle ] 
                
            ]
            , toggleSwitchViewLayout SetDefaultEmergencyContacts state.data.shareToEmergencyContacts (getString EMERGENCY_SHARING_WITH_CONTACTS) push
            , linearLayout [
                width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            , padding $ Padding 16 8 16 0

            ]
            [
                textView [ text $ getString SHARING_WITH
                , height MATCH_PARENT
                , color Color.black700
                , textSize FontSize.a_12
                , margin $ MarginRight 8   
                , gravity CENTER
                , fontStyle $ FontStyle.medium LanguageStyle ] 
                , linearLayout [](mapWithIndex (\index item -> contactCircleView item index) state.data.emergencyContactsData.contactsList)
                , PrimaryButton.view (push <<< EditEmergencyContacts) (editEmergencyContactsBtnConfig state)
                
            ]
            , separatorView
            , toggleSwitchViewLayout SetTriggerCustomerSupport state.data.triggerNYSupport (getString TRIGGER_ALERT_TO_NAMMA_YATRI_SUPPORT) push
            , separatorView
            , toggleSwitchViewLayout SetNightTimeSafetyAlert state.data.nightTimeSafety (getString NIGHT_TIME_SAFETY_CHECKS) push
        ]
    ]

toggleSwitchViewLayout ::  NammaSafetyStage -> Boolean -> String -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
toggleSwitchViewLayout stage isActive text' push =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation HORIZONTAL
  , margin $ Margin 16 0 16 0
  ][  
    textView [ 
        text text'
        , weight 1.0
        , color Color.black800
        , textSize FontSize.a_14
        , fontStyle $ FontStyle.semiBold LanguageStyle ]
    , toggleSwitchView isActive stage push
  ]

contactCircleView ::  NewContacts -> Int -> forall w. PrestoDOM (Effect Unit) w
contactCircleView contact index =
  linearLayout
    [ height $ V 32
    , width $ V 32
    , background (fromMaybe "" (fromMaybe [] (contactColorsList !! index) !! 0))
    , cornerRadius if EHC.os == "IOS" then 12.0 else 20.0
    , gravity CENTER
    , margin (MarginHorizontal 5 5)
    ]
    [ textView
        [ text (DS.toUpper ((<>) (getFirstChar contact.name) (getLastChar contact.name)))
        , color (fromMaybe "" (fromMaybe [] (contactColorsList !! index) !! 1))
        , textSize FontSize.a_12
        ]
    ]

getNameInitials :: String -> (Array String)
getNameInitials fullName = (take 2 (DS.split (DS.Pattern " ") (fullName)))

getFirstChar :: String -> String
getFirstChar name = DS.take 1 (fromMaybe "" ((getNameInitials name) !! 0))

getLastChar :: String -> String
getLastChar name = DS.take 1 (fromMaybe "" ((getNameInitials name) !! 1))
  
-- ---------------------------------- settingUpView -----------------------------------
settingUpView:: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
settingUpView state push=
  Anim.screenAnimation $ relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.white900
  , orientation VERTICAL
  , padding $ Padding 0 EHC.safeMarginTop 0 (if EHC.safeMarginBottom == 0 && EHC.os == "IOS" then 24 else EHC.safeMarginBottom)
  ][  PrestoAnim.animationSet
        [ Anim.fadeIn true
        ] $ settingUpContentView (settingUpContentViewData state) state push    
  ]

settingUpContentView :: ContentViewDataType -> NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
settingUpContentView config state push = 
    relativeLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ][ linearLayout [
        height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
    ][
        StepsHeaderModel.view (push <<< StepsHeaderModelAC)  (stepsHeaderData config.step)
        , linearLayout [
            height WRAP_CONTENT
            , width MATCH_PARENT
            , padding (Padding 16 32 16 16)
            , orientation VERTICAL
        ][
            linearLayout [
                height WRAP_CONTENT
                , width MATCH_PARENT
            ][
                linearLayout [
                    weight 1.0
                ][
                    imageView
                        [ imageWithFallback config.image
                        , height $ V 50
                        , margin (Margin 0 0 14 0)
                        , width $ V 50
                        , visibility if config.image /= "" then VISIBLE else GONE
                        ]
                ]
                , toggleSwitchView config.isActive state.props.currentStage push
            ]
            , textView [
                width WRAP_CONTENT
                , height MATCH_PARENT
                , text config.title
                -- , gravity CENTER_VERTICAL
                , color Color.black900
                , textSize FontSize.a_24
                , fontStyle $ FontStyle.bold LanguageStyle
            ]
            , textView $ [
                width WRAP_CONTENT
                , height MATCH_PARENT
                , text config.desc
                , color Color.black700
            ] <> FontStyle.body5 TypoGraphy
            , linearLayout [
                width MATCH_PARENT
                , height WRAP_CONTENT
                , orientation VERTICAL
                , visibility if state.props.currentStage == SetDefaultEmergencyContacts then VISIBLE else GONE
                , afterRender
                    ( \action -> do
                        _ <- push action
                        _ <- storeCallBackContacts push ContactsCallback
                        if ((getValueToLocalStore CONTACTS == "__failed") || (getValueToLocalStore CONTACTS == "(null)")) then do
                            _ <- push FetchContacts
                            pure unit
                        else do
                            pure unit
                        _ <- pure $ setRefreshing (getNewIDWithTag "EmergencyContactTag") false
                        pure unit
                    )
                    (const NoAction)
            ](mapWithIndex (\index item -> contactCardView push state item index) state.data.emergencyContactsData.contactsList)
            , linearLayout [
                width MATCH_PARENT
                , height WRAP_CONTENT
                , stroke $ "1,"<> Color.grey900
                , padding $ Padding 16 16 16 16
                , visibility if state.props.currentStage == SetDefaultEmergencyContacts && length state.data.emergencyContactsData.contactsList /= 3 then VISIBLE else GONE
                , cornerRadius 8.0
                , margin $ MarginTop 12
                , onClick push $ const $ AddContacts
            ][
                imageView [
                    imageWithFallback "ny_ic_add_filled,https://assets.juspay.in/nammayatri/images/user/ny_ic_emergency_contacts.png"
                    , height $ V 24
                    , width $ V 24
                    , margin $ MarginRight 12
                ]
                , textView $ [
                    text $ getString ADD_A_CONTACT
                    , color Color.blue900
                ] <> FontStyle.subHeading1 TypoGraphy
            ]

        ]
    ]
    , linearLayout
      [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background Color.white900
        , alignParentBottom "true,-1"
      ][  PrimaryButton.view (push <<< GoToNextStep) (continueNextStepButtonConfig state) ]
    ]

stepsHeaderData :: Int -> StepsHeaderModelState
stepsHeaderData currentIndex = 
    {
        activeIndex : currentIndex,
        textArray : [getString SET_UP_YOUR_PERSONAL_SAFETY_SETTINGS, getString SET_UP_YOUR_PERSONAL_SAFETY_SETTINGS, getString SET_UP_YOUR_PERSONAL_SAFETY_SETTINGS, getString SET_UP_YOUR_PERSONAL_SAFETY_SETTINGS],
        backArrowVisibility : true,
        config : DC.config
    }

type ContentViewDataType = {
  title :: String,
  desc :: String,
  image :: String,
  step :: Int,
  isActive :: Boolean
}

settingUpContentViewData :: NammaSafetyScreenState -> ContentViewDataType
settingUpContentViewData state = case state.props.currentStage of
    SetDefaultEmergencyContacts ->  {title: getString SHARE_INFO_WITH_EMERGENCY_CONTACTS_TITLE, desc: getString SHARE_INFO_WITH_EMERGENCY_CONTACTS_DESC, image: "ny_ic_share,https://assets.juspay.in/nammayatri/images/user/ny_ic_emergency_contacts.png", step : 0, isActive : state.data.shareToEmergencyContacts && length state.data.emergencyContactsData.contactsList /= 0}
    SetTriggerCustomerSupport ->  {title: getString TRIGGER_ALERT_TO_NAMMAYATRI_SUPPORT_TITLE, desc: getString TRIGGER_ALERT_TO_NAMMAYATRI_SUPPORT_DESC, image: "ny_ic_ny_support,https://assets.juspay.in/nammayatri/images/user/ny_ic_emergency_contacts.png", step : 1, isActive : state.data.triggerNYSupport}
    SetNightTimeSafetyAlert ->  {title: getString ENABLE_NIGHT_TIME_SAFETY_ALERTS_TITLE, desc: getString ENABLE_NIGHT_TIME_SAFETY_ALERTS_DESC, image: "ny_ic_night_safety,https://assets.juspay.in/nammayatri/images/user/ny_ic_emergency_contacts.png", step : 2, isActive : state.data.nightTimeSafety}
    SetPersonalSafetySettings ->  {title: getString ALMOST_DONE_TITLE, desc: getString ALMOST_DONE_DESC, image: "", step : 3, isActive : state.data.triggerNYSupport}
    _ -> {title:"", desc:"", image:"", step : 5, isActive : false}

------------------- separator -------------------
separatorView ::  forall w . PrestoDOM (Effect Unit) w
separatorView =
  linearLayout
  [ height (V 1)
    , width MATCH_PARENT
    , margin (Margin 16 16 16 16)
    , background Color.lightGreyShade
  ][]

educationView:: NammaSafetyScreenState -> forall w . PrestoDOM (Effect Unit) w
educationView state =
  Anim.screenAnimation $ relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.white900
  , orientation VERTICAL
  , padding $ Padding 0 EHC.safeMarginTop 0 (if EHC.safeMarginBottom == 0 && EHC.os == "IOS" then 24 else EHC.safeMarginBottom)
  ][  scrollView
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ][
        if state.props.currentStage == EduNammaSafetyMeasures then nammaSafetyMeasuresView state
          else if state.props.currentStage == EduNammaSafetyGuidelines then safetyGuidelinesView state
          else aboutSOSView state
        ]
   ]    

nammaSafetyMeasuresView :: NammaSafetyScreenState -> forall w . PrestoDOM (Effect Unit) w
nammaSafetyMeasuresView state = 
    linearLayout [
        height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
    ][
        imageView
            [ height $ V 210
            , width MATCH_PARENT
            , cornerRadius 20.0
            , margin $ Margin 16 16 16 16
            , imageWithFallback "ny_ic_safety_measures,user/nammaYatri/res/drawable/ny_ic_tick.png"
            ]
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , margin $ Margin 16 16 16 16
            , orientation VERTICAL
            ]
                (map (\item -> 
                    measureView item false true
                ) safetyMeasuresData) 
    ]

measureView :: String -> Boolean -> Boolean -> forall w . PrestoDOM (Effect Unit) w
measureView text' showBullet isCorrect = 
    linearLayout [
        height WRAP_CONTENT
        , width WRAP_CONTENT
        , margin $ MarginBottom 16
        , gravity CENTER
    ][
        textView $ [
            text "•"
            , visibility if showBullet then VISIBLE else GONE
            , gravity TOP_VERTICAL
            , height MATCH_PARENT
            , margin $ MarginRight 6
        ] <> FontStyle.body1 TypoGraphy
        , imageView [
            imageWithFallback if isCorrect then "ny_ic_tick,user/nammaYatri/res/drawable/ny_ic_tick.png" else "ny_ic_cross,user/nammaYatri/res/drawable/ny_ic_cross.png"
            , height $ V 20
            , width $ V 20
            , margin $ MarginRight 16
            , visibility if showBullet then GONE else VISIBLE
        ]
        , textView $ [
            text text'
            , color Color.black800
        ] <> FontStyle.body1 TypoGraphy
    ]

safetyMeasuresData :: Array String
safetyMeasuresData = [
    getString SAFETY_MEASURE_1,
    getString SAFETY_MEASURE_2,
    getString SAFETY_MEASURE_3,
    getString SAFETY_MEASURE_4,
    getString SAFETY_MEASURE_5,
    getString SAFETY_MEASURE_6
    ]

safetyGuidelinesView :: NammaSafetyScreenState -> forall w . PrestoDOM (Effect Unit) w
safetyGuidelinesView state = 
    linearLayout [
        height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
    ][
        imageView
            [ height $ V 210
            , width MATCH_PARENT
            , cornerRadius 20.0
            , margin $ Margin 16 16 16 16
            , imageWithFallback "ny_ic_safety_guidelines,user/nammaYatri/res/drawable/ny_ic_tick.png"
            ]
        , textView $ [
            text $ getString TO_ENSURE_SAFETY_USERS_SHOULD
            , margin $ Margin 16 16 16 0
        ] <> FontStyle.body1 TypoGraphy
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , margin $ Margin 16 16 16 0
            , orientation VERTICAL
            ]
                (map (\item -> 
                    measureView item false true
                ) safetyGuidelinesData) 
    ]

safetyGuidelinesData :: Array String
safetyGuidelinesData = [
    getString SAFETY_GUIDELINES_1,
    getString SAFETY_GUIDELINES_2,
    getString SAFETY_GUIDELINES_3,
    getString SAFETY_GUIDELINES_4,
    getString SAFETY_GUIDELINES_5,
    getString SAFETY_GUIDELINES_6,
    getString SAFETY_GUIDELINES_7
    ]

aboutSOSView :: NammaSafetyScreenState -> forall w . PrestoDOM (Effect Unit) w
aboutSOSView state = 
    linearLayout [
        height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
    ][
        imageView
            [ height $ V 210
            , width MATCH_PARENT
            , cornerRadius 20.0
            , margin $ Margin 16 16 16 16
            , imageWithFallback "ny_ic_about_sos,user/nammaYatri/res/drawable/ny_ic_tick.png"
            ]
        , textView $ [
            textFromHtml $ getString ABOUT_SOS_DESC
            , color Color.black800
            , margin $ Margin 16 16 16 0
        ] <> FontStyle.body1 TypoGraphy
        , textView $ [
            text $ getString FEW_EXAMPLES_OF_SOS_SITUATIONS
            , color Color.black800
            , margin $ Margin 16 16 16 0
        ] <> FontStyle.subHeading1 TypoGraphy
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , margin $ Margin 16 16 16 16
            , orientation VERTICAL
            ]
                (map (\item -> 
                    measureView item.text false item.isCorrect
                ) aboutSOSDataPoints) 
        , textView $ [
            text $ getString THINGS_TO_DO_DURING_SOS_SITUATION
            , color Color.black800
            , margin $ Margin 16 8 16 0
        ] <> FontStyle.subHeading1 TypoGraphy
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , margin $ Margin 16 16 16 16
            , orientation VERTICAL
            ] (map (\item -> 
                    measureView item true true
                ) aboutSoSData) 
    ]

aboutSOSDataPoints :: Array {text :: String, isCorrect :: Boolean}
aboutSOSDataPoints = [
    {text : getString ABOUT_SOS_1, isCorrect : true},
    {text : getString ABOUT_SOS_2, isCorrect : true},
    {text : getString ABOUT_SOS_3, isCorrect : true},
    {text : getString ABOUT_SOS_4, isCorrect : true},
    {text : getString ABOUT_SOS_5, isCorrect : false},
    {text : getString ABOUT_SOS_6, isCorrect : false},
    {text : getString ABOUT_SOS_7, isCorrect : false},
    {text : getString ABOUT_SOS_8, isCorrect : false}
    ]

aboutSoSData :: Array String
aboutSoSData = [
    getString ABOUT_SOS_9,
    getString ABOUT_SOS_10,
    getString ABOUT_SOS_11,
    getString ABOUT_SOS_12,
    getString ABOUT_SOS_13
]

sosActiveView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
sosActiveView state push = 
 Anim.screenAnimation $ relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , background Color.black900
  , color $ Color.white900
  , orientation VERTICAL
  , padding $ Padding 0 EHC.safeMarginTop 0 (if EHC.safeMarginBottom == 0 && EHC.os == "IOS" then 24 else EHC.safeMarginBottom)
  ][  linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ][ 
        linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , margin $ Margin 16 16 16 16
        , orientation VERTICAL
        ][
            textView [
                text $ getString EMERGENCY_REQUEST_SENT
                    , textSize FontSize.a_22
                    , margin $ MarginBottom 8
                    , fontStyle $ FontStyle.bold LanguageStyle
                    , color $ Color.white900
            ]
            , textView $ [
                text $ getString SOS_TRIGGERED_DESC
                    , margin $ MarginBottom 12
                    , color $ Color.white900
            ] <> FontStyle.body1 TypoGraphy
            , imageView [
                height $ V 250
                , width MATCH_PARENT
                , imageWithFallback "ny_ic_emergency_sent,https://assets.juspay.in/nammayatri/images/user/ny_ic_help.png"
                , margin $ MarginHorizontal 16 16
            ]
        ]
        , linearLayout 
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , alignParentBottom "true,-1"
            , margin $ Margin 16 16 16 16
            ][ textView $ [
                    text $ getString SOS_ACTIONS
                    , color Color.white900
                ] <> FontStyle.subHeading2 TypoGraphy
                , linearLayout [
                    height WRAP_CONTENT
                    , width MATCH_PARENT
                    , margin $ MarginVertical 8 8
                    , gravity CENTER
                    , orientation HORIZONTAL
                ][ linearLayout[
                        height WRAP_CONTENT
                        , width WRAP_CONTENT
                        , orientation VERTICAL
                        , gravity CENTER
                        , padding $ PaddingVertical 9 9
                        , cornerRadius 8.0
                        , background "#373A45"
                        , weight 1.0
                        , onClick push $ const $ CallForSupport "police"
                    ][
                        imageView [
                            imageWithFallback "ny_ic_police,https://assets.juspay.in/nammayatri/images/user/ny_ic_help.png"
                            , height $ V 26
                            , width $ V 26
                            , margin $ MarginBottom 8
                        ]
                        , textView $ [
                            textFromHtml $ getString CALL_POLICE
                            , gravity CENTER
                            , color Color.white900
                            , fontStyle $ FontStyle.semiBold LanguageStyle
                        ] <> FontStyle.paragraphText TypoGraphy
                    ]
                    , linearLayout[
                        height WRAP_CONTENT
                        , width WRAP_CONTENT
                        , orientation VERTICAL
                        , gravity CENTER
                        , margin $ MarginHorizontal 8 8
                        , padding $ PaddingVertical 9 9
                        , background "#373A45"
                        , cornerRadius 8.0
                        , weight 1.0
                        , onClick push $ const $ CallForSupport "ny_support"
                    ][
                        imageView [
                            imageWithFallback "ny_ic_support_unfilled,https://assets.juspay.in/nammayatri/images/user/ny_ic_help.png"
                            , height $ V 26
                            , width $ V 26
                            , margin $ MarginBottom 8
                        ]
                        , textView $ [
                            textFromHtml $ getString CALL_SUPPORT
                            , gravity CENTER
                            , color Color.white900
                            , fontStyle $ FontStyle.semiBold LanguageStyle
                            , cornerRadius 8.0
                        ] <> FontStyle.paragraphText TypoGraphy
                    ]
                    , linearLayout[
                        height WRAP_CONTENT
                        , width WRAP_CONTENT
                        , orientation VERTICAL
                        , gravity CENTER
                        , padding $ PaddingVertical 9 9
                        , cornerRadius 8.0
                        , background "#373A45"
                        , weight 1.0
                        , onClick push $ const $ SwitchToStage NammaSafetyVideoRecord
                    ][
                        imageView [
                            imageWithFallback "ny_ic_video,https://assets.juspay.in/nammayatri/images/user/ny_ic_help.png"
                            , height $ V 26
                            , width $ V 26
                            , margin $ MarginBottom 8
                        ]
                        , textView $ [
                            textFromHtml $ getString RECORD_VIDEO
                            , gravity CENTER
                            , color Color.white900
                            , fontStyle $ FontStyle.semiBold LanguageStyle
                            , cornerRadius 8.0
                        ] <> FontStyle.paragraphText TypoGraphy
                    ]
                ]
                , PrimaryButton.view (push <<< MarkRideAsSafe) (cancelSOSBtnConfig state)
            ]
        ]
   ]

videoRecordSOSView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
videoRecordSOSView state push = 
 let value = 15 - state.props.timerValue
     timerval = if (value < 10) then "0"<> show value else show value
 in
    Anim.screenAnimation $ relativeLayout
    [ height MATCH_PARENT
      , width MATCH_PARENT
      , background Color.black900
      , color $ Color.white900
      , orientation VERTICAL
      , padding $ Padding 0 EHC.safeMarginTop 0 (if EHC.safeMarginBottom == 0 && EHC.os == "IOS" then 24 else EHC.safeMarginBottom)
    ][  linearLayout [
            height WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
        ][ linearLayout[
                height MATCH_PARENT
                , width MATCH_PARENT
                , orientation VERTICAL
            ][  linearLayout 
                  [
                    height WRAP_CONTENT
                    , width MATCH_PARENT
                    , padding $ Padding 16 20 16 5
                  ][  linearLayout 
                        [
                            id $ EHC.getNewIDWithTag "VideoCamView"
                            , afterRender (\action -> do
                                pure $ setupCamera (getNewIDWithTag "VideoCamView"))(const NoAction)
                            , height MATCH_PARENT
                            , width MATCH_PARENT
                            ,  orientation VERTICAL
                        ][
                            linearLayout 
                            [
                                height MATCH_PARENT
                                , width MATCH_PARENT
                                , gravity CENTER_VERTICAL
                            ][
                                textView [
                                    text "Loading..."
                                    , gravity CENTER
                                    , color Color.white900
                                ]
                            ]
                        ]
                    ]
                , linearLayout[
                    height $ V 4
                    , width MATCH_PARENT
                    , background Color.white900
                    , margin $ MarginHorizontal 16 16
                    , id $ EHC.getNewIDWithTag "recordProgress"
                    , gravity LEFT
                ][  linearLayout
                        [   height $ V 4
                            , width $ V $ (((getLayoutBounds $ EHC.getNewIDWithTag "recordProgress").width) / 15 * (15 - state.props.timerValue))
                            , background Color.blue800
                        ][]
                ]
                , linearLayout [
                    height WRAP_CONTENT
                    , width MATCH_PARENT
                    , padding $ PaddingHorizontal 16 16
                ][ textView
                    [ text $ "0:" <> timerval 
                        , weight 1.0
                        , color Color.white900
                    ]
                    , textView[
                        text "0:15"
                        , color Color.white900
                    ]
                ]
            ]
            , textView $ [
                text $ getString THE_VIDEO_WILL_BE_RECORDED
                , color "#B9BABE"
                , padding $ PaddingHorizontal 16 16
            ] <> FontStyle.body3 TypoGraphy
        ]
        , linearLayout[
            width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation VERTICAL
            , gravity CENTER
            , alignParentBottom "true,-1"
            , onClick (\action -> do
                _ <- push action
                if state.props.recordingState == RECORDING then pure unit
                else do
                    _ <- startRecord push VideoStatusCallBack
                    if (os == "IOS") then liftEffect $ startTimerWithTime (show state.props.timerValue) state.props.timerId "1" push CountDown
                    else liftEffect $ countDown state.props.timerValue state.props.timerId push CountDown
                pure unit
            )(const ToggleRecord)
        ][
            imageView [
                imageWithFallback if state.props.recordingState == RECORDING then "ny_ic_stop_record,https://assets.juspay.in/nammayatri/images/user/ny_ic_help.png" 
                                  else "ny_ic_start_record,https://assets.juspay.in/nammayatri/images/user/ny_ic_help.png"
                , height $ V 45
                , width $ V 45
            ]
            , textView $ [
                text $ if state.props.recordingState == RECORDING then getString STOP_AND_SHARE_RECORDING
                       else getString START_RECORDING
                , color Color.white900
            ] <> FontStyle.tags TypoGraphy
        ]
    ]


toggleSwitchView :: Boolean -> NammaSafetyStage -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
toggleSwitchView isActive stage push = 
    linearLayout[
        height WRAP_CONTENT
        , width WRAP_CONTENT
        , gravity CENTER_VERTICAL
        , onClick push $ const $ ToggleSwitch stage
        , visibility if stage /= SetPersonalSafetySettings then VISIBLE else GONE
    ][
        imageView [
            imageUrl if isActive then "ny_ic_switch_active" else "ny_ic_switch_inactive"
            , width $ V 40
            , height $ V 24
        ]  
    ]

-- shareTimerView :: NammaSafetyScreenState -> (Action -> Effect Unit) -> forall w . PrestoDOM (Effect Unit) w
-- shareTimerView state push = 
--         PrestoAnim.animationSet [fadeIn $ true] $ linearLayout 
--             [   height MATCH_PARENT
--                 , width MATCH_PARENT
--                 , gravity CENTER_VERTICAL
--                 , orientation VERTICAL
--                 , padding $ Padding 16 20 16 5
--                 , visibility if state.props.recordingState == SHARING then VISIBLE else GONE
--                 , onAnimationEnd (\_ -> do
--                     _ <- pure $ spy "animation end" ""

--                     pure unit)(const NoAction)
--                 ][
--                     textView $ [
--                         text $ getString SHARING_THE_VIDEO_IN
--                         , height WRAP_CONTENT
--                         , width MATCH_PARENT
--                         , gravity CENTER
--                         , color Color.white900
--                     ] <> FontStyle.paragraphText TypoGraphy
--                     , textView $ [
--                         text $ show state.props.shareTimerValue
--                         , height WRAP_CONTENT
--                         , width MATCH_PARENT
--                         , color Color.white900
--                         , gravity CENTER
--                     ] <> FontStyle.priceFont TypoGraphy
--                 ]

getHeaderTitle :: NammaSafetyStage -> String
getHeaderTitle stage = 
    case stage of 
        EduNammaSafetyMeasures   -> getString NAMMA_SAFETY_MEASURES
        EduNammaSafetyGuidelines -> getString SAFETY_GUIDELINES_FOR_YOU
        EduNammaSafetyAboutSOS   -> getString ABOUT_SOS
        NammaSafetyVideoRecord   -> getString EMERGENCY_VIDEO
        _                        -> getString NAMMA_SAFETY

emergencyContactsView :: forall w. PrestoList.ListItem -> (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
emergencyContactsView listItemm push state =
  screenAnimation
    $ relativeLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , onBackPressed push (const BackPressed)
        , background Color.white900
        , padding if os == "IOS" then (Padding 0 safeMarginTop 0 (if safeMarginBottom == 0 && os == "IOS" then 16 else safeMarginBottom)) else (Padding 0 0 0 0)
        , gravity CENTER
        , afterRender
            ( \action -> do
                _ <- push action
                _ <- storeCallBackContacts push ContactsCallback
                if ((getValueToLocalStore CONTACTS == "__failed") || (getValueToLocalStore CONTACTS == "(null)")) then do
                  _ <- push FetchContacts
                  pure unit
                else do
                  pure unit
                pure unit
            )
            (const NoAction)
        ]
        [ linearLayout
            [ height MATCH_PARENT
            , width MATCH_PARENT
            , orientation VERTICAL
            ]
            [ 
            linearLayout
                [ height $ V 1
                , width $ V (screenWidth unit)
                , background Color.greySmoke
                ]
                []
            , linearLayout
                [ height MATCH_PARENT
                , width MATCH_PARENT
                , orientation VERTICAL
                , gravity CENTER
                , padding (Padding 16 0 16 0)
                , visibility if state.props.emergencyContactsProps.showContactList then GONE else VISIBLE
                ]
                [ emergencyContactsSelectView push state
                , PrimaryButton.view (push <<< AddEmergencyContacts) (addContactButtonConfig state)
                ]
            ]
        , if state.props.emergencyContactsProps.showContactList then (contactListView listItemm push state) else emptyTextView state
        ]
------------------------ EmptyTextView ---------------------------
emptyTextView :: forall w. NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
emptyTextView state = textView []

------------------------ ContactsListView ---------------------------
contactListView :: forall w. PrestoList.ListItem -> (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
contactListView listItemm push state =
    linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ]
        [ 
        linearLayout
            [ width MATCH_PARENT
            , height $ V 44
            , orientation HORIZONTAL
            , cornerRadius 8.0
            , padding (Padding 2 2 2 2)
            , margin (Margin 16 16 16 16)
            , gravity LEFT
            , stroke ("1," <> Color.borderColorLight)
            ]
            [ editText
                [ height MATCH_PARENT
                , width WRAP_CONTENT
                , weight 1.0
                , textSize FontSize.a_16
                , padding (Padding 14 10 0 10)
                , color Color.black800
                , gravity LEFT
                , id (getNewIDWithTag "contactEditText")
                , background Color.white900
                , fontStyle $ FontStyle.semiBold LanguageStyle
                , text ""
                , hint $ getString SEARCH_CONTACTS
                , pattern "[^\n]*,255"
                , onChange push $ ContactTextChanged
                ]
            , imageView
                [ height $ V 17
                , width $ V 17
                , imageWithFallback "ny_ic_cancel,https://assets.juspay.in/nammayatri/images/user/ny_ic_cancel.png"
                , gravity CENTER
                , margin (Margin 10 10 10 10)
                , onClick push $ const ContactListClearText
                ]
            ]
        , showEmergencyContact listItemm push state
        , linearLayout
            [ height if os == "IOS" then (V 84) else WRAP_CONTENT
            , width MATCH_PARENT
            , orientation VERTICAL
            , background Color.white900
            , padding (Padding 16 16 16 24)
            , stroke $ "1," <> Color.grey900
            , alignParentBottom "true,-1"
            , margin (Margin 0 0 0 0)
            ]
            [ linearLayout
                [ width MATCH_PARENT
                , height if os == "IOS" then (V 52) else WRAP_CONTENT
                , gravity BOTTOM
                , alignParentBottom "true,-1"
                ]
                [ PrimaryButton.view (push <<< ContactListPrimaryButtonActionController) (contactListPrimaryButtonConfig state.data.emergencyContactsData.contactsCount)
                ]
            ]
        ]

showEmergencyContact :: forall w. PrestoList.ListItem ->  (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
showEmergencyContact listitemm push config =
  swipeRefreshLayout
    ([ width MATCH_PARENT
    , height MATCH_PARENT
    , background Color.blue600
    , weight 1.0
    ] <> if os == "IOS" then [] else [id $ getNewIDWithTag "EmergencyContactTag"] )
    [ showEmergencyContactData listitemm push config
    ]

showEmergencyContactData :: forall w. PrestoList.ListItem -> (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
showEmergencyContactData listItemm push state =
  Keyed.linearLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  ]
  [ Tuple "contacts"
    $ PrestoList.list
    [ height MATCH_PARENT
    , width MATCH_PARENT
    , onScroll "contacts" "NammaSafetyScreen" push (ContactListScroll)
    , onScrollStateChange push (ContactListScrollStateChanged)
    , PrestoList.listItem listItemm
    , background Color.white900
    , PrestoList.listDataV2  $ state.data.emergencyContactsData.prestoListArrayItems
    ]
  ]

startsWith :: String -> String -> Boolean
startsWith prefix str = DS.take (DS.length prefix) str == prefix

horizontalLine :: forall w. PrestoDOM (Effect Unit) w
horizontalLine =
  linearLayout
    [ height $ V 1
    , width MATCH_PARENT
    , background Color.grey900
    ][]


--------------------------------------------------- emergencyContactsSelectView -----------------------------------------------------
emergencyContactsSelectView :: forall w. (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
emergencyContactsSelectView push state = 
  linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , weight 1.0
    ]
    [ emptyContactsView push state
      , emergencyContactsListView push state
    ]


--------------------------------------------------- emergencyContactsListView -----------------------------------------------------
emergencyContactsListView :: forall w. (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
emergencyContactsListView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , padding (Padding 0 10 0 10)
    , visibility if (null state.data.emergencyContactsData.contactsList) then GONE else VISIBLE
    , weight 1.0
    ]
    [ textView $ 
        [ height WRAP_CONTENT
        , width if os == "IOS" then V (screenWidth unit - 20) else WRAP_CONTENT
        , text (getString EMERGENCY_CONTACTS_SCREEN_DESCRIPTION)
        , color Color.black700
        , padding (Padding 0 10 0 10)
        ] <> FontStyle.paragraphText LanguageStyle
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        ]
        (mapWithIndex (\index item -> contactCardView push state item index) state.data.emergencyContactsData.contactsList)
    ]

--------------------------------------------------- emptyContactsView -----------------------------------------------------
emptyContactsView :: forall w. (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
emptyContactsView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER
    , visibility if (null state.data.emergencyContactsData.contactsList) then VISIBLE else GONE
    , weight 1.0
    ]
    [ imageView
        [ height $ V 150
        , width $ V 150
        , imageWithFallback $ "ny_ic_emergency_contact_empty," <> (getAssetStoreLink FunctionCall) <> "ny_ic_emergency_contact_empty.png"
        ]
    , textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , gravity CENTER
        , text (getString NO_EMERGENCY_CONTACTS_SET)
        , color Color.black900
        ] <> FontStyle.h2 LanguageStyle
    , textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , gravity CENTER
        , text (getString EMERGENCY_CONTACTS_SCREEN_DESCRIPTION)
        , color Color.black700
        , padding (Padding 16 10 16 10)
        ] <> FontStyle.paragraphText LanguageStyle
    ]

--------------------------------------------------- emergencyContactsListView -----------------------------------------------------
contactCardView :: forall w. (Action -> Effect Unit) -> NammaSafetyScreenState -> NewContacts -> Int -> PrestoDOM (Effect Unit) w
contactCardView push state contact index =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , padding $ Padding 18 18 18 18
    , margin $ Margin 0 5 0 5
    , cornerRadius 8.0
    , stroke ("1," <> Color.grey900)
    ]
    [ linearLayout
        [ height $ V 24
        , width $ V 24
        , background (fromMaybe "" (fromMaybe [] (contactColorsList !! index) !! 0))
        , cornerRadius 12.0
        , gravity CENTER
        , margin (MarginRight 10)
        ]
        [ textView $
            [ text (DS.toUpper ((<>) (getFirstChar contact.name) (getLastChar contact.name)))
            , color (fromMaybe "" (fromMaybe [] (contactColorsList !! index) !! 1))
            ] <> FontStyle.body3 TypoGraphy
        ]
    , textView $
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , weight 1.0
        , text contact.name
        , color Color.black800
        ] <> FontStyle.subHeading1 LanguageStyle
    , textView
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , text (getString REMOVE)
        , color Color.blue900
        , textSize 14
        , onClick push (const (RemoveButtonClicked contact))
        ]
    ]


removeContactPopUpView :: forall w. (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
removeContactPopUpView push state =
  linearLayout
    [ height MATCH_PARENT
    , width MATCH_PARENT
    ]
    [ PopUpModal.view (push <<< PopUpModalAction) (removeContactPopUpModelConfig state) ]

videoSharedView :: forall w. (Action -> Effect Unit) -> NammaSafetyScreenState -> PrestoDOM (Effect Unit) w
videoSharedView push state = 
    linearLayout
    [ width MATCH_PARENT
    , height WRAP_CONTENT
    , orientation VERTICAL
    ][ linearLayout
        [ width MATCH_PARENT
        , height WRAP_CONTENT
        , orientation VERTICAL
        , gravity CENTER
        , padding $ PaddingTop 100
        ][  imageView [
                imageWithFallback "ny_ic_video_shared,https://assets.juspay.in/nammayatri/images/user/ny_ic_help.png"
                , height $ V 300
                , padding $ PaddingHorizontal 16 16
            ]
            , textView $ [
                text $ getString EMERGENCY_INFO_SHARED
                , gravity CENTER
                , color Color.white900
            ] <> FontStyle.h1 TypoGraphy
            , textView $ [
                text $ getString EMERGENCY_INFO_SHARED_ACTION
                , padding $ PaddingHorizontal 20 20
                , gravity CENTER
                , color Color.white900
                , margin $ MarginTop 8
            ] <> FontStyle.body1 TypoGraphy
        ]
        , linearLayout
              [ height WRAP_CONTENT
              , width MATCH_PARENT
              , orientation HORIZONTAL
              , alignParentBottom "true,-1"
              , margin $ MarginTop 50
              ][  PrimaryButton.view (push <<< VideoShared) (goBackBtnConfig state) ]
    ]