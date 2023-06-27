{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Components.DriverInfoCard.View where

import Common.Types.App

import Animation (fadeIn)
import Common.Types.App (LazyCheck(..))
import Components.DriverInfoCard.Controller (Action(..), DriverInfoCardState)
import Components.PrimaryButton as PrimaryButton
import Components.SourceToDestination as SourceToDestination
import Data.Array as Array
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), split, length, take, drop)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (flowRunner, os, safeMarginBottom, screenWidth, getExpiryTime)
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink, getPaymentMethod)
import Helpers.Utils (secondsToHms, zoneOtpExpiryTimer)
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Utils (Merchant(..), getMerchant, getValueFromConfig)
import Prelude (Unit, (<<<), ($), (/), (<>), (==), unit, show, const, map, (>), (-), (*), bind, pure, discard, not, (&&), (||), (/=))
import Presto.Core.Types.Language.Flow (doAff)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Visibility(..), afterRender, alignParentBottom, alignParentLeft, alpha, background, clickable, color, cornerRadius, ellipsize, fontSize, fontStyle, frameLayout, gravity, height, imageUrl, imageView, imageWithFallback, letterSpacing, lineHeight, linearLayout, margin, maxLines, onClick, orientation, padding, scrollBarY, scrollView, singleLine, stroke, text, textSize, textView, visibility, weight, width)
import PrestoDOM.Animation as PrestoAnim
import PrestoDOM.Properties (cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types (Stage(..), ZoneType(..))
import Storage (isLocalStageOn)
import Styles.Colors as Color

view :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit ) w
view push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , background Color.transparent
  , orientation VERTICAL
  ][  mapOptionsView push state
    , if state.data.isSpecialZone then driverInfoViewSpecialZone push state else driverInfoView push state
    ]

driverInfoViewSpecialZone :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
driverInfoViewSpecialZone push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  ][ (if os == "IOS" then linearLayout else scrollView)
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , scrollBarY false
      ][ linearLayout
          [ orientation VERTICAL
          , height WRAP_CONTENT
          , width MATCH_PARENT
          , padding $ PaddingBottom 30
          , margin $ MarginTop 14
          , background Color.white900
          , gravity CENTER
          , cornerRadii $ Corners 24.0 true true false false
          , stroke $ "1," <> Color.grey900
          ][ linearLayout
              [ gravity CENTER
              , background Color.transparentGrey
              , height $ V 4
              , width $ V 34
              , margin (MarginVertical 8 8)
              , cornerRadius 4.0
              ][]
            , titleAndETA push state
            , otpAndWaitView push state
            , separator (MarginHorizontal 16 16) (V 1) Color.grey900 (state.props.currentStage == RideStarted && (secondsToHms state.data.eta) /= "" && (state.props.isSpecialZone && (state.props.estimatedTime /= "--")))
            , driverDetailsView push state
            , separator (MarginHorizontal 16 16) (V 1) Color.grey900 true
            , paymentMethodView push state (getString PAY_VIA_CASH_OR_UPI <> " :-") false
            , separator (MarginHorizontal 16 16) (V 1) Color.grey900 true
            , linearLayout
              [ width MATCH_PARENT
              , height WRAP_CONTENT
              , orientation VERTICAL
              ][ dropPointView push state
                , separator (MarginHorizontal 16 16) (V 1) Color.grey900 (state.props.currentStage == RideAccepted)
                , cancelRideLayout push state
              ]
            ]
      ]
  ]

titleAndETA :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
titleAndETA push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER_VERTICAL
  , padding $ Padding 16 20 16 16
  , visibility $ if ((state.props.currentStage /= RideAccepted && (secondsToHms state.data.eta) == "") || (state.props.currentStage == RideStarted && (spy "estimatedTimeABCD" state.props.estimatedTime == "--"))) then GONE else VISIBLE
  ][ textView $ 
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , text $ if state.props.currentStage == RideAccepted then getString BOARD_THE_FIRST_TAXI else "ETA: " <> if state.props.isSpecialZone then (state.props.estimatedTime) else (secondsToHms state.data.eta)
      , color Color.black800
      -- , fontSize FontSize.a_22
      ] <> FontStyle.h2 TypoGraphy
  ]

dropPointView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
dropPointView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , padding $ Padding 0 10 16 if (os == "IOS") then if safeMarginBottom == 0 then 16 else safeMarginBottom else 16
  ][  textView (
      [ text $ getString DROP <> " :-"
      , margin $ Margin 16 0 0 5
      ] <> FontStyle.body3 TypoGraphy)
    , textView $
      [ text state.data.destination
      , color Color.black800
      , margin $ Margin 16 0 0 5
      ] <> FontStyle.subHeading1 TypoGraphy
    , estimatedTimeAndDistanceView push state
  ]

estimatedTimeAndDistanceView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
estimatedTimeAndDistanceView push state =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , gravity CENTER
  , margin $ Margin 16 4 16 0
  ][ textView (
      [ text $ state.data.estimatedDistance <> "km " <> getString AWAY
      , width MATCH_PARENT
      , gravity CENTER
      , color Color.black650
      , height WRAP_CONTENT
      ] <> FontStyle.paragraphText TypoGraphy)
    -- , linearLayout
    --   [height $ V 4
    --   , width $ V 4
    --   , cornerRadius 2.5
    --   , background Color.black600
    --   , margin (Margin 6 2 6 0)
    --   ][]
    -- , textView
    --   [ text state.data.estimatedDropTime
    --   , textSize FontSize.a_14
    --   , width MATCH_PARENT
    --   , gravity CENTER
    --   , color Color.black650
    --   , height WRAP_CONTENT
    --   , fontStyle $ FontStyle.regular LanguageStyle
    --   ]
  ]

otpView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
otpView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  ] (map(\item ->
      linearLayout
        [ height $ V 32
        , width $ V 32
        , gravity CENTER
        , cornerRadius 4.0
        , background state.data.config.quoteListModel.otpTextBackground
        , margin $ MarginLeft 7
        ][ textView (
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , text item
            , color state.data.config.quoteListModel.otpTextColor
            ] <> FontStyle.h2 TypoGraphy)
        ]) $ split (Pattern "")  state.data.otp)

expiryTimeView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
expiryTimeView push state =
 linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER_HORIZONTAL
  , margin $ Margin 16 0 16 16
  , cornerRadius 9.0
  , background Color.grey800
  ][ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER
      ][ linearLayout
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , padding $ Padding 10 14 10 14
          , weight 1.0
          ][ textView (
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text $ getString OTP <> ":"
              , color Color.black700
              ] <> FontStyle.body4 TypoGraphy)
            , otpView push state
          ]
        , waitTimeView push state
      ]
  ]

mapOptionsView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
mapOptionsView push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  , background Color.transparent
  , orientation HORIZONTAL
  , gravity CENTER_VERTICAL
  , padding $ PaddingHorizontal 16 16
  ][  if state.data.isSpecialZone && state.props.currentStage == RideAccepted then navigateView push state  else sosView push state
    , linearLayout
      [ height WRAP_CONTENT
      , weight 1.0
      , clickable false
      ][]
    , linearLayout
      [ height WRAP_CONTENT
      , width WRAP_CONTENT
      , orientation VERTICAL
      ][ supportButton push state
       , locationTrackButton push state
      ]
    ]


supportButton :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
supportButton push state =
 linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , gravity CENTER
  , background Color.white900
  , stroke $ "1,"<> Color.grey900
  , margin $ MarginTop 10
  , cornerRadius 20.0
  ][ imageView
      [ imageWithFallback "ny_ic_share_icon,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_share_icon.png"
      , height $ V 18
      , width $ V 18
      , margin $ Margin 10 10 10 4
      , visibility (if (getValueFromConfig "enableShareRide") == "true" then VISIBLE else GONE)
      , onClick push $ const ShareRide
      ]
    , linearLayout
      [ height (V 1)
      , width (V 19)
      , visibility (if (getValueFromConfig "enableShareRide") == "true" then VISIBLE else GONE)
      , margin (MarginTop 8 )
      , background Color.lightGreyShade
      ][]
    , imageView
      [ imageWithFallback $ "ny_ic_contact_support," <> (getAssetStoreLink FunctionCall) <> "ny_ic_contact_support.png"
      , height $ V 18
      , width $ V 18
      , margin $ Margin 10 12 10 10
      , onClick push $ const Support
      ]
  ]


locationTrackButton :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
locationTrackButton push state =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , orientation VERTICAL
  , gravity CENTER
  , background Color.white900
  , stroke $ "1,"<> Color.grey900
  , visibility if state.props.currentStage == RideAccepted then GONE else VISIBLE
  , cornerRadius 20.0
  , onClick push (const $ LocationTracking)
  , margin $ MarginTop 8
  ][  imageView
      [ imageWithFallback $ "ny_ic_location_track," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_location_track.png"
      , height $ V 18
      , width $ V 18
      , margin $ Margin 10 10 10 10
      ]
  ]

sosView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
sosView push state =
  linearLayout
    [ height MATCH_PARENT
    , width WRAP_CONTENT
    , visibility if (state.props.currentStage == RideAccepted) || (state.props.currentStage == RideStarted) then VISIBLE else GONE
    , orientation VERTICAL
    , gravity if os == "IOS" then CENTER_VERTICAL else BOTTOM
    , onClick push $ const OpenEmergencyHelp
    ][ imageView
        [ imageWithFallback $ "ny_ic_sos," <> (getAssetStoreLink FunctionCall) <> "ny_ic_sos.png"
        , height $ V 50
        , width $ V 50
        ]
    ]

navigateView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
navigateView push state =
  linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , background Color.blue900
      , padding $ Padding 10 16 10 16
      , margin $ MarginRight 16
      , cornerRadius 25.0
      , gravity CENTER
      , orientation HORIZONTAL
      , onClick push (const OnNavigate)
      ][  imageView
          [ width $ V 20
          , height $ V 20
          , imageWithFallback "ny_ic_navigation,https://assets.juspay.in/nammayatri/images/driver/ny_ic_navigation.png"
          ]
        , textView (
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , margin (MarginLeft 8)
          , text $ getString NAVIGATE_TO_PICKUP
          , gravity CENTER
          , color Color.white900
          ] <> FontStyle.body1 TypoGraphy
          )
      ]

otpAndWaitView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
otpAndWaitView push state =
 linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER_HORIZONTAL
  , margin $ Margin 16 0 16 16
  , visibility if state.props.currentStage == RideAccepted then VISIBLE else GONE
  ][ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , gravity CENTER
      ][ linearLayout
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , cornerRadius 9.0
          , background state.data.config.quoteListModel.otpBackground
          , gravity CENTER
          , padding $ Padding 10 14 10 14
          , weight 1.0
          , stroke state.data.config.driverInfoConfig.otpStroke
          ][ textView (
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , text $ getString OTP <> ":"
              , color state.data.config.quoteListModel.otpTitleColor
              ] <> FontStyle.body4 TypoGraphy)
            , otpView push state
          ]
        , waitTimeView push state
      ]
  ]


waitTimeView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
waitTimeView push state =
 PrestoAnim.animationSet [ fadeIn state.data.driverArrived ] $
 linearLayout
  [ width WRAP_CONTENT
  , height if os == "IOS" then (V 60) else MATCH_PARENT
  , orientation VERTICAL
  , cornerRadius 9.0
  , background Color.grey800
  , gravity CENTER_VERTICAL
  , margin $ MarginLeft 12
  , padding $ Padding 14 2 14 2
  , visibility case state.data.isSpecialZone of
      true -> VISIBLE
      false -> if state.data.driverArrived then VISIBLE else GONE
  ][ textView (
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , text $ if state.data.isSpecialZone then getString EXPIRES_IN else  getString WAIT_TIME <> ":"
      , color Color.black700
      ] <> FontStyle.body1 TypoGraphy)
    , textView (
      [ width MATCH_PARENT
      , height WRAP_CONTENT
      , text state.data.waitingTime
      , lineHeight "24"
      , gravity CENTER
      , color Color.black800
      , afterRender
            ( \action -> do
                if state.data.isSpecialZone && (isLocalStageOn RideAccepted) then do
                  _ <- zoneOtpExpiryTimer (getExpiryTime state.data.bookingCreatedAt true) 1800 push ZoneOTPExpiryAction
                  pure unit
                  else pure unit
            )
            (const NoAction)
      ] <> FontStyle.h2 TypoGraphy)
  ]

driverInfoView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM ( Effect Unit) w
driverInfoView push state =
  linearLayout
  [ width MATCH_PARENT
  , height WRAP_CONTENT
  ][ (if os == "IOS" then linearLayout else scrollView)
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , scrollBarY false
      ][ linearLayout
         [ orientation VERTICAL
         , height WRAP_CONTENT
         , width MATCH_PARENT
         , margin $ MarginTop 14
         , background Color.blue800
         , gravity CENTER
         , cornerRadii $ Corners 24.0 true true false false
         , stroke $ state.data.config.driverInfoConfig.cardStroke
         ][ linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , background Color.blue800
            , cornerRadii $ Corners 24.0 true true false false
            , gravity CENTER
            , orientation HORIZONTAL
            , padding (PaddingVertical 4 4)
            , visibility if state.props.zoneType == METRO then VISIBLE else GONE
            ][ imageView
                [ width (V 15)
                , height (V 15)
                , margin (MarginRight 6)
                , imageWithFallback $  "ny_ic_metro_white," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_metro_white.png"
                ]
              , textView
                [ width WRAP_CONTENT
                , height WRAP_CONTENT
                , textSize FontSize.a_14
                , text (getString METRO_RIDE)
                , color Color.white900
                ]
             ]
          , linearLayout
            [ orientation VERTICAL
            , height WRAP_CONTENT
            , width MATCH_PARENT
            , padding $ PaddingBottom 24
            , background Color.white900
            , gravity CENTER
            , cornerRadii $ Corners 24.0 true true false false
            , stroke $ "1," <> Color.grey900
            ][ linearLayout
              [ gravity CENTER
              , background Color.transparentGrey
              , height $ V 4
              , width $ V 34
              , margin (MarginTop 8)
              , cornerRadius 4.0
              ][]
              , if state.props.isSpecialZone  then headerTextView push state else contactView push state
              , otpAndWaitView push state
              , separator (Margin 16 (if(state.props.currentStage == RideStarted && state.data.config.nyBrandingVisibility) then 16 else 0) 16 0) (V 1) Color.grey900 $ ((state.props.currentStage == RideAccepted || state.props.currentStage == RideStarted) && state.data.config.nyBrandingVisibility) || (state.props.currentStage == RideAccepted && not state.data.config.showPickUpandDrop)
              , driverDetailsView push state
              , separator (MarginHorizontal 16 16) (V 1) Color.grey900 true
              , paymentMethodView push state (getString RIDE_FARE) true
              ,  separator (Margin 16 0 16 0) (V 1) Color.grey900 (state.data.config.showPickUpandDrop == true)
              , (if os == "IOS" then scrollView else linearLayout)
                [ width MATCH_PARENT
                , height if os == "IOS" then (V 210) else WRAP_CONTENT
                , orientation VERTICAL
                ][ if state.props.isSpecialZone then destinationView push state else if state.data.config.showPickUpandDrop == false then dummyView push else sourceDistanceView push state
                  , separator (Margin 0 0 0 0) (V 1) Color.grey900 ((state.props.currentStage == RideAccepted || state.props.currentStage == RideStarted) && (state.data.config.showPickUpandDrop == true))
                  , cancelRideLayout push state
                ]
              ]
         ]
      ]
  ]

cancelRideLayout :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
cancelRideLayout push state =
 linearLayout
 [ width MATCH_PARENT
 , height WRAP_CONTENT
 , gravity CENTER
 , margin $ if state.data.config.showPickUpandDrop == false then MarginTop 0 else MarginTop 16
 , padding $ PaddingBottom if os == "IOS" then (if safeMarginBottom == 0 then 24 else safeMarginBottom) else 0
 , visibility if state.props.currentStage == RideAccepted then VISIBLE else GONE
 ][ linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  , padding $ Padding 5 5 5 5
  , margin $ MarginBottom if os == "IOS" then 24 else 0
  , onClick push $ const $ CancelRide state
  ][ textView (
     [ width WRAP_CONTENT
     , height WRAP_CONTENT
     , text $ getString CANCEL_RIDE
     , color state.data.config.cancelRideColor
     , alpha $ if (getMerchant FunctionCall) == PAYTM then 0.54 else 1.0
     ] <> FontStyle.subHeading1 TypoGraphy)
   ]
 ]

---------------------------------- contactView ---------------------------------------
contactView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
contactView push state = 
  let isChatEnabled = (getValueFromConfig "isChatEnabled")
  in 
    linearLayout
    [ orientation HORIZONTAL
    , height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity CENTER_VERTICAL
    , padding $ Padding 16 20 16 16
    , visibility if state.props.currentStage == RideAccepted then VISIBLE else GONE
    ][  linearLayout
        [ width (V (((screenWidth unit)/3 * 2)-27))
        , height WRAP_CONTENT
        , orientation if length state.data.driverName > 16 then VERTICAL else HORIZONTAL
        ][  textView (
            [ text $ state.data.driverName <> " "
            , color Color.black800
            , ellipsize true
            , singleLine true
            ] <> FontStyle.subHeading1 TypoGraphy)
          , textView (
            [ text $"is " <> secondsToHms state.data.eta
            , color Color.black800
            , visibility if state.data.distance > 1000 then VISIBLE else GONE
            ] <> FontStyle.subHeading1 TypoGraphy)
          , textView (
            [ text case state.data.distance > 1000 of
              true -> getString AWAY
              false -> if state.data.waitingTime == "--" then getString IS_ON_THE_WAY else getString IS_WAITING_FOR_YOU
            , color Color.black800
            ] <> FontStyle.subHeading1 TypoGraphy)
        ]
      , linearLayout[
        width MATCH_PARENT
      , gravity RIGHT
      , height WRAP_CONTENT
      ][linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity RIGHT
        ] [ linearLayout
            [ height $ V 40
            , width $ V 64
            , gravity CENTER
            , cornerRadius 20.0
            , background state.data.config.driverInfoConfig.callBackground
            , stroke state.data.config.driverInfoConfig.callButtonStroke
            , onClick push if isChatEnabled == "true" then const MessageDriver else const CallDriver
            ][ imageView
                [ imageWithFallback $ if (getValueFromConfig "isChatEnabled") == "true" then if state.props.unReadMessages then "ic_chat_badge_green," <> (getAssetStoreLink FunctionCall) <> "ic_chat_badge_green.png" else "ic_call_msg," <> (getAssetStoreLink FunctionCall) <> "ic_call_msg.png"
                                        else "ny_ic_call," <> (getAssetStoreLink FunctionCall) <> "ny_ic_call.png"
                , height $ V state.data.config.driverInfoConfig.callHeight
                , width $ V state.data.config.driverInfoConfig.callWidth
                ]
            ]
          ]
        ]
    ]
    

---------------------------------- driverDetailsView ---------------------------------------


driverDetailsView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
driverDetailsView push state =
 linearLayout
  [ orientation HORIZONTAL
  , height $ V 170
  , padding $ Padding 16 16 16 16
  , width MATCH_PARENT
  , visibility if state.props.isSpecialZone then (if state.props.currentStage == RideStarted then VISIBLE else GONE) else VISIBLE
  , gravity BOTTOM
  ][  linearLayout
      [ orientation VERTICAL
      , height MATCH_PARENT
      , width WRAP_CONTENT
      , gravity BOTTOM
      , alignParentLeft "true,-1"
      ][  linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT
          , gravity LEFT
          ][imageView
              [ height $ V 50
              , width $ V 50
              , padding $ Padding 2 3 2 1
              , imageWithFallback $ "ny_ic_profile_image," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_profile_image.png"
              ]  
          ]
        , textView $
          [ text state.data.driverName
          , maxLines 1
          , ellipsize true
          , color Color.black800
          , width MATCH_PARENT
          , height WRAP_CONTENT
          , gravity LEFT
          ] <> FontStyle.body7 TypoGraphy
        , textView (
          [ text state.data.vehicleDetails
          , color Color.black700
          , width MATCH_PARENT
          , height WRAP_CONTENT
          , margin $ Margin 0 4 0 13
          , gravity LEFT
          ] <> FontStyle.body3 TypoGraphy)
        , ratingView push state
      ]
    , linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      , gravity RIGHT
      ][  frameLayout
          [ height MATCH_PARENT
          , width $ V 172
          , gravity BOTTOM
          ][  imageView
              [ imageWithFallback $ "ic_driver_vehicle," <> (getAssetStoreLink FunctionCall) <> "ic_driver_vehicle.png"
              , height $ V 120
              , gravity RIGHT
              , width MATCH_PARENT
              , margin $ MarginBottom 15
              ]
            , linearLayout
              [ height $ V 138
              , width MATCH_PARENT
              , gravity BOTTOM
              ][  linearLayout
                  [ height $ V 38
                  , width MATCH_PARENT
                  , background Color.golden
                  , cornerRadius 4.0
                  , orientation HORIZONTAL
                  , gravity BOTTOM
                  , padding $ Padding 2 2 2 2
                  , alignParentBottom "true,-1"
                  ][
                    linearLayout
                    [ height $ V 34
                    , width MATCH_PARENT
                    , stroke $ "2," <> Color.black
                    , cornerRadius 4.0
                    , orientation HORIZONTAL
                    ][  imageView
                        [ imageWithFallback $ "ny_ic_number_plate," <> (getAssetStoreLink FunctionCall) <> "ny_ic_number_plate.png"
                        , gravity LEFT
                        , visibility if state.data.config.driverInfoConfig.showIndNumberPlate then VISIBLE else GONE
                        , background "#1C4188"
                        , height MATCH_PARENT
                        , width $ V 22
                        ]
                        , textView $
                        [ margin $ Margin 2 2 2 2
                        , width MATCH_PARENT
                        , height MATCH_PARENT
                        , text $ (makeNumber state.data.registrationNumber)
                        , color Color.black
                        , gravity CENTER
                        , cornerRadius 4.0
                        ] <> FontStyle.body7 TypoGraphy
                      ]
                    ]
                ]

            ]
        ]
    ]

---------------------------------- ratingView ---------------------------------------

makeNumber :: String -> String
makeNumber number = (take 2 number) <> " " <> (drop 2 (take 4 number)) <> " " <>  reverse (drop 4 (reverse (drop 4 number))) <> " " <>  reverse (take 4 (reverse number))

reverse :: String -> String
reverse = fromCharArray <<< Array.reverse <<< toCharArray

ratingView :: forall w. (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
ratingView push state =
  linearLayout
  [ orientation HORIZONTAL
  , height $ V 34
  , width WRAP_CONTENT
  , padding $ Padding 16 9 16 9
  , background state.data.config.driverInfoConfig.ratingBackground
  , gravity CENTER_VERTICAL
  , stroke  state.data.config.driverInfoConfig.ratingStroke
  , cornerRadius state.data.config.driverInfoConfig.ratingCornerRadius
  ][  imageView
      [ imageWithFallback $ "ny_ic_star_active," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_star_active.png"
      , height $ V 13
      , width $ V 13
      ]
    , textView (
      [ text $ if state.data.rating == 0.0 then "New" else show state.data.rating
      , color state.data.config.driverInfoConfig.ratingTextColor
      , gravity CENTER
      , margin (Margin 8 0 2 0)
      , width WRAP_CONTENT
      , height $ V 30
      ] <> FontStyle.tags TypoGraphy)
    ]

---------------------------------- paymentMethodView ---------------------------------------

paymentMethodView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> String -> Boolean -> PrestoDOM (Effect Unit) w
paymentMethodView push state title shouldShowIcon =
  linearLayout
  [ orientation HORIZONTAL
  , width MATCH_PARENT
  , height WRAP_CONTENT
  , gravity CENTER_VERTICAL
  ][  linearLayout
      [ orientation VERTICAL
      , height WRAP_CONTENT
      , padding $ Padding 16 16 16 16
      , width WRAP_CONTENT
      , gravity LEFT
      ][  textView $
          [ text title
          , color Color.black700
          ] <> FontStyle.body3 TypoGraphy
        , textView $
          [ text $ state.data.config.currency <> show state.data.price
          , color Color.black800
          ] <> FontStyle.h2 TypoGraphy
      ]
      , linearLayout
        [ height WRAP_CONTENT
        , weight 1.0
        ][]
      , linearLayout
          [ orientation HORIZONTAL
          , width WRAP_CONTENT
          , height WRAP_CONTENT
          , gravity CENTER
          , visibility if shouldShowIcon then VISIBLE else GONE
          ][  imageView
              [ imageWithFallback $ "ny_ic_wallet," <> (getAssetStoreLink FunctionCall) <> "ny_ic_wallet.png"
              , height $ V 20
              , width $ V 20
              ]
            , textView $
              [ text $ if (getPaymentMethod unit) == "cash" then getString PAYMENT_METHOD_STRING else getString PAYMENT_METHOD_STRING_
              , color Color.black800
              , padding $ Padding 8 0 20 0
              ] <> FontStyle.body1 TypoGraphy
            ]
    ]

---------------------------------- tripDetailsView ---------------------------------------

sourceDistanceView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
sourceDistanceView push state =
  linearLayout
  [ height WRAP_CONTENT
  , width MATCH_PARENT
  , orientation VERTICAL
  , padding $ Padding 0 10 0 if (os == "IOS" && state.props.currentStage == RideStarted) then safeMarginBottom else 16
  ][  textView (
      [ text $ getString PICKUP_AND_DROP
      , margin $ Margin 16 0 0 10
      ] <> FontStyle.body3 TypoGraphy)
    , SourceToDestination.view (push <<< SourceToDestinationAC) (sourceToDestinationConfig state)
  ]

---------------------------------- separator ---------------------------------------

separator :: forall w. Margin -> Length -> String -> Boolean -> PrestoDOM (Effect Unit) w
separator margin' height' color' isVisible =
  linearLayout
  [ height $ height'
  , margin $ margin'
  , width MATCH_PARENT
  , visibility if isVisible then VISIBLE else GONE
  , background color'
  ][]

---------------------------------- primaryButtonConfig ---------------------------------------

primaryButtonConfig :: PrimaryButton.Config
primaryButtonConfig = let
    config' = PrimaryButton.config
    primaryButtonConfig' = config'
      { width = WRAP_CONTENT
      , height = WRAP_CONTENT
      , background = Color.mint
      , cornerRadius = 17.0
      , isPrefixImage = true
      , prefixImageConfig {
          height = V 18
        , width = V 18
        , imageUrl = "ny_ic_call," <> (getCommonAssetStoreLink FunctionCall) <> "ny_ic_call.png"
        , margin = Margin 20 10 20 10
        }
      }
  in primaryButtonConfig'


---------------------------------- sourceToDestinationConfig ---------------------------------------

sourceToDestinationConfig :: DriverInfoCardState -> SourceToDestination.Config
sourceToDestinationConfig state = let
  config = SourceToDestination.config
  sourceToDestinationConfig' = config
    {
      margin = Margin 2 0 40 0
    , lineMargin = Margin 19 7 0 0
    , destinationMargin = MarginTop 16
    , sourceImageConfig {
        imageUrl = "ny_ic_pickup," <> (getAssetStoreLink FunctionCall) <> "ny_ic_pickup.png"
      , height = V 14
      , width = V 14
      , margin = Margin 13 4 0 0
      }
    , sourceTextConfig {
        text = state.data.source
      , textStyle = FontStyle.Body1
      , padding = Padding 2 0 2 2
      , margin = Margin 12 0 15 0
      , ellipsize = true
      , maxLines = 1
      }
    , destinationImageConfig {
        imageUrl = "ny_ic_drop," <> (getAssetStoreLink FunctionCall) <> "ny_ic_drop.png"
      , height = V 17
      , width = V 14
      , margin = Margin 13 2 0 0
      }
    , destinationTextConfig {
        text = state.data.destination
      , padding = Padding 2 0 2 2
      , margin = MarginVertical 12 15
      , maxLines = 1
      , textStyle = FontStyle.Body1
      , ellipsize = true
      }
    , distanceConfig {
        distanceVisibility = VISIBLE
      , distanceValue = state.data.estimatedDistance <> " km"
      , background = Color.grey700
      , margin = Margin 12 28 0 0
  }
    }
  in sourceToDestinationConfig'

headerTextView :: forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
headerTextView push state =
  linearLayout
  [ orientation VERTICAL
  , height WRAP_CONTENT
  , width MATCH_PARENT
  , gravity CENTER_VERTICAL
  , padding $ Padding 16 20 16 16
  ][ textView $
      [ text if state.props.currentStage == RideStarted then "ETA :" <> state.props.estimatedTime else  getString BOARD_THE_FIRST_TAXI
      , color Color.black800
      , padding $ PaddingBottom 16
      , ellipsize true
      ] <> FontStyle.body8 TypoGraphy
    ,  separator (MarginHorizontal 16 16) (V 1) Color.grey900 (state.props.currentStage == RideStarted)
    , if state.props.currentStage == RideStarted then  contactView push state else linearLayout[][]
  ]

destinationView ::  forall w.(Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
destinationView push state=
  linearLayout
      [ orientation VERTICAL
      , height WRAP_CONTENT
      , padding $ Padding 16 16 16 16
      , margin $ MarginBottom (if os == "IOS" then if safeMarginBottom == 0 then 24 else safeMarginBottom else 0)
      , width WRAP_CONTENT
      , gravity LEFT
      ][  textView $
          [ text if true then "Drop :-" else  getString RIDE_FARE
          , color Color.black700
          ] <> FontStyle.body3 TypoGraphy
        , textView $
          [ text state.data.destination
          , color Color.black800
          ] <> FontStyle.h2 TypoGraphy
        , linearLayout
            [ width WRAP_CONTENT
            , height WRAP_CONTENT
            , gravity CENTER
            , margin $ MarginTop 4
            ][ textView (
                [ text $ state.data.estimatedDistance <> " km"
                , width MATCH_PARENT
                , gravity CENTER
                , color Color.black650
                , height WRAP_CONTENT
                ] <> FontStyle.paragraphText TypoGraphy)
              , linearLayout
                [height $ V 4
                , width $ V 4
                , cornerRadius 2.5
                , background Color.black600
                , margin (Margin 6 2 6 0)
                ][]
              , textView (
                [ text state.props.estimatedTime
                , width MATCH_PARENT
                , gravity CENTER
                , color Color.black650
                , height WRAP_CONTENT
                ] <> FontStyle.paragraphText TypoGraphy)
            ]
      ]
openGoogleMap :: forall w . (Action -> Effect Unit) -> DriverInfoCardState -> PrestoDOM (Effect Unit) w
openGoogleMap push state =
  linearLayout
  [ width WRAP_CONTENT
  , height WRAP_CONTENT
  , gravity LEFT
  ][  linearLayout
      [ width WRAP_CONTENT
      , height WRAP_CONTENT
      , background Color.blue900
      , padding $ Padding 20 15 20 15
      , margin $ MarginRight 16
      , cornerRadius 30.0
      , gravity CENTER
      , orientation HORIZONTAL
      , onClick push (const OnNavigate)
      ][ textView (
          [ width WRAP_CONTENT
          , height WRAP_CONTENT
          , text (getString NAVIGATE)
          , gravity CENTER
          , color Color.white900
          ] <> FontStyle.body1 TypoGraphy
          )
        , imageView
          [ width $ V 20
          , height $ V 20
          , margin (MarginLeft 6)
          , imageWithFallback $ "ny_ic_navigation," <> (getCommonAssetStoreLink FunctionCall) <> "driver/images/ny_ic_navigation.png"
          ]
      ]
  ]

dummyView :: forall w . (Action -> Effect Unit) -> PrestoDOM (Effect Unit) w
dummyView push  = 
  linearLayout
  [ height WRAP_CONTENT
  , width WRAP_CONTENT
  ][]

configurations ∷ { letterSpacing ∷ Number , paddingOTP ∷ Padding , paddingOTPText ∷ Padding }
configurations =
  case os of
    "IOS" -> {paddingOTPText : PaddingVertical 4 4
              , letterSpacing : 6.0
              , paddingOTP : Padding 20 5 18 7}
    _     -> {paddingOTPText : PaddingVertical 2 2
              , letterSpacing : 3.0
              , paddingOTP : Padding 11 0 11 7
              }
