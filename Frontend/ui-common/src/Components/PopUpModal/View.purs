{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
  
-}


------------------------ Steps To Implement Animation for PopUpModal -------------------------------------
{- 
    
    Animation for Pop Up Modal is being handled using the popUpStatus which is of three types : 
        - OPEN
        - CLOSING 
        - CLOSED 
    
    - OPEN : To start the Slide in Animation 
    - CLOSING : To Start the closing Animation 
    - CLOSED : Default State

    OnClick of any of the buttons , OnClose Action is triggered , This action needs to be handled in the
    controller of the screen where this component is being used. 

    As the animations are being handled using popUpStatus so in each screen's state variable wherever popUpModal
    is being used we need to declare a variable popUpConfig of the following type. 
        popUpConfig :: PopUpConfig 

        type PopUpConfig  = {
            status :: PopUpStatus 
            , actionType :: Maybe PopUpAction   ----> This is to maintain which action triggered the closing animation
            }

    this variable would be sent in config so that the PopUpStatus can change in the popUpView 

    Inside OnClose Action do the following changes : 

    eval (DistanceOutsideLimitsActionController (PopUpModal.OnClose actionType)) state = continue state{data{popUpConfig = closePopUpConfig (Just actionType)}}

    There are two functions : closePopUpConfig and defaultPopUpConfig. 

    closePopUpConfig will make the changes to trigger the closing animation
    defaultPopUpConfig will restore the popUpStatus to default.

    In order to trigger the opening animation i.e slide in animation , explicitly change the value of
    the state's popUpConfig's status to OPEN.

-}


module Components.PopUpModal.View where

import Prelude (Unit, const, unit, ($), (<>), (/), (-), (+), (==), (||), (&&), (>), not, (<<<), bind, discard, show, pure)
import Effect (Effect)
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Orientation(..), PrestoDOM, Visibility(..), afterRender, imageView, imageUrl, background, clickable, color, cornerRadius, fontStyle, gravity, height, linearLayout, margin, onClick, orientation, text, textSize, textView, width, stroke, alignParentBottom, relativeLayout, padding, visibility, onBackPressed, alpha, imageWithFallback, weight, onAnimationEnd)
import Components.PopUpModal.Controller (Action(..), Config)
import PrestoDOM.Properties (lineHeight, cornerRadii)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Font.Style as FontStyle
import Common.Styles.Colors as Color
import Font.Size as FontSize
import Engineering.Helpers.Commons (screenHeight, screenWidth)
import PrestoDOM.Properties (cornerRadii)
import Components.PrimaryEditText.View as PrimaryEditText
import Components.PrimaryEditText.Controller as PrimaryEditTextConfig
import Effect.Class (liftEffect)
import Engineering.Helpers.Commons (os, clearTimer, countDown)
import Data.Array ((!!), mapWithIndex)
import Data.Maybe (fromMaybe, isNothing, Maybe(..))
import Control.Monad.Trans.Class (lift)
import JBridge (startTimerWithTime)
import Data.Maybe (Maybe(..))
import PrestoDOM.Animation as PrestoAnim
import Animation (fadeIn, fadeOut, translateYAnimFromTop)
import Common.Animation.Config as AnimConfig
import Common.Types.App(PopUpStatus(..), PopUpAction(..), LazyCheck(..)) as Common

view :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
view push state =
  PrestoAnim.animationSet 
    [ AnimConfig.fadeIn (state.popUpStatus == Common.OPEN) 
    , AnimConfig.fadeOut (state.popUpStatus == Common.CLOSING)  
    ] $ 
    linearLayout
    [ width MATCH_PARENT
    , height MATCH_PARENT
    , orientation VERTICAL
    , background state.backgroundColor
    , afterRender
        ( \action -> do
            if (state.option2.enableTimer || state.option1.enableTimer) then do
              let
                timerValue' = if state.option2.enableTimer then state.option2.timerValue else state.option1.timerValue
              if os == "IOS" then
                liftEffect $ startTimerWithTime (show timerValue') "" "1" push CountDown
              else
                countDown timerValue' "" push CountDown
              pure unit
            else
              pure unit
        )
        (const NoAction)
    , onClick
        ( \action -> do
            _ <- push action
            clearTheTimer state
            pure unit
        ) (const $ if state.backgroundClickable && state.dismissPopup then OnClose Common.DismissPopUp else if state.backgroundClickable then OnClose Common.Button1Click else NoAction )
    , gravity state.gravity
    ][ (PrestoAnim.animationSet if (state.popUpStatus == Common.OPEN) then [ translateYAnimFromTop $ AnimConfig.translateYAnimConfig AnimConfig.BOTTOM_TOP] else [ AnimConfig.fadeOut ( state.popUpStatus == Common.CLOSING)] ) $ linearLayout
        [height WRAP_CONTENT
        , onAnimationEnd push $ if (state.popUpStatus == Common.CLOSING) then 
                                                if state.actionType == Just Common.Button2Click then const OnButton2Click
                                                else if state.actionType == Just Common.Button1Click then const OnButton1Click
                                                else if state.actionType == Just Common.OnImageClick then const OnImageClick
                                                else if state.backgroundClickable && state.dismissPopup || (isNothing state.actionType)  then const DismissPopup
                                                else const NoAction
                                            else const NoAction
        , width MATCH_PARENT
        ][  linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , margin state.dismissIconMargin
            , gravity RIGHT
            , visibility state.dismissIconVisibility
            ][ imageView
                [ height $ V 21
                , width $ V 21
                , imageWithFallback "ny_ic_dismiss,https://assets.juspay.in/nammayatri/images/user/ny_ic_dismiss.png" 
                ]
            ]
        ,   linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , cornerRadii state.cornerRadius
            , orientation VERTICAL
            , background Color.white900
            , margin state.margin
            , padding state.padding
            , clickable true
            ][  linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity CENTER
                , visibility state.coverImageConfig.visibility
                , cornerRadii state.cornerRadius
                ][  imageView
                    [ height state.coverImageConfig.height
                    , width state.coverImageConfig.width
                    , margin state.coverImageConfig.margin
                    , padding state.coverImageConfig.padding
                    , imageWithFallback state.coverImageConfig.imageUrl
                    , visibility state.coverImageConfig.visibility
                    ]
                ]
          , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , orientation HORIZONTAL
            ]
            [ textView $
                [ text $ state.primaryText.text
                , color $ state.primaryText.color
                , margin $ state.primaryText.margin
                , gravity $ state.primaryText.gravity
                , width if state.dismissPopupConfig.visibility == VISIBLE then WRAP_CONTENT else MATCH_PARENT
                , height WRAP_CONTENT
                , visibility $ state.primaryText.visibility
                ] <> (FontStyle.getFontStyle state.primaryText.textStyle Common.LanguageStyle)
            , linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , gravity RIGHT
                , visibility state.dismissPopupConfig.visibility
                ]
                [ linearLayout
                    [ height WRAP_CONTENT
                    , width WRAP_CONTENT
                    , margin state.dismissPopupConfig.margin
                    , onClick push $ const (OnClose Common.OnImageClick )--OnImageClick
                    , padding state.dismissPopupConfig.padding
                    ]
                    [ imageView
                        [ width state.dismissPopupConfig.width
                        , height state.dismissPopupConfig.height
                        , imageWithFallback state.dismissPopupConfig.imageUrl
                        , visibility state.dismissPopupConfig.visibility
                        ]
                    ]
                ]
            ]
        , textView $
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , color $ state.secondaryText.color
            , gravity $ state.secondaryText.gravity
            , padding state.secondaryText.padding
            , margin $ state.secondaryText.margin
            , text $ state.secondaryText.text
            , visibility $ state.secondaryText.visibility
            ] <> FontStyle.body1 Common.TypoGraphy
        , contactView push state
        , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , visibility state.editTextVisibility
            ]
            [ PrimaryEditText.view (push <<< ETextController) (state.eTextConfig) ]
        , tipsView push state
        , linearLayout
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , gravity CENTER
            , orientation HORIZONTAL
            , margin state.buttonLayoutMargin
            ]
            [ linearLayout
                [ width $ if state.optionButtonOrientation == "VERTICAL" then MATCH_PARENT else if (not state.option1.visibility) || (not state.option2.visibility) then MATCH_PARENT else WRAP_CONTENT
                , height WRAP_CONTENT
                , orientation if state.optionButtonOrientation == "VERTICAL" then VERTICAL else HORIZONTAL
                ]
                [ linearLayout
                    [ if state.option2.visibility then width state.option1.width else weight 1.0
                    , background state.option1.background
                    , height $ V 48
                    , cornerRadius 8.0
                    , visibility $ if state.option1.visibility then VISIBLE else GONE
                    , stroke $ "1," <> state.option1.strokeColor
                    , clickable state.option1.isClickable
                    , alpha $ if state.option1.isClickable then 1.0 else 0.5
                    , margin  state.option1.margin
                    , padding  state.option1.padding
                    , gravity CENTER
                    , onClick
                        ( \action -> do
                            _ <- push action
                            clearTheTimer state
                            pure unit
                        )
                        (const $ OnClose Common.Button1Click)
                    ]
                    [ textView $
                        [ width WRAP_CONTENT
                        , height WRAP_CONTENT
                        , text $ if state.option1.enableTimer && state.option1.timerValue > 0 then (state.option1.text <> " (" <> (show state.option1.timerValue) <> ")") else state.option1.text
                        , color $ state.option1.color
                        , gravity CENTER
                        ] <> (FontStyle.getFontStyle state.option1.textStyle Common.LanguageStyle)
                    ]
                , linearLayout
                    [ if state.option1.visibility then width state.option2.width else weight 1.0
                    , height state.option2.height
                    , background state.option2.background
                    , cornerRadius 8.0
                    , visibility if state.option2.visibility then VISIBLE else GONE
                    , stroke ("1," <> state.option2.strokeColor)
                    , margin state.option2.margin
                    , gravity CENTER
                    , onClick
                        ( \action -> do
                            _ <- push action
                            clearTheTimer state
                            pure unit
                        )(const $ OnClose Common.Button2Click)
                        -- (const OnButton2Click)
                    , padding state.option2.padding
                    , orientation VERTICAL
                    , clickable state.option2.isClickable
                    , alpha (if state.option2.isClickable then 1.0 else 0.5)
                    ]
                    [ textView $ 
                        [ width WRAP_CONTENT
                        , height WRAP_CONTENT
                        , text $ if state.option2.enableTimer && state.option2.timerValue > 0 then (state.option2.text <> " (" <> (show state.option2.timerValue) <> ")") else state.option2.text
                        , color state.option2.color
                        , gravity CENTER
                        ] <> (FontStyle.getFontStyle state.option2.textStyle Common.LanguageStyle)
                    ]
                ]
            ]
        , textView $
            [ width MATCH_PARENT
            , height WRAP_CONTENT
            , gravity CENTER
            , margin $ MarginTop 5
            , padding $ Padding 5 5 5 10
            , onClick push $ const $ OnClose Common.DismissPopUp
            ] <> (case state.dismisText of
                    Just txt -> [text txt]
                    Nothing -> [visibility GONE]) 
              <> (FontStyle.subHeading2 Common.LanguageStyle)
        ]
      ]
    ]

tipsView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
tipsView push state =
  linearLayout
    [ 
      height WRAP_CONTENT
    , width MATCH_PARENT
    , visibility if state.customerTipAvailable then VISIBLE else GONE
    , orientation VERTICAL
    , margin state.tipLayoutMargin
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , visibility if state.customerTipAvailable then VISIBLE else GONE
        ]( mapWithIndex
        ( \index item ->
            linearLayout
              [ width WRAP_CONTENT
              , height WRAP_CONTENT
              , weight 1.0
              , margin $ state.tipButton.margin
              ]
              [ textView $
                  [ text item
                  , color $ state.tipButton.color
                  , visibility if state.tipButton.visibility then VISIBLE else GONE
                  , clickable $ state.tipButton.isClickable
                  , stroke $ "1," <> (if (state.activeIndex == index) then Color.blue800 else Color.grey900)
                  , cornerRadius 8.0
                  , width WRAP_CONTENT
                  , height WRAP_CONTENT
                  , padding state.tipButton.padding
                  , onClick push $ const $ Tipbtnclick index (fromMaybe 100 (state.customerTipArrayWithValues !! index))
                  , background (if (state.activeIndex == index) then Color.blue600 else state.tipButton.background)
                  ] <> (FontStyle.getFontStyle state.tipButton.textStyle Common.LanguageStyle)
              ]
        ) state.customerTipArray)
    ,   linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background Color.grey700
        , cornerRadius 4.0
        , margin $ MarginTop 12
        , padding $ Padding 20 13 20 13
        ]
        [ 
            linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            ]
            [   imageView
                [ imageWithFallback "ny_ic_wallet_filled,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_wallet_filled.png"
                , width $ V 20
                , height $ V 20
                , margin $ MarginRight 5
                ]
            ,   textView
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text state.fareEstimateText
                , weight 1.0
                , color Color.black800
                , textSize FontSize.a_12
                ]
            ,   textView
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text state.fareEstimate
                , color Color.black800
                , textSize FontSize.a_14
                ]
            ]
        ,   linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , margin $ MarginTop 14
            ]
            [   imageView
                [ imageWithFallback "ny_ic_stop_circle_yellow,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_stop_circle_yellow.png"
                , width $ V 20
                , height $ V 20
                , margin $ MarginRight 5
                ]
            ,   textView
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text state.tipSelectedText
                , weight 1.0
                , color Color.black800
                , textSize FontSize.a_12
                ]
            ,   textView
                [ height WRAP_CONTENT
                , width WRAP_CONTENT
                , text state.tipSelected
                , color Color.black800
                , textSize FontSize.a_14
                ]
            ]
        ]
    ]

clearTheTimer :: Config -> Effect Unit
clearTheTimer config =
  if config.option1.enableTimer then do
    pure $ clearTimer config.option1.timerID
  else if config.option2.enableTimer then do
    pure $ clearTimer config.option2.timerID
  else
    pure unit

contactView :: forall w. (Action -> Effect Unit) -> Config -> PrestoDOM (Effect Unit) w
contactView push state =
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , margin state.contactViewMargin
    , stroke ("1," <> Color.borderColorLight)
    , padding state.contactViewPadding
    , cornerRadius 8.0
    , visibility state.contactViewConfig.visibility
    ]
    [ linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity LEFT
        ]
        [ linearLayout
            [ height $ V 24
            , width $ V 24
            , background Color.yellow900
            , cornerRadius 12.0
            , gravity CENTER
            ]
            [ textView $
                [ text state.contactViewConfig.nameInitials
                , color Color.black800
                ] <> FontStyle.body3 Common.TypoGraphy
            ]
        , linearLayout
            [ height WRAP_CONTENT
            , width WRAP_CONTENT
            , padding state.contactViewConfig.padding
            ]
            [ textView $
                [ text state.contactViewConfig.fullName
                , color Color.black800
                ] <> FontStyle.subHeading1 Common.TypoGraphy
            ]
        ]
    ]
