{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.DriverRideRatingScreen.View where

import PrestoDOM (Gravity(..), InputType(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, alignParentBottom, background, clickable, color, cornerRadius, editText, fontStyle, gravity, height, hint, imageUrl, imageView, inputType, lineHeight, linearLayout, margin, onBackPressed, onChange, onClick, orientation, padding, relativeLayout, singleLine, stroke, text, textSize, textView, weight, width, pattern, scrollView, afterRender, imageWithFallback)
import Screens.DriverRideRatingScreen.Controller (Action(..), ScreenOutput, eval, getFeedBackString)
import Prelude (Unit, const, unit, ($), (-), (<<<), (<=), (<>), (==), bind, pure)
import Screens.Types as ST
import Screens.DriverRideRatingScreen.ScreenData(feedbackSuggestionArray)
import Components.PrimaryButton as PrimaryButton
import Engineering.Helpers.Commons as EHC
import Language.Strings (getString)
import Animation as Anim
import Data.Array as DA 
import Data.Array ((!!))
import Language.Types (STR(..))
import Font.Style as FontStyle
import Styles.Colors as Color
import Font.Size as FontSize
import Effect (Effect)
import Data.Maybe
import Common.Types.App
import Screens.DriverRideRatingScreen.ComponentConfig
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import Common.Types.App (LazyCheck(..))

screen :: ST.DriverRideRatingScreenState -> Screen Action ST.DriverRideRatingScreenState ScreenOutput
screen initialState =
  { initialState
  , view
  , name : "DriverRideRatingScreen"
  , globalEvents : []
  , eval
  }

view :: forall w . (Action -> Effect Unit) -> ST.DriverRideRatingScreenState -> PrestoDOM (Effect Unit) w
view push state =
  Anim.screenAnimation $
    linearLayout
        [ height MATCH_PARENT
        , width MATCH_PARENT
        , orientation VERTICAL
        , background Color.white900
        , onBackPressed push $ (const BackPressed)
        , afterRender (\action -> do
            _ <- push action
            pure unit
            ) $ const AfterRender
        ][  scrollView
            [ width MATCH_PARENT
            , orientation VERTICAL
            , background Color.white900
            , padding (Padding 16 16 16 16)
            , weight 1.0
            ][ linearLayout
                [ height WRAP_CONTENT
                , width MATCH_PARENT
                , orientation VERTICAL
                ][ topCloseButtonView state push
                 , ratingScreenView state push
                 ]
            ]
          , linearLayout
            [ height WRAP_CONTENT
            , width MATCH_PARENT
            , alignParentBottom "true,-1"
            ][PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state)]
          ]



topCloseButtonView :: forall w . ST.DriverRideRatingScreenState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
topCloseButtonView state push = 
    linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , gravity RIGHT 
    ][  linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , onClick push $ const BackPressed
        ][  imageView
            [ height $ V 25
            , width $ V 25
            , imageWithFallback $ "ny_ic_close," <> (getCommonAssetStoreLink FunctionCall) <> "/ny_ic_close.png"
            , margin (Margin 12 12 12 12)
            ]
          ]
        ]


ratingScreenView :: forall w . ST.DriverRideRatingScreenState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
ratingScreenView state push = 
  relativeLayout
  [ height MATCH_PARENT
  , width MATCH_PARENT
  , orientation VERTICAL
  ][ linearLayout
      [ height WRAP_CONTENT
      , width MATCH_PARENT
      , orientation VERTICAL
      ][  starRatingView state push
        , suggestionButtonView state push
        , writeCommentView state push
        ]
  ]

starRatingView :: forall w . ST.DriverRideRatingScreenState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
starRatingView state push = 
  linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT
    , orientation VERTICAL
    , gravity CENTER
    ][ textView
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , textSize FontSize.a_16
        , text $ (getString HOW_WAS_YOUR_RIDE_WITH ) <> " " <> state.data.customerName
        , color Color.black800
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , gravity CENTER
        , lineHeight "20"
        , margin (MarginVertical 32 18)
        ]
    , linearLayout
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , gravity CENTER
        ](DA.mapWithIndex (\index item -> 
                          linearLayout
                          [ height WRAP_CONTENT
                          , width WRAP_CONTENT
                          , margin (MarginHorizontal 5 5)
                          , onClick push $ const (Rating index)
                          ][imageView
                              [ height $ V 30
                              , width $ V 30
                              , imageWithFallback if index <= state.data.rating then "ny_ic_star_active," <> (getCommonAssetStoreLink FunctionCall) <> "/ny_ic_star_active.png" else "ny_ic_star_inactive," <> (getCommonAssetStoreLink FunctionCall) <> "/ny_ic_star_inactive.png"
                              ]
                          ]) [1,2,3,4,5])
    ]

writeCommentView :: forall w . ST.DriverRideRatingScreenState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
writeCommentView state push = 
  linearLayout
    [ height $ V 100
    , width $ V (EHC.screenWidth unit - 32)
    , margin (MarginVertical 22 60)
    , background Color.grey800
    , cornerRadius 10.0
    ][linearLayout
        [ height WRAP_CONTENT
        , width WRAP_CONTENT
        , gravity START
        , margin (Margin 20 20 10 0)
        ][imageView
          [ height $ V 20
          , width $ V 20 
          , imageWithFallback $ "ny_ic_message_square," <> (getCommonAssetStoreLink FunctionCall) <> "/ny_ic_message_square.png"
          ]
        ]
      , editText
        [ height $ V 150
        , width MATCH_PARENT
        , textSize FontSize.a_12
        , color Color.black
        , fontStyle $ FontStyle.regular LanguageStyle
        , hint $ getString WRITE_A_COMMENT
        , inputType TypeText
        , gravity START
        , background Color.grey800
        , lineHeight "18"
        , weight 1.0
        , pattern "[^\n]*,255"
        , singleLine false
        , margin (MarginTop 12)
        , onChange push FeedbackChanged
        ]
      ]

suggestionButtonView :: forall w . ST.DriverRideRatingScreenState -> (Action  -> Effect Unit) -> PrestoDOM (Effect Unit) w
suggestionButtonView state push = 
   linearLayout
    [ height WRAP_CONTENT
    , width MATCH_PARENT 
    , orientation VERTICAL
    , gravity CENTER
    , margin (MarginTop 40)
    ][  textView
        [ height WRAP_CONTENT
        , width MATCH_PARENT
        , textSize FontSize.a_16
        , text $ getString GOT_IT_TELL_US_MORE
        , color Color.black800
        , fontStyle $ FontStyle.semiBold LanguageStyle
        , gravity CENTER
        , lineHeight "20"
        , margin (MarginBottom 10)
        ]
        , linearLayout
          [ width MATCH_PARENT
          , orientation VERTICAL  
          ](DA.mapWithIndex (\index item -> 
              linearLayout
                [ width WRAP_CONTENT
                , orientation HORIZONTAL  
                , margin (MarginTop 10)
                ](DA.mapWithIndex (\index item -> 
                    linearLayout
                      [ height $ V 30
                      , width MATCH_PARENT
                      , background if (item == fromMaybe ST.NOTHING state.data.activeFeedBackOption) then Color.black800 else Color.grey800
                      , stroke ("1," <> Color.grey900)
                      , gravity CENTER
                      , margin (MarginRight 10)
                      , onClick push (const $ FeedBackClick item)
                      , cornerRadius 7.0
                      ][textView
                          [ height WRAP_CONTENT
                          , width MATCH_PARENT
                          , textSize FontSize.a_12
                          , text $ getFeedBackString item
                          , color if (item == fromMaybe ST.NOTHING state.data.activeFeedBackOption) then Color.white900 else Color.black800
                          , fontStyle $ FontStyle.medium LanguageStyle
                          , gravity CENTER
                          , lineHeight "15"
                          , singleLine true
                          , padding (Padding 6 2 6 2)
                          ]
                        ]) (fromMaybe [] (feedbackSuggestionArray!!index)) )
                    ) feedbackSuggestionArray)
        ]
