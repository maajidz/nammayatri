{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.NammaSafetyScreen.ComponentConfig
  where

import Common.Types.App

import Components.GenericHeader as GenericHeader
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Data.Array (any, length, null)
import Data.Maybe (Maybe(..))
import Engineering.Helpers.Commons (os)
import Engineering.Helpers.Commons as EHC
import Font.Size as FontSize
import Font.Style as FontStyle
import Helpers.Utils (getCommonAssetStoreLink)
import JBridge as JB
import Language.Strings (getString)
import Language.Types (STR(..))
import Prelude (not, show, ($), (<>), (==), (>), (||))
import PrestoDOM (Gravity(..), Length(..), Margin(..), Padding(..), Visibility(..), alpha, padding, textFromHtml, visibility)
import PrestoDOM.Types.DomAttributes (Corners(..))
import Screens.Types (NammaSafetyScreenState, NammaSafetyStage(..))
import Styles.Colors as Color

startNSOnboardingButtonConfig :: NammaSafetyScreenState -> PrimaryButton.Config
startNSOnboardingButtonConfig state = 
  PrimaryButton.config
    { textConfig{ text = (getButtonString state.props.currentStage) }
    , isClickable = true
    , visibility = if state.data.hasCompletedSafetySetup == true || state.props.onRide then GONE else VISIBLE
    , margin = (Margin 16 0 16 24 )
    }

continueNextStepButtonConfig:: NammaSafetyScreenState -> PrimaryButton.Config
continueNextStepButtonConfig state = 
  PrimaryButton.config
    { textConfig{ text = if state.props.currentStage == SetPersonalSafetySettings then "Finish Setup" else "Continue" }
    , isClickable = true
    , visibility = VISIBLE
    , margin = (Margin 16 0 16 24 )
    }

editEmergencyContactsBtnConfig :: NammaSafetyScreenState -> PrimaryButton.Config
editEmergencyContactsBtnConfig state = 
  PrimaryButton.config
    { textConfig{ text = (getString EDIT) , color = Color.blue900
    }
    , isClickable = true
    , height = MATCH_PARENT
    , width = WRAP_CONTENT
    , margin = (MarginLeft 9)
    , gravity = CENTER
    , background = Color.white900
    }

cancelSOSBtnConfig :: NammaSafetyScreenState -> PrimaryButton.Config
cancelSOSBtnConfig state = 
  PrimaryButton.config
    { textConfig{ text = "Mark Ride As Safe" , color = Color.black900
    }
    , isClickable = true
    , height = V 48
    , width = MATCH_PARENT
    , margin = (MarginTop 10)
    , gravity = CENTER
    , background = Color.white900
    }

goBackBtnConfig :: NammaSafetyScreenState -> PrimaryButton.Config
goBackBtnConfig state = 
  PrimaryButton.config
    { textConfig{ text = "Go Back" , color = Color.black900
                }
    , isClickable = true
    , height = V 48
    , width = MATCH_PARENT
    , margin = (Margin 16 10 16 24)
    , gravity = CENTER
    , background = Color.white900
    }


getButtonString :: NammaSafetyStage -> String
getButtonString stage = case stage of
  SetTriggerCustomerSupport ->  "Add Emergency Contacts"
  SetPersonalSafetySettings ->  "Finish Setup"
  _ ->  "Start Namma Safety Setup"

genericHeaderConfig :: String -> NammaSafetyScreenState -> GenericHeader.Config 
genericHeaderConfig title state = 
  GenericHeader.config
    {
      height = WRAP_CONTENT
    , width = MATCH_PARENT
    , background = Color.transparent
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = if any (_ == state.props.currentStage)[ActivateNammaSafety, TriggeredNammaSafety] then "ny_ic_chevron_left_white,https://assets.juspay.in/nammayatri/images/common/ny_ic_chevron_left.png" else "ny_ic_chevron_left,https://assets.juspay.in/nammayatri/images/common/ny_ic_chevron_left.png"
      , margin = (Margin 12 14 12 12)
      , visibility = if state.props.currentStage == NammaSafetyVideoRecord then GONE else VISIBLE
      } 
    , textConfig {
        text = title
      , color = if any (_ == state.props.currentStage)[ActivateNammaSafety, TriggeredNammaSafety, NammaSafetyVideoRecord] then Color.white900 else Color.black800
      , margin = if state.props.currentStage == NammaSafetyVideoRecord then MarginLeft 16 else Margin 0 0 0 0
      }
    , suffixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = "ny_ic_close_white,https://assets.juspay.in/nammayatri/images/common/ny_ic_close_white.png"
      , margin = (Margin 12 14 12 12)
      , visibility = if state.props.currentStage == NammaSafetyVideoRecord then VISIBLE else GONE
      }
    , padding = (Padding 0 5 0 5)
    }
  



activateSoSButtonConfig :: NammaSafetyScreenState -> PrimaryButton.Config
activateSoSButtonConfig state = 
  PrimaryButton.config
    { textConfig{ text = "Activate SOS" , color = Color.black900}
    , isClickable = true
    , margin = (Margin 16 0 16 8 )
    , background = Color.white900
    }

dismissSoSButtonConfig :: NammaSafetyScreenState -> PrimaryButton.Config
dismissSoSButtonConfig state = 
  PrimaryButton.config
    { textConfig{ text = "Dismiss" , color = Color.white900}
    , isClickable = true
    , margin = (Margin 16 8 16 0 )
    , background = Color.black900
    , stroke = ("1," <> Color.white900)
    }

contactListPrimaryButtonConfig :: Int -> PrimaryButton.Config
contactListPrimaryButtonConfig count =
  let
    config' = PrimaryButton.config
    primaryButtonConfig' =
      config'
        { textConfig
          { text = if (count > 0) then (getString CONFIRM_EMERGENCY_CONTACTS) else (getString SELECT_CONTACTS)
          , color = Color.yellow900
          }
        , alpha = if (count > 0) then 1.0 else 0.6
        , background =  Color.black900
        , isClickable = if (count > 0) then true else false
        , id = "ContactListPrimaryButton"
        }
  in
    primaryButtonConfig'

--------------------------------------------------- primaryButtonConfig -----------------------------------------------------
addContactButtonConfig :: NammaSafetyScreenState -> PrimaryButton.Config
addContactButtonConfig state =
  let
    config = PrimaryButton.config

    primaryButtonConfig' =
      config
        { textConfig
          { text = if null state.data.emergencyContactsData.contactsList then (getString ADD_EMERGENCY_CONTACTS) else (getString ADD_ANOTHER_CONTACT)
          }
        , isClickable = true
        , width = if os == "IOS" then (V 360) else (MATCH_PARENT)
        , margin = (MarginBottom 24)
        , visibility = if ((length state.data.emergencyContactsData.contactsList) == 3) then GONE else VISIBLE
        }
  in
    primaryButtonConfig'


--------------------------------------------------- removeContactPopUpModelConfig -----------------------------------------------------
removeContactPopUpModelConfig :: NammaSafetyScreenState -> PopUpModal.Config
removeContactPopUpModelConfig state =
  let
    config' = PopUpModal.config

    popUpConfig' =
      config'
        { primaryText { text = (getString REMOVE) <> " " <> state.data.emergencyContactsData.removedContactDetail.name }
        , secondaryText { text = (getString ARE_YOU_SURE_YOU_WANT_TO_REMOVE_CONTACT) }
        , option1
          { text = (getString CANCEL_)
          , strokeColor = Color.black700
          }
        , option2
          { text = (getString YES_REMOVE)
          , background = Color.red
          , color = Color.white900
          , strokeColor = Color.red
          }
        , backgroundClickable = false
        , buttonLayoutMargin = MarginBottom if os == "IOS" then 0 else 24
        }
  in
    popUpConfig'

  
confirmPopUpModelConfig :: NammaSafetyScreenState -> PopUpModal.Config
confirmPopUpModelConfig state = 
  PopUpModal.config {
      cornerRadius = Corners 15.0 true true true true
      , margin = MarginHorizontal 16 16
      , padding = Padding 16 16 16 16
      , gravity = CENTER
      , backgroundColor =  Color.black9000
      , backgroundClickable = false
      , buttonLayoutMargin = MarginBottom 0
      , optionButtonOrientation = "VERTICAL"

    ,primaryText {
        text = "You are about to activate SOS"
                 
      , margin = Margin 16 16 16 0
      , visibility = VISIBLE
      , color = Color.black800
      -- , textStyle = Heading2
     },
      option1 {
        text =  "Confirm SOS Activation"
                 
      , color = Color.yellow900
      , background = Color.black
      , visibility =true
      , margin = MarginTop 16
      , width =  MATCH_PARENT
      },
    secondaryText {
      text = "Your safety is our top priority. Use SOS only during genuine emergencies. Misuse may divert resources from real critical situations"
      , color = Color.black700
      , margin = Margin 16 4 16 0
      , visibility = VISIBLE 
      -- , textStyle = if Mb.Just SupportPopup == state.props.popUpState then SubHeading1 else Body1
      },
    option2 { 
      visibility = true
      , text = "Dismiss"
      , color = Color.black650
      , background = Color.white900
      , strokeColor = Color.white900
      , width = MATCH_PARENT
      , margin = (Margin 0 0 0 0)
    }
    }