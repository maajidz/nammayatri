#creating a new screen given the directory name 


#----------------------------- updating Types.purs -----------------------------------------------
# echo " -- ######################################### $1ScreenState ####################################################

# type $1ScreenState = {
#   data :: $1ScreenData ,
#   props :: $1ScreenProps
# }

# type $1ScreenData = {} 

# type $1ScreenProps = {} " >> $2/src/Screens/Types.purs


mkdir $2/src/Screens/CustomerUtils/$1
cd $2/src/Screens/CustomerUtils/$1
touch View.purs
touch Controller.purs
touch ScreenData.purs
touch Handler.purs
touch ComponentConfig.purs

#-------------------------------------- populating View.purs ---------------------------------------
echo "module Screens.$1.View where

import PrestoDOM (Gravity(..), Length(..), Margin(..), Orientation(..), Padding(..), PrestoDOM, Screen, background, gravity, height, linearLayout, margin, onBackPressed, orientation, padding, weight, width)
import Screens.Types as ST
import Styles.Colors as Color
import Effect (Effect)
import Animation as Anim
import Common.Types.App (LazyCheck(..))
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Prelude (Unit, const, ($), (<<<))

screen :: ST.$1ScreenState -> Screen Action ST.$1ScreenState ScreenOutput
screen initialState = 
  { initialState
  , view
  , name : \"$1Screen\"
  , globalEvents : [] 
  , eval : 
      (\state action -> do 
          let _ = spy \"$1Screen -----------state\" state 
          let _ = spy \"$1Screen -----------action\" action
          eval state action
      )
  }

view :: forall w. (Action -> Effect Unit) -> ST.$1ScreenState -> PrestoDOM (Effect Unit) w 
view push state = 
  Anim.screenAnimation 
    $ linearLayout
      [ height MATCH_PARENT
      , width MATCH_PARENT
      , orientation VERTICAL 
      , gravity CENTER
      , onBackPressed push $ const BackPressed 
      , background Color.white900
      , padding $ Padding 16 16 16 16
      ][  GenericHeader.view (push <<< GenericHeaderAC) (genericHeaderConfig state)
        , linearLayout 
          [ weight 1.0 
          , width MATCH_PARENT
          , gravity CENTER 
          ][  textView
              [ text \"Dummy Text\"
              , color Color.black900
              , textSize FontSize.a_14
              , fontStyle FontStyle.bold LanguageStyle
              ]
            ]
        , linearLayout
          [ height WRAP_CONTENT
          , width MATCH_PARENT  
          , gravity BOTTOM
          , weight 1.0
          ][  PrimaryButton.view (push <<< PrimaryButtonActionController) (primaryButtonConfig state)]
      ]" > View.purs

#----------------------------------------------------------------------------------------------------------

# -------------------------------------- populating Controller.purs ---------------------------------------

echo "{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.$1Screen.Controller where 

import Components.GenericHeader.Controller (Action(..)) as GenericHeaderController
import Components.PrimaryButton.Controller as PrimaryButtonController
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Prelude (class Show, pure, unit, bind, discard, ($), (/=), (==))
import PrestoDOM (Eval, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.Types ($1ScreenState)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    _ -> pure unit

data Action = PrimaryButtonActionController PrimaryButtonController.Action 
            | GenericHeaderActionController GenericHeaderController.Action
            | BackPressed

data ScreenOutput = GoBack

eval :: Action -> $1ScreenState -> Eval Action ScreenOutput $1ScreenState

eval (PrimaryButtonActionController PrimaryButtonController.OnClick) state = continue state

eval (GenericHeaderActionController (GenericHeaderController.PrefixImgOnClick )) state = continueWithCmd state [do pure BackPressed]

eval BackPressed state = exit $ GoBack

eval _ state = continue state" > Controller.purs

#----------------------------------------------------------------------------------------------------------

# -------------------------------------- populating Handler.purs ---------------------------------------

echo "{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.$1Screen.Handler where

import Engineering.Helpers.BackTrack (getState)
import Prelude (bind, pure, discard, ($), (<$>))
import Screens.$1Screen.Controller (ScreenOutput(..))
import Control.Monad.Except.Trans (lift)
import ModifyScreenState (modifyScreenState)
import Control.Transformers.Back.Trans as App
import PrestoDOM.Core.Types.Language.Flow (runScreen)
import Screens.$1Screen.View as $1Screen
import Types.App (FlowBT, GlobalState(..), $1_SCREEN_OUTPUT(..),ScreenType(..))

$1Screen :: FlowBT String $1_SCREEN_OUTPUT
$1Screen = do
  (GlobalState state) <- getState
  action <- lift $ lift $ runScreen $ $1Screen.screen state.$1Screen
  case action of
    GoBack -> App.BackT $ App.GoBack " > Handler.purs


#----------------------------------------------------------------------------------------------------------

# -------------------------------------- populating ScreenData.purs ---------------------------------------

echo "{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.$1Screen.ScreenData where

import Screens.Types ($1ScreenState)

initData :: $1ScreenState 
initData = 
  { data : {}
  , props : {}
  } " > ScreenData.purs

#----------------------------------------------------------------------------------------------------------

# -------------------------------------- populating ComponentConfig.purs ---------------------------------------

echo "{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.CustomerUtils.$1Screen.ComponentConfig where 

import Components.GenericHeader as GenericHeader 
import Components.PrimaryButton as PrimaryButton 
import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..))
import Screens.Types as ST 
import Styles.Colors as Color
import PrestoDOM (Length(..), Margin(..), Padding(..), Visibility(..))
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)


primaryButtonConfig :: ST.$1ScreenState -> PrimaryButton.Config
primaryButtonConfig state = let 
    config = PrimaryButton.config
    primaryButtonConfig' = config 
      {   textConfig
         { text = \"Button\"
         } 
        , margin = (Margin 0 0 0 0)
        , id = \"DummyButton\"
      }
  in primaryButtonConfig'

genericHeaderConfig :: ST.$1eScreenState -> GenericHeader.Config 
genericHeaderConfig state = let 
  genericHeaderConfig' = GenericHeader.config 
    {
      height = WRAP_CONTENT
    , prefixImageConfig {
        height = V 25
      , width = V 25
      , imageUrl = \"ny_ic_chevron_left,\" <> (getCommonAssetStoreLink FunctionCall) <> \"ny_ic_chevron_left.png\"
      } 
    , textConfig {
        text = \"Generic Header\"
      }
    , suffixImageConfig {
        visibility = GONE
      }
    }
  in genericHeaderConfig' " > ComponentConfig.purs