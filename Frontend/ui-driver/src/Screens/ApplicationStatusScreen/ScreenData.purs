{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.ApplicationStatusScreen.ScreenData where

import Screens.Types
import Common.Types.App (PopUpStatus(..))
import Data.Maybe(Maybe(..)) as Mb

initData :: ApplicationStatusScreenState
initData = {
  data: {
    dlVerificationStatus : "",
    rcVerificationStatus : "",
    mobileNumber : "",
    otpValue : "",
    popUpConfig : {
      status : CLOSED ,
      actionType : Mb.Nothing
      }
    },
  props : {
      isSelected : true,
      onBoardingFailure : false,
      isVerificationFailed : false,
      popupview : false,
      enterMobileNumberView : false,
      buttonVisibilty : false,
      enterOtp : false,
      alternateNumberAdded : false,
      isValidAlternateNumber : false,
      isValidOtp : false,
      isAlternateMobileNumberExists : false
  }
}