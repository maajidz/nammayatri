{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module KN where

import Language.Types (STR(..))

getKN ::STR -> String
getKN script = case script of
  LETS_GET_STARTED -> "ಪ್ರಾರಂಭಿಸೋಣ"
  YOUR_APPLICATION_HAS_BEEN_SUBMITTED_SUCCESSFULLY_AND_IS_UNDER_VERIFICATION -> "ನಿಮ್ಮ ಅರ್ಜಿಯನ್ನು ಯಶಸ್ವಿಯಾಗಿ ಸಲ್ಲಿಸಲಾಗಿದೆ ಮತ್ತು ಪರಿಶೀಲನೆ ಹಂತದಲ್ಲಿದೆ"
  VIEW_STATUS -> "ಸ್ಥಿತಿಯನ್ನು ವೀಕ್ಷಿಸಿ"
  GO_HOME -> "ಮನೆಗೆ ಹೋಗು"
  SELECT_LANGUAGE -> "ಭಾಷೆಯನ್ನು ಆಯ್ಕೆಮಾಡಿ"
  WHICH_LANGUAGE_DO_YOU_PREFER -> "ನೀವು ಯಾವ ಭಾಷೆಗೆ ಆದ್ಯತೆ ನೀಡುತ್ತೀರಿ?"
  NEXT -> "ಮುಂದೆ"
  T_C -> "ಬಳಕೆಯ ನಿಯಮಗಳು ಮತ್ತು ಗೌಪ್ಯತೆ ನೀತಿ"
  ENTER_MOBILE_NUMBER -> "ಮೊಬೈಲ್ ಸಂಖ್ಯೆಯನ್ನು ನಮೂದಿಸಿ"
  BY_CLICKING_NEXT_YOU_WILL_BE_AGREEING_TO_OUR -> "ಮುಂದೆ ಟ್ಯಾಪ್ ಮಾಡುವ ಮೂಲಕ\nಎ) ನೀವು ಬೀಟಾ ಪರೀಕ್ಷೆಯಲ್ಲಿ ಭಾಗವಹಿಸಲು ಸಿದ್ಧರಿದ್ದೀರಿ ಎಂದು ನೀವು ಒಪ್ಪುತ್ತೀರಿ ಮತ್ತು ಜಸ್ಪೇ ನಿಮ್ಮ ವಿರುದ್ಧ ಯಾವುದೇ ಹೊಣೆಗಾರಿಕೆಯನ್ನು ಹೊಂದಿರುವುದಿಲ್ಲ"
  ENTER_OTP -> "OTP ನಮೂದಿಸಿ"
  DIDNT_RECIEVE_OTP -> "OTP ಸ್ವೀಕರಿಸಲಿಲ್ಲವೇ?"
  RESEND_OTP -> "OTP ಅನ್ನು ಮರುಕಳುಹಿಸಿ"
  PLEASE_ENTER_VALID_OTP -> "ದಯವಿಟ್ಟು ಮಾನ್ಯ OTP ಅನ್ನು ನಮೂದಿಸಿ"
  INVALID_MOBILE_NUMBER -> "ಅಮಾನ್ಯ ಮೊಬೈಲ್ ಸಂಖ್ಯೆ"
  REGISTER -> "ನೋಂದಣಿ"
  MOBILE_NUMBER -> "ಮೊಬೈಲ್ ನಂಬರ"
  AUTO_READING_OTP -> "ಸ್ವಯಂ ಓದುವಿಕೆ OTP..."
  UPLOAD_DRIVING_LICENSE -> "ಚಾಲನಾ ಪರವಾನಗಿಯನ್ನು ಅಪ್‌ಲೋಡ್ ಮಾಡಿ"
  UPLOAD_BACK_SIDE -> "ಬ್ಯಾಕ್ ಸೈಡ್ ಅನ್ನು ಅಪ್‌ಲೋಡ್ ಮಾಡಿ"
  UPLOAD_FRONT_SIDE -> "ನಿಮ್ಮ ಡಿಎಲ್‌ನ ಫೋಟೋ ಭಾಗವನ್ನು ಅಪ್‌ಲೋಡ್ ಮಾಡಿ"
  BACK_SIDE -> "ಬ್ಯಾಕ್ ಸೈಡ್"
  FRONT_SIDE -> "ನಿಮ್ಮ DL ನ ಫೋಟೋ ಸೈಡ್"
  LICENSE_INSTRUCTION_PICTURE -> "ದಯವಿಟ್ಟು ಪರವಾನಗಿಯ ಎರಡೂ ಬದಿಗಳ ಸ್ಪಷ್ಟ ಚಿತ್ರಗಳನ್ನು ಅಪ್‌ಲೋಡ್ ಮಾಡಿ"
  LICENSE_INSTRUCTION_CLARITY -> "ಫೋಟೋ ಮತ್ತು ಎಲ್ಲಾ ವಿವರಗಳು ಸ್ಪಷ್ಟವಾಗಿ ಗೋಚರಿಸುತ್ತವೆ ಎಂದು ಖಚಿತಪಡಿಸಿಕೊಳ್ಳಿ"
  REGISTRATION_STEPS -> "ನೋಂದಣಿ ಹಂತಗಳು"
  PROGRESS_SAVED -> "ನಿಮ್ಮ ಪ್ರಗತಿಯನ್ನು ಉಳಿಸಲಾಗಿದೆ, ಯಾವುದೇ ಮಾಹಿತಿಯನ್ನು ಬದಲಾಯಿಸಲು ನೀವು ಹಿಂದಿನ ಹಂತಗಳಿಗೆ ಹಿಂತಿರುಗಬಹುದು"
  DRIVING_LICENSE -> "ಚಾಲನಾ ಪರವಾನಿಗೆ"
  AADHAR_CARD -> "ಆಧಾರ್ ಕಾರ್ಡ್"
  BANK_DETAILS -> "ಬ್ಯಾಂಕ್ ವಿವರಗಳು"
  VEHICLE_DETAILS -> "ವಾಹನದ ವಿವರಗಳು"
  UPLOAD_FRONT_BACK -> "ಮುಂಭಾಗ ಮತ್ತು ಹಿಂಭಾಗವನ್ನು ಅಪ್ಲೋಡ್ ಮಾಡಿ"
  EARNINGS_WILL_BE_CREDITED -> "ನಿಮ್ಮ ಗಳಿಕೆಯು ಇಲ್ಲಿ ಕ್ರೆಡಿಟ್ ಆಗಿರುತ್ತದೆ"
  FILL_VEHICLE_DETAILS -> "ನಿಮ್ಮ ವಾಹನದ ವಿವರಗಳನ್ನು ಭರ್ತಿ ಮಾಡಿ"
  FOLLOW_STEPS -> "ನೋಂದಾಯಿಸಲು ದಯವಿಟ್ಟು ಕೆಳಗಿನ ಹಂತಗಳನ್ನು ಅನುಸರಿಸಿ"
  REGISTRATION -> "ನೋಂದಣಿ"
  UPLOAD_ADHAAR_CARD -> "ಆಧಾರ್ ಕಾರ್ಡ್ ಅನ್ನು ಅಪ್ಲೋಡ್ ಮಾಡಿ"
  ADHAAR_INTRUCTION_PICTURE -> "ದಯವಿಟ್ಟು ಆಧಾರ್ ಕಾರ್ಡ್‌ನ ಎರಡೂ ಬದಿಗಳ ಸ್ಪಷ್ಟ ಚಿತ್ರಗಳನ್ನು ಅಪ್‌ಲೋಡ್ ಮಾಡಿ"
  ADD_VEHICLE_DETAILS -> "ವಾಹನದ ವಿವರಗಳನ್ನು ಸೇರಿಸಿ"
  VEHICLE_REGISTRATION_NUMBER -> "ವಾಹನ ನೋಂದಣಿ ಸಂಖ್ಯೆ"
  RE_ENTER_VEHICLE_REGISTRATION_NUMBER -> "ಮತ್ತೆ ವಾಹನ ನೋಂದಣಿ ಸಂಖ್ಯೆ"
  ENTER_VEHICLE_NO -> "ವಾಹನದ ಸಂಖ್ಯೆಯನ್ನು ನಮೂದಿಸಿ."
  VEHICLE_TYPE -> "ವಾಹನದ ಪ್ರಕಾರ"
  VEHICLE_MODEL_NAME -> "ವಾಹನದ ಮಾದರಿ ಹೆಸರು"
  ENTER_MODEL_NAME -> "ಮಾದರಿ ಹೆಸರನ್ನು ನಮೂದಿಸಿ"
  VEHICLE_COLOUR -> "ವಾಹನದ ಬಣ್ಣ"
  ENTER_VEHICLE_COLOUR -> "ವಾಹನದ ಬಣ್ಣವನ್ನು ನಮೂದಿಸಿ"
  UPLOAD_REGISTRATION_CERTIFICATE -> "ನೋಂದಣಿ ಪ್ರಮಾಣಪತ್ರವನ್ನು ಅಪ್‌ಲೋಡ್ ಮಾಡಿ (RC)"
  UPLOAD_RC -> "ಆರ್ಸಿ ಅಪ್ಲೋಡ್ ಮಾಡಿ"
  PREVIEW -> "ಮುನ್ನೋಟ"
  CHOOSE_VEHICLE_TYPE -> "ವಾಹನದ ಪ್ರಕಾರವನ್ನು ಆರಿಸಿ"
  BENIFICIARY_NUMBER -> "ಫಲಾನುಭವಿ ಖಾತೆ ಸಂಖ್ಯೆ"
  RE_ENTER_BENIFICIARY_NUMBER -> "ಫಲಾನುಭವಿ ಖಾತೆ ಸಂಖ್ಯೆ ಮರು-ನಮೂದಿಸಿ."
  IFSC_CODE -> "IFSC ಕೋಡ್"
  SENDING_OTP -> "OTP ಕಳುಹಿಸಲಾಗುತ್ತಿದೆ"
  PLEASE_WAIT_WHILE_IN_PROGRESS -> "ಪ್ರಗತಿಯಲ್ಲಿರುವಾಗ ದಯವಿಟ್ಟು ನಿರೀಕ್ಷಿಸಿ"
  LIMIT_EXCEEDED -> "ಮಿತಿ ಮೀರಿದೆ"
  YOUR_REQUEST_HAS_TIMEOUT_TRY_AGAIN -> "ನಿಮ್ಮ ವಿನಂತಿಯ ಅವಧಿ ಮೀರಿದೆ ಮತ್ತೆ ಪ್ರಯತ್ನಿಸಿ"
  ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER -> "ದೋಷ ಸಂಭವಿಸಿದೆ ದಯವಿಟ್ಟು ನಂತರ ಮತ್ತೆ ಪ್ರಯತ್ನಿಸಿ"
  LIMIT_EXCEEDED_PLEASE_TRY_AGAIN_AFTER_10MIN -> "ಮಿತಿ ಮೀರಿದೆ ದಯವಿಟ್ಟು ನಂತರ ಮತ್ತೆ ಪ್ರಯತ್ನಿಸಿ"
  ENTER_OTP_SENT_TO -> "OTP ಗೆ ಕಳುಹಿಸಲಾಗಿದೆ "
  OTP_SENT_TO -> " ಗೆ ಕಳುಹಿಸಲಾದ OTP ಅನ್ನು ನಮೂದಿಸಿ"
  COUNTRY_CODE_INDIA -> "+91"
  ENTER_ACCOUNT_NUMBER -> "ಖಾತೆ ಸಂಖ್ಯೆ ನಮೂದಿಸಿ."
  ADD_BANK_DETAILS -> "ಬ್ಯಾಂಕ್ ವಿವರಗಳನ್ನು ಸೇರಿಸಿ"
  ENTER_IFSC_CODE -> "IFSC ಕೋಡ್ ನಮೂದಿಸಿ"
  SUBMIT -> "ಸಲ್ಲಿಸು"
  PERSONAL_DETAILS -> "ವೈಯಕ್ತಿಕ ವಿವರಗಳು"
  LANGUAGES -> "ಭಾಷೆಗಳು"
  HELP_AND_FAQ -> "ಸಹಾಯ ಮತ್ತು FAQ ಗಳು"
  ABOUT -> "ಬಗ್ಗೆ"
  LOGOUT -> "ಲಾಗ್ ಔಟ್"
  UPDATE -> "ನವೀಕರಿಸಿ"
  EDIT -> "ತಿದ್ದು"
  AUTO -> "ಆಟೋ"
  NAME -> "ಹೆಸರು"
  PRIVACY_POLICY -> "ಗೌಪ್ಯತಾ ನೀತಿ"
  LOGO -> "ಲೋಗೋ"
  ABOUT_APP_DESCRIPTION -> "ಯಾತ್ರಿ ಪಾಲುದಾರರು ಪ್ರಯಾಣಿಕರೊಂದಿಗೆ ಚಾಲಕರನ್ನು ಸಂಪರ್ಕಿಸಲು ಮುಕ್ತ ವೇದಿಕೆಯಾಗಿದೆ. ಅಪ್ಲಿಕೇಶನ್ ಚಾಲಕರು ಪ್ರಯಾಣಿಕರನ್ನು ಹುಡುಕಲು ಅನುಕೂಲವಾಗುವಂತೆ ಮಾಡುತ್ತದೆ. ಮತ್ತು ಅವುಗಳನ್ನು ಸೇವಾ ಪೂರೈಕೆದಾರರೊಂದಿಗೆ ಸಂಪರ್ಕಿಸುವ ಮೂಲಕ ಈ ಆಯ್ಕೆಗಳನ್ನು ಪಡೆದುಕೊಳ್ಳಿ"
  TERMS_AND_CONDITIONS -> "ನಿಯಮ ಮತ್ತು ಶರತ್ತುಗಳು"
  UPDATE_VEHICLE_DETAILS -> "ವಾಹನದ ವಿವರಗಳನ್ನು ನವೀಕರಿಸಿ"
  Help_AND_SUPPORT -> "ಸಹಾಯ ಮತ್ತು ಬೆಂಬಲ"
  NOTE -> "ಸೂಚನೆ:"
  VISIT_MY_RIDES_SCREEN_FOR_SPECIFIC_COMPLAINTS -> "ನಿರ್ದಿಷ್ಟ ದೂರುಗಳಿಗಾಗಿ ನನ್ನ ರೈಡ್ಸ್ ವಿಭಾಗಕ್ಕೆ ಭೇಟಿ ನೀಡಿ"
  THANK_YOU_FOR_WRTITTING_US -> "ನಮಗೆ ಬರೆದಿದ್ದಕ್ಕಾಗಿ ಧನ್ಯವಾದಗಳು!"
  WE_HAVE_RECIEVED_YOUR_ISSUE -> "ನಿಮ್ಮ ಸಮಸ್ಯೆಯನ್ನು ನಾವು ಸ್ವೀಕರಿಸಿದ್ದೇವೆ. ನಾವು ಸ್ವಲ್ಪ ಸಮಯದ ನಂತರ ನಿಮ್ಮನ್ನು ಸಂಪರ್ಕಿಸುತ್ತೇವೆ."
  GO_TO_HOME -> "ಮನೆಗೆ ಹೋಗು"
  YOUR_RECENT_RIDE -> "ನಿಮ್ಮ ಇತ್ತೀಚಿನ ಸವಾರಿ"
  ALL_TOPICS -> "ಎಲ್ಲಾ ವಿಷಯಗಳು"
  REPORT_AN_ISSUE_WITH_THIS_TRIP -> "ಈ ಪ್ರವಾಸದಲ್ಲಿ ಸಮಸ್ಯೆಯನ್ನು ವರದಿ ಮಾಡಿ"
  YOU_RATED -> "ನೀವು ರೇಟ್ ಮಾಡಿದ್ದೀರಿ"
  VIEW_ALL_RIDES -> "ಎಲ್ಲಾ ರೈಡ್‌ಗಳನ್ನು ವೀಕ್ಷಿಸಿ"
  WRITE_TO_US -> "ನಮಗೆ ಬರೆಯಿರಿ"
  SUBJECT -> "ವಿಷಯ"
  YOUR_EMAIL_ID -> "ನಿಮ್ಮ ಇಮೇಲ್ ಐಡಿ"
  DESCRIBE_YOUR_ISSUE -> "ನಿಮ್ಮ ಸಮಸ್ಯೆಯನ್ನು ವಿವರಿಸಿ"
  GETTING_STARTED_AND_FAQ -> "ಪ್ರಾರಂಭಿಸುವಿಕೆ ಮತ್ತು FAQ ಗಳು"
  FOR_OTHER_ISSUES_WRITE_TO_US -> "ಇತರ ಸಮಸ್ಯೆಗಳಿಗಾಗಿ, ನಮಗೆ ಬರೆಯಿರಿ"
  YOU_CAN_DESCRIBE_ISSUE_THAT_YOU_FACED_HERE -> "ನೀವು ಎದುರಿಸಿದ ಸಮಸ್ಯೆಯನ್ನು ಇಲ್ಲಿ ವಿವರಿಸಬಹುದು"
  REGISTRATION_CERTIFICATE_IMAGE -> "ನೋಂದಣಿ ಪ್ರಮಾಣಪತ್ರ (RC) ಚಿತ್ರ"
  HOME -> "ಮುಖಪುಟ"
  RIDES -> "ಸವಾರಿಗಳು"
  PROFILE -> "ಪ್ರೊಫೈಲ್"
  ENTER_DRIVING_LICENSE_NUMBER -> "ಚಾಲನಾ ಪರವಾನಗಿ ಸಂಖ್ಯೆಯನ್ನು ನಮೂದಿಸಿ"
  WHERE_IS_MY_LICENSE_NUMBER -> "ನನ್ನ ಪರವಾನಗಿ ಸಂಖ್ಯೆ ಎಲ್ಲಿದೆ?"
  TRIP_DETAILS -> "ಪ್ರವಾಸದ ವಿವರಗಳು"
  BY_CASH -> "ನಗದು ಮೂಲಕ"
  ONLINE_ ->  "ಆನ್ಲೈನ್"
  REPORT_AN_ISSUE -> "ಸಮಸ್ಯೆಯನ್ನು ವರದಿ ಮಾಡಿ"
  DISTANCE -> "ದೂರ"
  TIME_TAKEN -> "ತೆಗೆದುಕೊಂಡ ಸಮಯ"
  MAPS -> "Maps"
  CALL -> "ಕರೆ ಮಾಡಿ"
  START_RIDE -> "ರೈಡ್ ಪ್ರಾರಂಭಿಸಿ"
  CANCEL_RIDE -> "ರೈಡ್ ರದ್ದುಮಾಡಿ"
  END_RIDE -> "ಎಂಡ್ ರೈಡ್"
  RIDE_COMPLETED_WITH -> "ಗ್ರಾಹಕರೊಂದಿಗೆ ಸವಾರಿ ಪೂರ್ಣಗೊಂಡಿದೆ"
  COLLECT_AMOUNT_IN_CASH -> "ಮೊತ್ತವನ್ನು ನಗದು ರೂಪದಲ್ಲಿ ಸಂಗ್ರಹಿಸಿ"
  CASH_COLLECTED -> "ನಗದು ಸಂಗ್ರಹಿಸಲಾಗಿದೆ"
  OFFLINE -> "ಆಫ್‌ಲೈನ್"
  ACCEPT_FOR -> "ಇದಕ್ಕಾಗಿ ಸ್ವೀಕರಿಸಿ:"
  DECLINE -> "ನಿರಾಕರಿಸು"
  REQUEST -> "ವಿನಂತಿ"
  YOU_ARE_OFFLINE -> "ನೀವು ಆಫ್‌ಲೈನ್‌ನಲ್ಲಿರುವಿರಿ"
  YOU_ARE_CURRENTLY_BUSY_GO_ONLINE_TO_RECIEVE_TRIP_REQUESTS -> "ನೀವು ಪ್ರಸ್ತುತ ಕಾರ್ಯನಿರತರಾಗಿದ್ದೀರಿ. ಟ್ರಿಪ್ ವಿನಂತಿಗಳನ್ನು ಸ್ವೀಕರಿಸಲು ಆನ್‌ಲೈನ್‌ಗೆ ಹೋಗಿ"
  GOING_OFFLINE_WILL_NOT_GET_YOU_ANY_RIDE -> "ಆಫ್‌ಲೈನ್‌ಗೆ ಹೋಗುವುದರಿಂದ ನಿಮಗೆ ಯಾವುದೇ ಸವಾರಿ ಸಿಗುವುದಿಲ್ಲ"
  CANCEL -> "ರದ್ದುಮಾಡು"
  GO_OFFLINE -> "ಆಫ್‌ಲೈನ್‌ಗೆ ಹೋಗಿ"
  IS_WAITING_FOR_YOU -> "ಪಿಕಪ್‌ಗೆ ಹೋಗುತ್ತಿದ್ದೇನೆ"
  YOU_ARE_ON_A_RIDE -> "ಸವಾರಿ ಪ್ರಾರಂಭವಾಯಿತು..."
  PLEASE_ASK_THE_CUSTOMER_FOR_THE_OTP -> "ದಯವಿಟ್ಟು OTP ಗಾಗಿ ಗ್ರಾಹಕರನ್ನು ಕೇಳಿ"
  COMPLETED_ -> "ಪೂರ್ಣಗೊಂಡಿದೆ"
  CANCELLED_ -> "ರದ್ದುಗೊಳಿಸಲಾಗಿದೆ"
  WE_NEED_SOME_ACCESS -> "ನಮಗೆ ಸ್ವಲ್ಪ ಪ್ರವೇಶ ಬೇಕು!"
  ALLOW_ACCESS -> "ಪ್ರವೇಶವನ್ನು ಅನುಮತಿಸಿ"
  ENTER_RC_NUMBER -> "RC ಸಂಖ್ಯೆಯನ್ನು ನಮೂದಿಸಿ"
  WHERE_IS_MY_RC_NUMBER -> "ನನ್ನ RC ಸಂಖ್ಯೆ ಎಲ್ಲಿದೆ?"
  THANK_YOU_FOR_WRITING_TO_US -> "ನಮಗೆ ಬರೆದಿದ್ದಕ್ಕಾಗಿ ಧನ್ಯವಾದಗಳು!"
  RIDER -> "ಸವಾರ"
  TRIP_ID -> "ಪ್ರವಾಸದ ಐಡಿ"
  NEED_IT_TO_SHOW_YOU_INCOMING_RIDE_REQUEST -> "ಅಪ್ಲಿಕೇಶನ್ ಮುಚ್ಚಿದಾಗ ಒಳಬರುವ ಸವಾರಿ ವಿನಂತಿಯನ್ನು ನಿಮಗೆ ತೋರಿಸಲು ಇದು ಅಗತ್ಯವಿದೆ"
  NEED_IT_TO_DISABLE_BATTERY_OPTIMIZATION_FOR_THE_APP -> "ಅಪ್ಲಿಕೇಶನ್ ಮುಚ್ಚಿದಾಗ ನಿಮ್ಮನ್ನು ಆನ್‌ಲೈನ್‌ನಲ್ಲಿ ಇರಿಸಲು ಅಪ್ಲಿಕೇಶನ್‌ಗಾಗಿ ಬ್ಯಾಟರಿ ಆಪ್ಟಿಮೈಸೇಶನ್ ಅನ್ನು ನಿಷ್ಕ್ರಿಯಗೊಳಿಸಲು ಇದು ಅಗತ್ಯವಿದೆ"
  NEED_IT_TO_AUTOSTART_YOUR_APP -> "ಅಪ್ಲಿಕೇಶನ್ ಮುಚ್ಚಿದಾಗ ನಿಮ್ಮನ್ನು ಆನ್‌ಲೈನ್‌ನಲ್ಲಿ ಇರಿಸಲು ನಿಮ್ಮ ಅಪ್ಲಿಕೇಶನ್ ಅನ್ನು ಆಟೋಸ್ಟಾರ್ಟ್ ಅಗತ್ಯವಿದೆ"
  NEED_IT_TO_ENABLE_LOCATION -> "ಅಪ್ಲಿಕೇಶನ್ ಮುಚ್ಚಿದಾಗ ಅಥವಾ ಬಳಕೆಯಲ್ಲಿಲ್ಲದಿದ್ದರೂ ಸಹ ಚಾಲಕ ಪ್ರಸ್ತುತ ಸ್ಥಳವನ್ನು ಮೇಲ್ವಿಚಾರಣೆ ಮಾಡಲು ನಿಮ್ಮ ಸ್ಥಳವನ್ನು ಹಂಚಿಕೊಳ್ಳಲು ಸಕ್ರಿಯಗೊಳಿಸಲು ನಮ್ಮ ಯಾತ್ರಿ ಪಾಲುದಾರರು ಸ್ಥಳ ಡೇಟಾವನ್ನು ಸಂಗ್ರಹಿಸುತ್ತಾರೆ."
  OVERLAY_TO_DRAW_OVER_APPLICATIONS -> "ಅಪ್ಲಿಕೇಶನ್‌ಗಳನ್ನು ಸೆಳೆಯಲು ಒವರ್ಲೆ"
  BATTERY_OPTIMIZATIONS -> "ಬ್ಯಾಟರಿ ಆಪ್ಟಿಮೈಸೇಶನ್"
  AUTO_START_APPLICATION_IN_BACKGROUND -> "ಹಿನ್ನೆಲೆಯಲ್ಲಿ ಆಟೋಸ್ಟಾರ್ಟ್ ಅಪ್ಲಿಕೇಶನ್"
  LOCATION_ACCESS -> "ಸ್ಥಳ ಪ್ರವೇಶ"
  STEP -> "ಹಂತ"
  PAID -> "ಪಾವತಿಸಲಾಗಿದೆ"
  PLEASE_TELL_US_WHY_YOU_WANT_TO_CANCEL -> "ನೀವು ಏಕೆ ರದ್ದುಗೊಳಿಸಬೇಕೆಂದು ದಯವಿಟ್ಟು ನಮಗೆ ತಿಳಿಸಿ"
  MANDATORY -> "ಕಡ್ಡಾಯ"
  ENTERED_WRONG_OTP -> "ತಪ್ಪು ಒಟಿಪಿಗೆ ಪ್ರವೇಶಿಸಿದೆ"
  COPIED ->  "ನಕಲಿಸಿದ"
  BANK_NAME -> "ಬ್ಯಾಂಕ್ ಹೆಸರು"
  AADHAR_DETAILS -> "ಆಧಾರ್ ವಿವರಗಳು"
  AADHAR_NUMBER -> "ಆಧರ್ ಸಂಖ್ಯೆ"
  FRONT_SIDE_IMAGE -> "ಮುಂಭಾಗದ ಚಿತ್ರ"
  BACK_SIDE_IMAGE -> " ಬ್ಯಾಕ್ ಸೈಡ್ ಇಮೇಜ್"
  CALL_SUPPORT_CENTER -> "ಬೆಂಬಲ ಕೇಂದ್ರಕ್ಕೆ ಕರೆ ಮಾಡಿ"
  STILL_NOT_RESOLVED -> "ಇನ್ನೂ ಪರಿಹರಿಸಲಾಗಿಲ್ಲವೇ? ನಮಗೆ ಕರೆ ಮಾಡಿ"
  CASE_TWO -> "ಬಿ) ಬಹಿರಂಗಪಡಿಸದಿರುವ "
  NON_DISCLOUSER_AGREEMENT -> " ಒಪ್ಪಂದಕ್ಕೆ"
  DATA_COLLECTION_AUTHORITY -> "ಸಿ) ನಾನು ಈ ಮೂಲಕ ನನ್ನ ಮಾಹಿತಿಯನ್ನು ಸಂಗ್ರಹಿಸಲು Juspay ಗೆ ನೇಮಿಸುತ್ತೇನೆ ಮತ್ತು ಅಧಿಕಾರ ನೀಡುತ್ತೇನೆ ಮತ್ತು ಮುಂದುವರಿಸುವ ಮೂಲಕ, ನಾನು ಬಳಕೆಯ ನಿಯಮಗಳು ಮತ್ತು ಗೌಪ್ಯತೆ ನೀತಿಗೆ ಸಮ್ಮತಿಸುತ್ತೇನೆ"
  SOFTWARE_LICENSE -> "ಸಾಫ್ಟ್‌ವೇರ್ ಪರವಾನಗಿ"
  LOAD_MORE -> "ಇನ್ನಷ್ಟು ಲೋಡ್ ಮಾಡಿ"
  ARE_YOU_SURE_YOU_WANT_TO_LOGOUT -> "ನೀವು ಲಾಗ್‌ಔಟ್ ಮಾಡಲು ಬಯಸುತ್ತೀರಿ ಎಂಬುದು ಖಚಿತವೇ?"
  GO_BACK -> "ಹಿಂದೆ ಹೋಗು"
  ARE_YOU_SURE_YOU_WANT_TO_END_THE_RIDE -> "ನೀವು ರೈಡ್ ಅನ್ನು ಕೊನೆಗೊಳಿಸಲು ಬಯಸುತ್ತೀರಿ ಎಂದು ನೀವು ಖಚಿತವಾಗಿ ಬಯಸುವಿರಾ"
  THANK_YOU_FOR_REGISTERING_US -> "ನಮ್ಮೊಂದಿಗೆ ನೋಂದಾಯಿಸಿದ್ದಕ್ಕಾಗಿ ಧನ್ಯವಾದಗಳು!"
  UNFORTANUTELY_WE_ARE_NOT_AVAILABLE__YET_FOR_YOU -> "ದುರದೃಷ್ಟವಶಾತ್, ನಾವು ನಿಮಗಾಗಿ ಇನ್ನೂ ಲಭ್ಯವಿಲ್ಲ. ನಾವು ಶೀಘ್ರದಲ್ಲೇ ನಿಮಗೆ ತಿಳಿಸುತ್ತೇವೆ."
  EMPTY_RIDES -> "ಖಾಲಿ ಸವಾರಿಗಳು"
  YOU_HAVE_NOT_TAKEN_A_TRIP_YET -> "ನೀವು ಇನ್ನೂ ಪ್ರವಾಸ ಕೈಗೊಂಡಿಲ್ಲ"
  BOOK_NOW -> "ಈಗ ಪುಸ್ತಕ"
  RESEND_OTP_IN -> "OTP ಅನ್ನು ಮರುಕಳುಹಿಸಿ - "
  WE_NEED_ACCESS_TO_YOUR_LOCATION -> "ನಮಗೆ ನಿಮ್ಮ ಸ್ಥಳದ ಅನುಮತಿಯ ಅಗತ್ಯವಿದೆ"
  YOUR_LOCATION_HELPS_OUR_SYSTEM -> "ನಿಮ್ಮ ಸ್ಥಳದ ಅನುಮತಿಯು ಸಿಸ್ಟಂ ಅನ್ನು ಆಟೋಗಳ ಮೂಲಕ ಮ್ಯಾಪ್ ಮಾಡಲು ಸಹಾಯ ಮಾಡುತ್ತದೆ ಮತ್ತು ನಿಮಗೆ ಸಾಧ್ಯವಿರುವ ಸವಾರಿಯನ್ನು ಪಡೆಯಲು ಸಹಾಯ ಮಾಡುತ್ತದೆ."
  NO_INTERNET_CONNECTION -> "ಯಾವುದೇ ಇಂಟರ್ನೆಟ್ ಸಂಪರ್ಕವಿಲ್ಲ"
  PLEASE_CHECK_YOUR_INTERNET_CONNECTION_AND_TRY_AGAIN -> "ದಯವಿಟ್ಟು ನಿಮ್ಮ ಇಂಟರ್ನೆಟ್ ಸಂಪರ್ಕವನ್ನು ಪರಿಶೀಲಿಸಿ ಹಾಗೂ ಮತ್ತೆ ಪ್ರಯತ್ನಿಸಿ"
  TRY_AGAIN -> "ಮತ್ತೊಮ್ಮೆ ಪ್ರಯತ್ನಿಸಿ"
  GRANT_ACCESS -> "ಅನುಮತಿ ನೀಡಿ"
  OTP_LIMIT_EXCEED -> "OTP ಮಿತಿಯನ್ನು ಮೀರಿದೆ, ಸವಾರಿಯನ್ನು ರದ್ದುಮಾಡಿ ಮತ್ತು ಮತ್ತೆ ಪ್ರಯತ್ನಿಸಿ"
  ENTER_REFERRAL_MOBILE_NUMBER ->  "ರೆಫರಲ್ ಮೊಬೈಲ್ ಸಂಖ್ಯೆಯನ್ನು ನಮೂದಿಸಿ"
  APPLY -> "ಅನ್ವಯಿಸು"
  HAVE_A_REFERRAL -> "ಉಲ್ಲೇಖವಿದೆಯೇ?"
  ADD_HERE -> "ಇಲ್ಲಿ ಸೇರಿಸಿ"
  REFERRAL_APPLIED -> "ಉಲ್ಲೇಖವನ್ನು ಅನ್ವಯಿಸಲಾಗಿದೆ!"
  SMALLEDIT -> "ತಿದ್ದು"
  ADD_DRIVING_LICENSE -> "ಡಿಎಲ್ ಸೇರಿಸಿ"
  HELP -> "ಸಹಾಯ?"
  INVALID_DL_NUMBER -> "ಅಮಾನ್ಯ ಡಿಎಲ್ ಸಂಖ್ಯೆ"
  DRIVING_LICENSE_NUMBER -> "ಡಿಎಲ್ ಸಂಖ್ಯೆ"
  RE_ENTER_DRIVING_LICENSE_NUMBER -> "ಮತ್ತೆ ಡಿಎಲ್ ಸಂಖ್ಯೆ"
  ENTER_DL_NUMBER -> "ಡಿಎಲ್ ಸಂಖ್ಯೆಯನ್ನು ನಮೂದಿಸಿ"
  SELECT_DATE_OF_BIRTH -> "ಹುಟ್ಟಿದ ದಿನಾಂಕವನ್ನು ಆಯ್ಕೆಮಾಡಿ"
  DATE_OF_BIRTH -> "ಹುಟ್ಟಿದ ದಿನಾಂಕ"
  WATCH_A_TUTORIAL_FOR_EASY_REGISTRATION -> "ಸುಲಭ ನೋಂದಣಿಗಾಗಿ ಟ್ಯುಟೋರಿಯಲ್ ವೀಕ್ಷಿಸಿ"
  ENTER_MINIMUM_FIFTEEN_CHARACTERS -> "ಕನಿಷ್ಠ 15 ಅಕ್ಷರಗಳನ್ನು ನಮೂದಿಸಿ"
  ADD_YOUR_FRIEND -> "ನಿಮ್ಮ ಸ್ನೇಹಿತನನ್ನು ಸೇರಿಸಿ"
  PLEASE_WAIT_WHILE_VALIDATING_THE_IMAGE -> "ಚಿತ್ರವನ್ನು ಮೌಲ್ಯೀಕರಿಸುವಾಗ ದಯವಿಟ್ಟು ನಿರೀಕ್ಷಿಸಿ"
  VALIDATING -> "ಮೌಲ್ಯೀಕರಿಸುವುದು"
  VERIFICATION_PENDING -> "ಪರಿಶೀಲನೆ ಬಾಕಿಯಿದೆ"
  VERIFICATION_FAILED -> "ಪರಿಶೀಲನೆ ವಿಫಲವಾಗಿದೆ"
  NO_DOC_AVAILABLE -> "ಯಾವುದೇ ದಾಖಲೆ ಲಭ್ಯವಿಲ್ಲ"
  ISSUE_WITH_DL_IMAGE -> "ನಿಮ್ಮ DL ಚಿತ್ರದಲ್ಲಿ ಕೆಲವು ಸಮಸ್ಯೆ ಇರುವಂತಿದೆ, ನಮ್ಮ ಬೆಂಬಲ ತಂಡವು ಶೀಘ್ರದಲ್ಲೇ ನಿಮ್ಮನ್ನು ಸಂಪರ್ಕಿಸುತ್ತದೆ."
  STILL_HAVE_SOME_DOUBT -> "ಇನ್ನೂ ಸ್ವಲ್ಪ ಅನುಮಾನವಿದೆಯೇ?"
  ISSUE_WITH_RC_IMAGE -> "ನಿಮ್ಮ RC ಚಿತ್ರದಲ್ಲಿ ಕೆಲವು ಸಮಸ್ಯೆ ಇರುವಂತಿದೆ, ನಮ್ಮ ಬೆಂಬಲ ತಂಡವು ಶೀಘ್ರದಲ್ಲೇ ನಿಮ್ಮನ್ನು ಸಂಪರ್ಕಿಸುತ್ತದೆ."
  PLEASE_CHECK_FOR_IMAGE_IF_VALID_DOCUMENT_IMAGE_OR_NOT -> "ದಯವಿಟ್ಟು ಚಿತ್ರವು ಮಾನ್ಯವಾದ ಡಾಕ್ಯುಮೆಂಟ್ ಚಿತ್ರವೇ ಅಥವಾ ಇಲ್ಲವೇ ಎಂಬುದನ್ನು ಪರಿಶೀಲಿಸಿ"
  OOPS_YOUR_APPLICATION_HAS_BEEN_REJECTED -> "ಓಹ್! ನಿಮ್ಮ ಅರ್ಜಿಯನ್ನು ತಿರಸ್ಕರಿಸಲಾಗಿದೆ. ದಯವಿಟ್ಟು ಪುನಃ ಪ್ರಯತ್ನಿಸಿ"
  INVALID_DRIVING_LICENSE -> "ಅಮಾನ್ಯ DL"
  LIMIT_EXCEEDED_FOR_DL_UPLOAD -> "DL ಅಪ್‌ಲೋಡ್‌ಗೆ ಮಿತಿ ಮೀರಿದೆ"
  INVALID_VEHICLE_REGISTRATION_CERTIFICATE -> "ಅಮಾನ್ಯವಾದ ವಾಹನ ನೋಂದಣಿ ಪ್ರಮಾಣಪತ್ರ"
  LIMIT_EXCEEDED_FOR_RC_UPLOAD -> "ನೋಂದಣಿ ಪ್ರಮಾಣಪತ್ರ ಅಪ್‌ಲೋಡ್‌ಗೆ ಮಿತಿ ಮೀರಿದೆ"
  YOUR_DOCUMENTS_ARE_APPROVED -> "ನಿಮ್ಮ ದಾಖಲೆಗಳನ್ನು ಅನುಮೋದಿಸಲಾಗಿದೆ. ಬೆಂಬಲ ತಂಡವು ನಿಮ್ಮ ಖಾತೆಯನ್ನು ಶೀಘ್ರದಲ್ಲೇ ಸಕ್ರಿಯಗೊಳಿಸುತ್ತದೆ. ನಿಮ್ಮ ಖಾತೆಯನ್ನು ಸಕ್ರಿಯಗೊಳಿಸಲು ನೀವು ಬೆಂಬಲ ತಂಡಕ್ಕೆ ಕರೆ ಮಾಡಬಹುದು"
  APPLICATION_STATUS -> "ಅಪ್ಲಿಕೇಶನ್ ಸ್ಥಿತಿ"
  FOR_SUPPORT -> "ಬೆಂಬಲಕ್ಕಾಗಿ"
  CONTACT_US -> " ನಮ್ಮನ್ನು ಸಂಪರ್ಕಿಸಿ"
  IMAGE_VALIDATION_FAILED -> "ಚಿತ್ರದ ಮೌಲ್ಯಿಕರಣ ವಿಫಲವಾಗಿದೆ"
  IMAGE_NOT_READABLE -> "ಚಿತ್ರವನ್ನು ಓದಲು ಆಗುತ್ತಿಲ್ಲ"
  IMAGE_LOW_QUALITY -> "ಚಿತ್ರದ ಗುಣಮಟ್ಟ ಕಡಿಮೆ ಇದೆ"
  IMAGE_INVALID_TYPE -> "ಚಿತ್ರದ ಪ್ರಕಾರ ಅಮಾನ್ಯವಾಗಿದೆ"
  IMAGE_DOCUMENT_NUMBER_MISMATCH -> "ಚಿತ್ರದ ದಾಖಲೆ ಸಂಖ್ಯೆ ಹಾಗು ನಮೂದಿಸಿದ ದಾಖಲೆ ಸಂಖ್ಯೆ ಹೊಂದಿಕೆಯಾಗುತ್ತಿಲ್ಲ"
  IMAGE_EXTRACTION_FAILED -> "ಚಿತ್ರದ ಸಾರಾಂಶ ಓದಲು ವಿಫಲವಾಗಿದೆ"
  IMAGE_NOT_FOUND -> "ಚಿತ್ರ ಸಿಗಲಿಲ್ಲ"
  IMAGE_NOT_VALID -> "ಅಮಾನ್ಯ ಚಿತ್ರ"
  DRIVER_ALREADY_LINKED -> "ಚಾಲಕನನ್ನು ಈಗಾಗಲೇ ಲಿಂಕ್ ಮಾಡಲಾಗಿದೆ"
  DL_ALREADY_UPDATED -> "ಯಾವುದೇ ಕ್ರಮ ತೆಗೆದುಕೊಳ್ಳುವ ಅವಶ್ಯಕತೆ ಇಲ್ಲ. ಲೈಸೆನ್ಸ್ ಈಗಾಗಲೇ ಲಿಂಕ್ ಆಗಿದೆ"
  RC_ALREADY_LINKED -> "ಈ RC ಈಗಾಗಲೇ ಬೇರೆ ಚಾಲಕನಿಗೆ ಲಿಂಕ್ ಆಗಿದೆ"
  RC_ALREADY_UPDATED -> "ಯಾವುದೇ ಕ್ರಮ ತೆಗೆದುಕೊಳ್ಳುವ ಅವಶ್ಯಕತೆ ಇಲ್ಲ. RC ಈಗಾಗಲೇ ಲಿಂಕ್ ಆಗಿದೆ."
  DL_ALREADY_LINKED -> "ಈ DL ಈಗಾಗಲೇ ಬೇರೆ ಚಾಲಕನಿಗೆ ಲಿಂಕ್ ಆಗಿದೆ"
  SOMETHING_WENT_WRONG -> "ಏನೋ ತಪ್ಪಾಗಿದೆ"
  PICKUP -> "ಪಿಕಪ್"
  TRIP -> "ಪ್ರವಾಸ"
  CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER -> "ಪ್ರಸ್ತುತ, ನಾವು ಕರ್ನಾಟಕ ನೋಂದಾಯಿತ ಸಂಖ್ಯೆಯನ್ನು ಮಾತ್ರ ಅನುಮತಿಸುತ್ತೇವೆ"
  UPDATED_AT -> "ಕೊನೆಯ ಸ್ಥಳ ನವೀಕರಣ"
  DATE_OF_REGISTRATION -> "ನೋಂದಣಿ ದಿನಾಂಕ"
  DATE_OF_ISSUE ->  "ವಿತರಣೆಯ ದಿನಾಂಕ"
  SELECT_DATE_OF_ISSUE -> "ವಿತರಣೆಯ ದಿನಾಂಕವನ್ನು ಆಯ್ಕೆಮಾಡಿ"
  PROVIDE_DATE_OF_ISSUE_TEXT -> "ಕ್ಷಮಿಸಿ ನಿಮ್ಮ ವಿವರವನ್ನು ಮೌಲ್ಯೀಕರಿಸಲು ನಮಗೆ ಸಾಧ್ಯವಾಗಲಿಲ್ಲ, ದಯವಿಟ್ಟು ನಿಮ್ಮ DL ವಿವರವನ್ನು ಮೌಲ್ಯೀಕರಿಸಲು<b> ವಿತರಣೆಯ ದಿನಾಂಕ</b>ವನ್ನು ಒದಗಿಸಿ"
  PROVIDE_DATE_OF_REGISTRATION_TEXT -> "ಕ್ಷಮಿಸಿ ನಿಮ್ಮ ವಿವರವನ್ನು ಮೌಲ್ಯೀಕರಿಸಲು ನಮಗೆ ಸಾಧ್ಯವಾಗಲಿಲ್ಲ, ದಯವಿಟ್ಟು ನಿಮ್ಮ RC ವಿವರವನ್ನು ಮೌಲ್ಯೀಕರಿಸಲು<b> ನೋಂದಣಿ ದಿನಾಂಕ</b>ವನ್ನು ಒದಗಿಸಿ"
  SELECT_DATE_OF_REGISTRATION -> "ನೋಂದಣಿ ದಿನಾಂಕವನ್ನು ಆಯ್ಕೆಮಾಡಿ"
  TRIP_COUNT ->"ಇಂದಿನ ಪ್ರವಾಸಗಳು"
  TODAYS_EARNINGS -> "ಇಂದಿನ ಗಳಿಕೆ"
  SAME_REENTERED_RC_MESSAGE -> "ದಯವಿಟ್ಟು ಮರು-ನಮೂದಿಸಿದ ಆರ್‌ಸಿ ಸಂಖ್ಯೆಯು ಮೇಲೆ ಒದಗಿಸಿದ ಆರ್‌ಸಿ ಸಂಖ್ಯೆಯಂತೆಯೇ ಇದೆಯೇ ಎಂಬುದನ್ನು ಖಚಿತಪಡಿಸಿಕೊಳ್ಳಿ"
  SAME_REENTERED_DL_MESSAGE -> "ಮರು-ನಮೂದಿಸಿದ DL ಸಂಖ್ಯೆಯು ಮೇಲೆ ಒದಗಿಸಲಾದ DL ಸಂಖ್ಯೆಯೊಂದಿಗೆ ಹೊಂದಿಕೆಯಾಗುತ್ತಿಲ್ಲ"
  WHERE_IS_MY_ISSUE_DATE -> "ನನ್ನ ವಿತರಣೆಯ ದಿನಾಂಕ ಎಲ್ಲಿದೆ?"
  WHERE_IS_MY_REGISTRATION_DATE -> "ನೋಂದಣಿ ದಿನಾಂಕ ಎಲ್ಲಿದೆ?"
  OTP_RESENT -> "OTP ಮರು ಕಳುಹಿಸಲಾಗಿದೆ"
  EARNINGS_CREDITED_IN_ACCOUNT -> "ನಿಮ್ಮ ಗಳಿಕೆಯನ್ನು ಈ ಖಾತೆಯಲ್ಲಿ ಜಮಾ ಮಾಡಲಾಗುತ್ತದೆ"
  INVALID_PARAMETERS -> "ಅಮಾನ್ಯ ನಿಯತಾಂಕಗಳು"
  UNAUTHORIZED -> "ಅನಧಿಕೃತ"
  INVALID_TOKEN -> "ಅಮಾನ್ಯ ಟೋಕನ್"
  SOME_ERROR_OCCURED_IN_OFFERRIDE -> "ಆಫರ್ ರೈಡ್ ನಲ್ಲಿ ಕೆಲವು ದೋಷ ಸಂಭವಿಸಿದೆ"
  SELECT_VEHICLE_TYPE -> "ವಾಹನದ ಪ್ರಕಾರವನ್ನು ಆಯ್ಕೆಮಾಡಿ"
  RIDE -> "ಸವಾರಿ"
  NO_LOCATION_UPDATE -> "ಸ್ಥಳ ನವೀಕರಣವಿಲ್ಲ"
  GOT_IT_TELL_US_MORE -> "ಅರ್ಥವಾಯಿತು, ನಮಗೆ ಇನ್ನಷ್ಟು ಹೇಳಿ?"
  WRITE_A_COMMENT -> "ಅನಿಸಿಕೆಯನ್ನು ಬರೆಯಿರಿ"
  HOW_WAS_YOUR_RIDE_WITH -> "ಜೊತೆಗಿನ ನಿಮ್ಮ ಪ್ರಯಾಣ ಹೇಗಿತ್ತು?"
  RUDE_BEHAVIOUR -> "ಒರಟು ನಡವಳಿಕೆ"
  LONG_WAITING_TIME -> "ಹೆಚ್ಚು ಕಾಯುವ ಸಮಯ"
  DIDNT_COME_TO_PICUP_LOCATION -> "ಪಿಕಪ್ ಸ್ಥಳಕ್ಕೆ ಬಂದಿರಲಿಲ್ಲ"
  HELP_US_WITH_YOUR_REASON -> "ನಿಮ್ಮ ಕಾರಣದೊಂದಿಗೆ ನಮಗೆ ಸಹಾಯ ಮಾಡಿ"
  MAX_CHAR_LIMIT_REACHED -> "ಗರಿಷ್ಠ ಅಕ್ಷರ ಮಿತಿಯನ್ನು ತಲುಪಿದೆ,"
  SHOW_ALL_OPTIONS -> "ಎಲ್ಲಾ ಆಯ್ಕೆಗಳನ್ನು ತೋರಿಸಿ"
  UPDATE_REQUIRED -> "ಅಪ್ಡೇಟ್ ಅಗತ್ಯವಿದೆ"
  PLEASE_UPDATE_APP_TO_CONTINUE_SERVICE -> "ಸೇವೆಯನ್ನು ಮುಂದುವರಿಸಲು ದಯವಿಟ್ಟು ಅಪ್ಲಿಕೇಶನ್ ಅನ್ನು ನವೀಕರಿಸಿ"
  NOT_NOW -> "ಈಗಲ್ಲ"
  OF -> "ರಲ್ಲಿ"
  DROP -> "ಡ್ರಾಪ್"
  PLEASE_WAIT -> "ದಯಮಾಡಿ ನಿರೀಕ್ಷಿಸಿ"
  SETTING_YOU_OFFLINE -> "ನಾವು ನಿಮ್ಮ ಆಫ್‌ಲೈನ್ ಅನ್ನು ಹೊಂದಿಸುತ್ತಿದ್ದೇವೆ"
  SETTING_YOU_ONLINE -> "ನಾವು ನಿಮ್ಮನ್ನು ಆನ್‌ಲೈನ್‌ನಲ್ಲಿ ಹೊಂದಿಸುತ್ತಿದ್ದೇವೆ"
  VIEW_BREAKDOWN -> "ವಿಭಜನೆಯನ್ನು ವೀಕ್ಷಿಸಿ"
  APP_INFO -> "ಅಪ್ಲಿಕೇಶನ್ ಮಾಹಿತಿ"
  OTHER -> "ಬೇರೆ"
  VEHICLE_ISSUE -> "ವಾಹನ ಸಮಸ್ಯೆ.ವಾಹನ ಸಮಸ್ಯೆ"
  FARE_UPDATED -> "ದರವನ್ನು ನವೀಕರಿಸಲಾಗಿದೆ"
  FREQUENT_CANCELLATIONS_WILL_LEAD_TO_LESS_RIDES -> "ಪದೇ ಪದೇ ರದ್ದುಗೊಳಿಸುವುದರಿಂದ ಕಡಿಮೆ ರೈಡ್‌ಗಳು ಮತ್ತು ಕಡಿಮೆ ರೇಟಿಂಗ್‌ಗೆ ಕಾರಣವಾಗುತ್ತದೆ"
  CONTINUE -> "ಮುಂದುವರಿಸಿ"
  CONFIRM_PASSWORD -> "ಪಾಸ್ವರ್ಡ್ ದೃಢೀಕರಿಸಿ"
  DEMO_MODE -> "ಡೆಮೊ ಮೋಡ್"
  PASSWORD -> "ಪಾಸ್ವರ್ಡ್"
  ENTER_DEMO_MODE_PASSWORD -> "ಡೆಮೊ ಮೋಡ್ ಪಾಸ್ವರ್ಡ್ ನಮೂದಿಸಿ"
  DEMO_MODE_DISABLED -> "ಡೆಮೊ ಮೋಡ್ ನಿಷ್ಕ್ರಿಯಗೊಳಿಸಲಾಗಿದೆ"
  ONLINE_VIA_DEMO_MODE -> "ಆನ್‌ಲೈನ್ (ಡೆಮೊ)"
  MORE -> "more"
  LESS -> "less"
  YOU_ARE_AT_PICKUP -> "ನೀವು ಪಿಕಪ್ ಸ್ಥಳದಲ್ಲಿದ್ದೀರಿ"
  WAITING_FOR_CUSTOMER -> "ನೀವು ಗ್ರಾಹಕರಿಗಾಗಿ ಕಾಯುತ್ತಿದ್ದೀರಿ :"
  CUSTOMER_NOTIFIED -> "ಗ್ರಾಹಕರು ಸೂಚಿಸಿದ್ದಾರೆ"
  PICKUP_TOO_FAR -> "ಪಿಕಪ್ ತುಂಬಾ ದೂರದಲ್ಲಿದೆ"
  CUSTOMER_NOT_PICKING_CALL -> "ಗ್ರಾಹಕರು ಕರೆಯನ್ನು ತೆಗೆದುಕೊಳ್ಳುತ್ತಿಲ್ಲ"
  TRAFFIC_JAM -> "ಟ್ರಾಫಿಕ್ ಜಾಮ್"
  CUSTOMER_WAS_RUDE -> "ಗ್ರಾಹಕರು ಅಸಭ್ಯವಾಗಿ ವರ್ತಿಸಿದರು"
  ALERT -> "ಘೋಷಣೆ"
  ALL_ALERTS -> "ಎಲ್ಲಾ ಘೋಷಣೆ"
  ADD_A_COMMENT -> "ಕಾಮೆಂಟ್ ಸೇರಿಸಿ"
  POST_COMMENT -> "ಕಾಮೆಂಟ್ ಪೋಸ್ಟ್ ಮಾಡಿ"
  ENTER_YOUR_COMMENT -> "ನಿಮ್ಮ ಕಾಮೆಂಟ್ ಅನ್ನು ನಮೂದಿಸಿ"
  NO_NOTIFICATIONS_RIGHT_NOW -> "ಇದೀಗ ಯಾವುದೇ ಅಧಿಸೂಚನೆಗಳಿಲ್ಲ!"
  NO_NOTIFICATIONS_RIGHT_NOW_DESC -> "ಹೊಸ ಅಧಿಸೂಚನೆಗಳು ಬಂದಾಗ ನಾವು ನಿಮಗೆ ತಿಳಿಸುತ್ತೇವೆ"
  ALERTS -> "ಘೋಷಣೆಗಳು"
  YOUR_COMMENT -> "ನಿಮ್ಮ ಅನಿಸಿಕೆ"
  SHOW_MORE -> "ಇನ್ನು ಹೆಚ್ಚು ತೋರಿಸು"
  LOAD_OLDER_ALERTS -> "ಇನ್ನಷ್ಟು ಲೋಡ್ ಮಾಡಿ"
  CONTEST -> "ಸ್ಪರ್ಧೆ"
  YOUR_REFERRAL_CODE_IS_LINKED -> "ನಿಮ್ಮ ರೆಫರಲ್ ಕೋಡ್ ಲಿಂಕ್ ಆಗಿದೆ!"
  YOU_CAN_NOW_EARN_REWARDS -> "ಗ್ರಾಹಕರನ್ನು ಉಲ್ಲೇಖಿಸುವುದಕ್ಕಾಗಿ ನೀವು ಈಗ ಬಹುಮಾನಗಳನ್ನು ಗಳಿಸಬಹುದು!"
  COMING_SOON -> "ಶೀಘ್ರದಲ್ಲೇ ಬರಲಿದೆ!"
  COMING_SOON_DESCRIPTION -> "ನಿಮ್ಮನ್ನು ರೆಫರಲ್ ಪ್ರೋಗ್ರಾಂಗೆ ಸೇರಿಸಲು ನಾವು ಕೆಲಸ ಮಾಡುತ್ತಿದ್ದೇವೆ. ಹೆಚ್ಚಿನ ಮಾಹಿತಿಗಾಗಿ ಘೋಷಣೆಗಳು ಪುಟವನ್ನು ಪರಿಶೀಲಿಸಿ."
  REFERRAL_CODE -> "ರೆಫರಲ್ ಕೋಡ್"
  REFERRAL_CODE_HINT ->"6-ಅಂಕಿಯ ರೆಫರಲ್ ಕೋಡ್ ಅನ್ನು ನಮೂದಿಸಿ"
  CONFIRM_REFERRAL_CODE -> "ರೆಫರಲ್ ಕೋಡ್ ಅನ್ನು ದೃಢೀಕರಿಸಿ"
  CONFIRM_REFERRAL_CODE_HINT -> "ರೆಫರಲ್ ಕೋಡ್ ಅನ್ನು ಮರು-ನಮೂದಿಸಿ"
  YOUR_REFERRAL_CODE -> "ನಿಮ್ಮ ರೆಫರಲ್ ಕೋಡ್"
  FIRST_REFERRAL_SUCCESSFUL -> "ಮೊದಲ ರೆಫರಲ್ ಯಶಸ್ವಿಯಾಗಿದೆ!\nರಿವಾರ್ಡ್ ಅನ್‌ಲಾಕ್ ಮಾಡಲಾಗಿದೆ!"
  AWAITING_REFERRAL_RIDE -> "ರೆಫರಲ್ ರೈಡ್‌ಗಾಗಿ ಕಾಯಲಾಗುತ್ತಿದೆ"
  CHECK_THIS_SPACE_WHEN_YOU_GET_REFERRAL_ALERT -> "ನೀವು ರೆಫರಲ್ ಘೋಷಣೆಗಳು ಪಡೆದಾಗ \nಈ ಜಾಗವನ್ನು ಪರಿಶೀಲಿಸಿ"
  REFERRED_CUSTOMERS -> "ಉಲ್ಲೇಖಿಸಿದ ಗ್ರಾಹಕರು"
  ACTIVATED_CUSTOMERS -> "ಸಕ್ರಿಯ ಗ್ರಾಹಕರು"
  REFERRAL_CODE_LINKING -> "ರೆಫರಲ್ ಕೋಡ್ ಲಿಂಕ್ ಮಾಡುವುದು"
  CONTACT_SUPPORT -> "ಬೆಂಬಲವನ್ನು ಸಂಪರ್ಕಿಸಿ"
  CALL_SUPPORT -> "ಕರೆ ಬೆಂಬಲ"
  YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT -> "ನೀವು ನಮ್ಮ ಯಾತ್ರಿ ಬೆಂಬಲ ತಂಡಕ್ಕೆ ಕರೆ ಮಾಡಲಿರುವಿರಿ. ನೀವು ಮುಂದುವರೆಯಲು ಬಯಸುವಿರಾ?"
  REFERRAL_ENROLMENT -> "ರೆಫರಲ್ ದಾಖಲಾತಿ"    
  REFERRALS -> "ಉಲ್ಲೇಖಗಳು"
  LINK_REFERRAL_CODE -> "ಲಿಂಕ್ ರೆಫರಲ್ ಕೋಡ್"
  DRIVER_DETAILS -> "ಚಾಲಕ ವಿವರಗಳು"
  FOR_UPDATES_SEE_ALERTS -> "ನವೀಕರಣಗಳಿಗಾಗಿ ಘೋಷಣೆಗಳನ್ನು ನೋಡಿ"
  SHARE_OPTIONS -> "ಹಂಚಿಕೆ ಆಯ್ಕೆಗಳು"
  ENTER_PASSWORD -> "ಪಾಸ್ವರ್ಡ್ ನಮೂದಿಸಿ"
  RIDE_FARE -> "ರೈಡ್ ದರ"
  RIDE_DISTANCE -> "ಸವಾರಿ ದೂರ"
  START_YOUR_CHAT_USING_THESE_QUICK_CHAT_SUGGESTIONS -> "ಈ ತ್ವರಿತ ಚಾಟ್ ಸಲಹೆಗಳನ್ನು ಬಳಸಿಕೊಂಡು ನಿಮ್ಮ ಚಾಟ್ ಅನ್ನು ಪ್ರಾರಂಭಿಸಿ"
  MESSAGE -> "ಸಂದೇಶ"
  FARE_UPDATED -> "ದರವನ್ನು ನವೀಕರಿಸಲಾಗಿದೆ"
  START_YOUR_CHAT_WITH_THE_DRIVER -> "ಚಾಲಕನೊಂದಿಗೆ ನಿಮ್ಮ ಚಾಟ್ ಅನ್ನು ಪ್ರಾರಂಭಿಸಿ"
  I_AM_ON_MY_WAY -> "ನಾನು ಬರುತ್ತಿರುವೆ"
  GETTING_DELAYED_PLEASE_WAIT -> "ತಡವಾಗುತ್ತಿದೆ, ನಿರೀಕ್ಷಿಸಿ"
  UNREACHABLE_PLEASE_CALL_BACK -> "ನನಗೆ ಕರೆ ಮಾಡಿ"
  ARE_YOU_STARING -> "ನೀವು ಪ್ರಾರಂಭಿಸುತ್ತಿದ್ದೀರಾ?"
  PLEASE_COME_SOON -> "ದಯವಿಟ್ಟು ಬೇಗ ಬನ್ನಿ"
  OK_I_WILL_WAIT -> "ಸರಿ, ನಾನು ಕಾಯುತ್ತೇನೆ"
  I_HAVE_ARRIVED -> "ನಾನು ಆಗಮಿಸಿದೆ"
  PLEASE_COME_FAST_I_AM_WAITING -> "ಬೇಗ ಬನ್ನಿ, ನಾನು ಕಾಯುತ್ತಿದ್ದೇನೆ"
  PLEASE_WAIT_I_WILL_BE_THERE -> "ನಾನು ಬರುತ್ತಿದ್ದೇನೆ"
  LOOKING_FOR_YOU_AT_PICKUP -> "ನಾನು ಪಿಕಪ್‌ನಲ್ಲಿದ್ದೇನೆ"
  SILENT -> "ಸೈಲೆಂಟ್"
  TRY_SILENT_MODE -> "ಸೈಲೆಂಟ್ ಮೋಡ್ ಅನ್ನು ಪ್ರಯತ್ನಿಸಿ?"
  SILENT_MODE_PROMPT -> "ನೀವು ಅಡಚಣೆಯನ್ನು ಬಯಸದಿದ್ದರೆ, ಬದಲಿಗೆ ನೀವು ಸೈಲೆಂಟ್ ಮೋಡ್ ಅನ್ನು ಬಳಸಬಹುದು"
  GO_SILENT -> "ಸೈಲೆಂಟ್ ಸಕ್ರಿಯಗೊಳಿಸಿ"
  GO_ONLINE -> "ಗೋ!"
  GO_ONLINE_PROMPT -> "ನೀವು ಪ್ರಸ್ತುತ ಆಫ್‌ಲೈನ್‌ನಲ್ಲಿರುವಿರಿ. \n ಸವಾರಿ ವಿನಂತಿಗಳನ್ನು ಪಡೆಯಲು, ಈಗಲೇ ಆನ್‌ಲೈನ್‌ಗೆ ಹೋಗಿ!"
  LIVE_DASHBOARD -> "ಲೈವ್ ಸ್ಥಿತಿ ಡ್ಯಾಶ್‌ಬೋರ್ಡ್"
  CLICK_TO_ACCESS_YOUR_ACCOUNT -> "ನಿಮ್ಮ ಖಾತೆಯನ್ನು ಪ್ರವೇಶಿಸಲು ಇಲ್ಲಿ ಕ್ಲಿಕ್ ಮಾಡಿ"
  CUSTOMER_WAS_RUDE -> "ಗ್ರಾಹಕರು ಅಸಭ್ಯವಾಗಿ ವರ್ತಿಸಿದರು"
  ADD_ALTERNATE_NUMBER -> "ಪರ್ಯಾಯ ಸಂಖ್ಯೆಯನ್ನು ಸೇರಿಸಿ"
  ENTER_ALTERNATE_MOBILE_NUMBER -> "ಪರ್ಯಾಯ ಮೊಬೈಲ್ ಸಂಖ್ಯೆಯನ್ನು ನಮೂದಿಸಿ"
  PLEASE_ENTER_A_VALID_10_DIGIT_NUMBER -> "ದಯವಿಟ್ಟು ಮಾನ್ಯವಾದ 10-ಅಂಕಿಯ ಸಂಖ್ಯೆಯನ್ನು ನಮೂದಿಸಿ"
  ALTERNATE_MOBILE_NUMBER -> "ಪರ್ಯಾಯ ಮೊಬೈಲ್ ಸಂಖ್ಯೆ"
  REMOVE -> "ತೆಗೆದುಹಾಕಿ"
  REMOVE_ALTERNATE_NUMBER -> "ಪರ್ಯಾಯ ಸಂಖ್ಯೆಯನ್ನು ತೆಗೆದುಹಾಕಿ"
  ARE_YOU_SURE_YOU_WANT_TO_REMOVE_YOUR_ALTERNATE_MOBILE_NUMBER -> "ನಿಮ್ಮ ಪರ್ಯಾಯ ಮೊಬೈಲ್ ಸಂಖ್ಯೆಯನ್ನು ತೆಗೆದುಹಾಕಲು ನೀವು ಖಚಿತವಾಗಿ ಬಯಸುವಿರಾ?"
  YES_REMOVE_IT -> "ಹೌದು, ಅದನ್ನು ತೆಗೆದುಹಾಕಿ"
  NUMBER_REMOVED_SUCCESSFULLY -> "ಸಂಖ್ಯೆಯನ್ನು ಯಶಸ್ವಿಯಾಗಿ ತೆಗೆದುಹಾಕಲಾಗಿದೆ"
  EDIT_ALTERNATE_MOBILE_NUMBER -> "ಪರ್ಯಾಯ ಮೊಬೈಲ್ ಸಂಖ್ಯೆಯನ್ನು ಸಂಪಾದಿಸಿ"
  NUMBER_ADDED_SUCCESSFULLY -> "ಸಂಖ್ಯೆಯನ್ನು ಯಶಸ್ವಿಯಾಗಿ ಸೇರಿಸಲಾಗಿದೆ"
  NUMBER_EDITED_SUCCESSFULLY -> "ಸಂಖ್ಯೆಯನ್ನು ಯಶಸ್ವಿಯಾಗಿ ನವೀಕರಿಸಲಾಗಿದೆ"
  ALTERNATE_MOBILE_OTP_LIMIT_EXCEED -> "ಪರ್ಯಾಯ ಮೊಬೈಲ್ OTP ಮಿತಿ ಮೀರಿದೆ"
  WRONG_OTP -> "ದಯವಿಟ್ಟು ಮಾನ್ಯ OTP ಅನ್ನು ನಮೂದಿಸಿ "
  ATTEMPTS_LEFT -> " ಪ್ರಯತ್ನಗಳು ಉಳಿದಿವೆ"
  OTP_LIMIT_EXCEEDED -> "OTP ಮಿತಿ ಮೀರಿದೆ"
  OTP_LIMIT_EXCEEDED_MESSAGE -> "ನಿಮ್ಮ OTP ಮಿತಿಯನ್ನು ನೀವು ತಲುಪಿರುವಿರಿ. ದಯವಿಟ್ಟು 10 ನಿಮಿಷಗಳ ನಂತರ ಮತ್ತೆ ಪ್ರಯತ್ನಿಸಿ."
  TRY_AGAIN_LATER -> "ನಂತರ ಮತ್ತೆ ಪ್ರಯತ್ನಿಸಿ"
  ATTEMPT_LEFT -> " ಪ್ರಯತ್ನ ಉಳಿದಿದೆ"
  NUMBER_ALREADY_EXIST_ERROR -> "ಮತ್ತೊಂದು ಖಾತೆಗೆ ಸಂಖ್ಯೆಯನ್ನು ಲಿಂಕ್ ಮಾಡಲಾಗಿದೆ! ದಯವಿಟ್ಟು ಇನ್ನೊಂದು ಸಂಖ್ಯೆಯನ್ನು ಬಳಸಿ"
  OTP_RESEND_LIMIT_EXCEEDED -> "OTP ಮಿತಿಯನ್ನು ಮರುಕಳುಹಿಸಲಾಗಿದೆ"
  LIMIT_EXCEEDED_FOR_ALTERNATE_NUMBER -> "ಅನೇಕ ತಪ್ಪು ಪ್ರಯತ್ನಗಳು. ದಯವಿಟ್ಟು ನಂತರ ಮತ್ತೆ ಪ್ರಯತ್ನಿಸಿ"
  ALTERNATE_NUMBER_CANNOT_BE_ADDED -> "ಪರ್ಯಾಯ ಸಂಖ್ಯೆಯನ್ನು ಸೇರಿಸಲಾಗುವುದಿಲ್ಲ"
  ADD_ALTERNATE_NUMBER_IN_MEANTIME -> "ಈ ಪ್ರಕ್ರಿಯೆಯು ಪೂರ್ಣಗೊಳ್ಳಲು 2 ಕೆಲಸದ ದಿನಗಳವರೆಗೆ ತೆಗೆದುಕೊಳ್ಳಬಹುದು. ಈ ಮಧ್ಯೆ, ನೀವು \nಪರ್ಯಾಯ ಮೊಬೈಲ್ ಸಂಖ್ಯೆಯನ್ನು ಸೇರಿಸಬಹುದು."
  VERIFICATION_IS_TAKING_A_BIT_LONGER -> "ನಿಮ್ಮ ಪರಿಶೀಲನೆಯು \nನಿರೀಕ್ಷೆಗಿಂತ ಸ್ವಲ್ಪ ಹೆಚ್ಚು ಸಮಯ ತೆಗೆದುಕೊಳ್ಳುತ್ತಿರುವಂತೆ ತೋರುತ್ತಿದೆ.\nನಿಮಗೆ ಸಹಾಯ ಮಾಡಲು ನೀವು ಬೆಂಬಲವನ್ನು ಸಂಪರ್ಕಿಸಬಹುದು."
  COMPLETE_ONBOARDING -> "ಆನ್‌ಬೋರ್ಡಿಂಗ್ ಅನ್ನು ಪೂರ್ಣಗೊಳಿಸಿ"
  PERSON_WITH_THIS_NUMBER_ALREADY_EXISTS -> "ಈ ಮೊಬೈಲ್ ಸಂಖ್ಯೆಯನ್ನು ಹೊಂದಿರುವ ವ್ಯಕ್ತಿ ಈಗಾಗಲೇ ಅಸ್ತಿತ್ವದಲ್ಲಿದ್ದಾರೆ."
  DEMO -> "ಡೆಮೊ"
  COMPLAINTS_GRIEVANCES -> "ದೂರುಗಳು ಮತ್ತು ಕುಂದುಕೊರತೆಗಳು"
  COMPLAINTS_DESCRIPTION -> "ಯಾವುದೇ ದೂರುಗಳಿಗಾಗಿ, ದಯವಿಟ್ಟು ನಮ್ಮನ್ನು ಸಂಪರ್ಕಿಸಿ <u>nammayatri.support@juspay.in</u>;"
  COMPLAINTS_DESCRIPTION_ADDITIONAL -> "ಕುಂದುಕೊರತೆಗಳ ಪರಿಹಾರಕ್ಕಾಗಿ, ದಯವಿಟ್ಟು ನಮ್ಮ <u>ಗೌಪ್ಯತಾ ನೀತಿಯನ್ನು</u> ಉಲ್ಲೇಖಿಸಿ"
  REGISTERED_ADDRESS -> "ನೋಂದಾಯಿಸಿದ ವಿಳಾಸ"
  REGISTERED_ADDRESS_DESCRIPTION -> "Stallion Business Centre, No. 444, 3rd & 4th Floor, 18th Main, 6th Block, Koramangala, Bengaluru, Karnataka- 560095, India"
