{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module EN where

import Language.Types (STR(..))

getEN ::STR -> String
getEN script = case script of
  LETS_GET_STARTED -> "Let's get started"
  YOUR_APPLICATION_HAS_BEEN_SUBMITTED_SUCCESSFULLY_AND_IS_UNDER_VERIFICATION -> "Your Application has been submitted successfully and is under verification"
  VIEW_STATUS -> "View Status"
  GO_HOME -> "Go Home"
  SELECT_LANGUAGE -> "Select Language"
  WHICH_LANGUAGE_DO_YOU_PREFER -> "Which language do you prefer?"
  NEXT -> "Next"
  T_C -> "Terms & Conditions"
  ENTER_MOBILE_NUMBER -> "Enter mobile number"
  BY_CLICKING_NEXT_YOU_WILL_BE_AGREEING_TO_OUR -> "By tapping next\na) You agree that you are willing participant of the beta testing and Juspay shall have no liability against you in any respect"
  ENTER_OTP -> "Enter OTP"
  DIDNT_RECIEVE_OTP -> "Didn't recieve OTP?"
  RESEND_OTP -> "Resend OTP"
  PLEASE_ENTER_VALID_OTP -> "Please Enter Valid OTP"
  INVALID_MOBILE_NUMBER -> "Invalid mobile number"
  REGISTER -> "Register"
  MOBILE_NUMBER -> "Mobile Number"
  AUTO_READING_OTP -> "Auto Reading OTP..."
  UPLOAD_DRIVING_LICENSE -> "Upload Driving License"
  UPLOAD_BACK_SIDE -> "Upload Back Side"
  UPLOAD_FRONT_SIDE -> "Upload Photo side of your DL"
  BACK_SIDE -> "Back Side"
  FRONT_SIDE -> "Photo side of your DL"
  LICENSE_INSTRUCTION_PICTURE -> "Kindly upload clear pictures of both sides of the license"
  LICENSE_INSTRUCTION_CLARITY -> "Ensure photo and all details are clearly visible"
  REGISTRATION_STEPS -> "Registration Steps"
  PROGRESS_SAVED -> "Your progress is saved, you can also go back to previous steps to change any information"
  DRIVING_LICENSE -> "Driving License"
  AADHAR_CARD -> "Aadhar Card"
  BANK_DETAILS -> "Bank Details"
  VEHICLE_DETAILS -> "Vehicle Details"
  UPLOAD_FRONT_BACK -> "Upload front and back side"
  EARNINGS_WILL_BE_CREDITED -> "Your earnings will be credit here"
  FILL_VEHICLE_DETAILS -> "Fill your vehicle details"
  FOLLOW_STEPS -> "Please follow below steps to register"
  REGISTRATION -> "Registration"
  UPLOAD_ADHAAR_CARD -> "Upload Aadhar Card"
  ADHAAR_INTRUCTION_PICTURE -> "Kindly upload clear pictures of both sides of the Aadhar Card"
  ADD_VEHICLE_DETAILS -> "Add Vehicle Details"
  VEHICLE_REGISTRATION_NUMBER -> "Vehicle Registration Number"
  RE_ENTER_VEHICLE_REGISTRATION_NUMBER -> "Re-enter Vehicle Registration Number"
  ENTER_VEHICLE_NO -> "Enter Vehicle No."
  VEHICLE_TYPE -> "Vehicle Type"
  VEHICLE_MODEL_NAME -> "Vehicle Model Name"
  ENTER_MODEL_NAME -> "Enter Model Name"
  VEHICLE_COLOUR -> "Vehicle Colour"
  ENTER_VEHICLE_COLOUR -> "Enter Vehicle Colour"
  UPLOAD_REGISTRATION_CERTIFICATE -> "Upload Registration Certificate (RC)"
  UPLOAD_RC -> "Upload RC"
  PREVIEW -> "Preview"
  CHOOSE_VEHICLE_TYPE -> "Choose Vehicle Type"
  BENIFICIARY_NUMBER -> "Beneficiary Account No"
  RE_ENTER_BENIFICIARY_NUMBER -> "Re-enter Beneficiary Account No."
  IFSC_CODE -> "IFSC Code"
  SENDING_OTP -> "Sending OTP"
  PLEASE_WAIT_WHILE_IN_PROGRESS -> "Please wait while in progress"
  LIMIT_EXCEEDED -> "Limit exceeded"
  YOUR_REQUEST_HAS_TIMEOUT_TRY_AGAIN -> "Your request has timeout try again"
  ERROR_OCCURED_PLEASE_TRY_AGAIN_LATER -> "Error occured please try again later"
  LIMIT_EXCEEDED_PLEASE_TRY_AGAIN_AFTER_10MIN -> "Limit exceeded Please try again later"
  ENTER_OTP_SENT_TO -> "Enter OTP sent to "
  OTP_SENT_TO -> "OTP Sent to "
  COUNTRY_CODE_INDIA -> "+91"
  ENTER_ACCOUNT_NUMBER -> "Enter Account No."
  ADD_BANK_DETAILS -> "Add Bank Details"
  ENTER_IFSC_CODE -> "Enter IFSC Code"
  SUBMIT -> "Submit"
  PERSONAL_DETAILS -> "Personal Details"
  LANGUAGES -> "Languages"
  HELP_AND_FAQ -> "Help & FAQs"
  ABOUT -> "About"
  LOGOUT -> "Logout"
  UPDATE -> "Update"
  EDIT -> "Edit"
  AUTO -> "Auto"
  NAME -> "Name"
  PRIVACY_POLICY -> "Privacy Policy"
  LOGO -> "Logo"
  ABOUT_APP_DESCRIPTION -> "Namma Yatri partner is an open platform to connect drivers with riders. The app makes it convenient for drivers to find riders with proposed desired rates. No ride based commission, just pay small amount in the form of monthly subscription"
  TERMS_AND_CONDITIONS -> "Terms & Conditions"
  UPDATE_VEHICLE_DETAILS -> "Update Vehicle Details"
  Help_AND_SUPPORT -> "Help And Support"
  NOTE -> "Note:"
  VISIT_MY_RIDES_SCREEN_FOR_SPECIFIC_COMPLAINTS -> "Visit My Rides Section for specific complaints"
  THANK_YOU_FOR_WRTITTING_US -> "Thank you for writing us!"
  WE_HAVE_RECIEVED_YOUR_ISSUE -> "We have recieved your issue. We'll reachout to you in sometime."
  GO_TO_HOME -> "Go To Home"
  YOUR_RECENT_RIDE -> "Your Recent Ride"
  ALL_TOPICS -> "All Topics"
  REPORT_AN_ISSUE_WITH_THIS_TRIP -> "Report an issue with this trip"
  YOU_RATED -> "You Rated:"
  VIEW_ALL_RIDES -> "View All Rides"
  WRITE_TO_US -> "Write To Us"
  SUBJECT -> "Subject"
  YOUR_EMAIL_ID -> "Your Email ID"
  DESCRIBE_YOUR_ISSUE -> "Describe Your issue"
  GETTING_STARTED_AND_FAQ -> "Getting started and FAQs"
  FOR_OTHER_ISSUES_WRITE_TO_US -> "For other issues, write to us"
  CALL_SUPPORT_CENTER -> "Call support center"
  YOU_CAN_DESCRIBE_ISSUE_THAT_YOU_FACED_HERE -> "You can describe issue that you faced here"
  REGISTRATION_CERTIFICATE_IMAGE -> "Registration Certificate (RC) Image"
  HOME -> "Home"
  RIDES -> "Rides"
  PROFILE -> "Profile"
  ENTER_DRIVING_LICENSE_NUMBER -> "Enter Driving License Number"
  WHERE_IS_MY_LICENSE_NUMBER -> "Where is my License Number?"
  TRIP_DETAILS -> "Trip Details"
  BY_CASH -> "by Cash"
  ONLINE_ ->  "Online"
  REPORT_AN_ISSUE -> "Report an issue"
  DISTANCE -> "Distance"
  TIME_TAKEN -> "Time Taken"
  MAPS -> "Maps"
  CALL -> "Call"
  START_RIDE -> "Start Ride"
  CANCEL_RIDE -> "Cancel Ride"
  PLEASE_TELL_US_WHY_YOU_WANT_TO_CANCEL -> "Please tell us why you want to cancel"
  MANDATORY -> "Mandatory"
  END_RIDE -> "End Ride"
  RIDE_COMPLETED_WITH -> "Ride Completed with customer"
  COLLECT_AMOUNT_IN_CASH -> "Collect the amount in cash"
  CASH_COLLECTED -> "Cash Collected"
  OFFLINE -> "Offline"
  ACCEPT_FOR -> "Accept for:"
  DECLINE -> "Decline"
  REQUEST -> "Request"
  YOU_ARE_OFFLINE -> "You're offline"
  YOU_ARE_CURRENTLY_BUSY_GO_ONLINE_TO_RECIEVE_TRIP_REQUESTS -> "You are currently Busy. Go online to recieve trip requests"
  GOING_OFFLINE_WILL_NOT_GET_YOU_ANY_RIDE -> "Going offline will not get you any ride"
  CANCEL -> "Cancel"
  GO_OFFLINE -> "Go Offline"
  IS_WAITING_FOR_YOU -> "is waiting for you"
  YOU_ARE_ON_A_RIDE -> "You are on a ride..."
  PLEASE_ASK_THE_CUSTOMER_FOR_THE_OTP -> "Please ask the customer for the OTP"
  COMPLETED_ -> "Completed"
  CANCELLED_ -> "Cancelled"
  WE_NEED_SOME_ACCESS -> "Grant us following access!"
  ALLOW_ACCESS -> "Allow Access"
  THANK_YOU_FOR_WRITING_TO_US -> "Thank you for Writing to us!"
  RIDER -> "Rider"
  TRIP_ID -> "Trip Id"
  NEED_IT_TO_SHOW_YOU_INCOMING_RIDE_REQUEST -> "Get incoming ride request while the app is in background"
  NEED_IT_TO_DISABLE_BATTERY_OPTIMIZATION_FOR_THE_APP -> "Recommended, enables the app to run in background for longer"
  NEED_IT_TO_AUTOSTART_YOUR_APP -> "Helps by keeping the app running in background"
  NEED_IT_TO_ENABLE_LOCATION -> "Namma Yatri Partner collect location data to enable share your location to monitor driver current location, even when the app is closed or not in use."
  OVERLAY_TO_DRAW_OVER_APPLICATIONS -> "Draw over applications"
  BATTERY_OPTIMIZATIONS -> "Battery Optimization"
  AUTO_START_APPLICATION_IN_BACKGROUND -> "Autostart app in background"
  LOCATION_ACCESS -> "Location Access"
  ENTER_RC_NUMBER -> "Enter RC  Number"
  WHERE_IS_MY_RC_NUMBER -> "Where is my RC Number?"
  STEP -> "Step"
  PAID -> "Paid"
  ENTERED_WRONG_OTP -> "Entered wrong OTP"
  COPIED -> "Copied"
  BANK_NAME -> "Bank Name"
  AADHAR_DETAILS -> "Aadhar Details"
  AADHAR_NUMBER -> "Aadhar Number"
  FRONT_SIDE_IMAGE -> "Front Side Image"
  BACK_SIDE_IMAGE -> "Back Side Image"
  STILL_NOT_RESOLVED -> "Still not resolved? Call Us"
  CASE_TWO -> "b) To the "
  NON_DISCLOUSER_AGREEMENT -> "no discloure agreement"
  DATA_COLLECTION_AUTHORITY -> "c) I hereby appoint and authorize Juspay to collect my information and by continuing, I agree to the Terms of Use and Privacy Policy."
  SOFTWARE_LICENSE -> "Software license"
  LOAD_MORE -> "Load More"
  ARE_YOU_SURE_YOU_WANT_TO_LOGOUT -> "Are you sure you want to logout?"
  GO_BACK -> "Go Back"
  THANK_YOU_FOR_REGISTERING_US -> "Thank You for register with us!"
  UNFORTANUTELY_WE_ARE_NOT_AVAILABLE__YET_FOR_YOU -> "Unfortunately, we are not available yet for you. We’ll notify you soon."
  ARE_YOU_SURE_YOU_WANT_TO_END_THE_RIDE -> "Are you sure you want to end the ride?"
  EMPTY_RIDES -> "Empty Rides"
  YOU_HAVE_NOT_TAKEN_A_TRIP_YET -> "You haven't taken a trip yet"
  BOOK_NOW -> "Book Now"
  RESEND_OTP_IN -> "Resend OTP in "
  WE_NEED_ACCESS_TO_YOUR_LOCATION -> "We need access to your location!"
  YOUR_LOCATION_HELPS_OUR_SYSTEM -> "Your location helps our system to map down all the near by autos and get you the quickest ride possible."
  NO_INTERNET_CONNECTION -> "No Internet Connection"
  PLEASE_CHECK_YOUR_INTERNET_CONNECTION_AND_TRY_AGAIN -> "Please check you internet connection and try again"
  TRY_AGAIN -> "Try Again"
  GRANT_ACCESS -> "Grant Access"
  OTP_LIMIT_EXCEED -> "OTP limit exceeded, cancel ride & try again"
  ENTER_REFERRAL_MOBILE_NUMBER ->  "Enter Referral Mobile Number"
  APPLY -> "Apply"
  HAVE_A_REFERRAL -> "Have a referral?"
  ADD_HERE -> "Add here"
  REFERRAL_APPLIED -> "Referral applied!"
  SMALLEDIT -> "edit"
  ADD_DRIVING_LICENSE -> "Add Driving License"
  HELP -> "Help?"
  INVALID_DL_NUMBER -> "Invalid DL Number"
  DRIVING_LICENSE_NUMBER -> "Driving Licence Number"
  RE_ENTER_DRIVING_LICENSE_NUMBER -> "Re-enter Driving Licence Number"
  ENTER_DL_NUMBER -> "Enter DL Number"
  SELECT_DATE_OF_BIRTH -> "Select date of birth"
  DATE_OF_BIRTH -> "Date of Birth"
  WATCH_A_TUTORIAL_FOR_EASY_REGISTRATION -> "Watch a tutorial for easy \nregistration"
  ENTER_MINIMUM_FIFTEEN_CHARACTERS -> "Enter minium 15 characters"
  ADD_YOUR_FRIEND -> "Add your friend"
  PLEASE_WAIT_WHILE_VALIDATING_THE_IMAGE -> "Please wait while validating the image"
  VALIDATING -> "Validating"
  VERIFICATION_PENDING -> "Verification Pending"
  VERIFICATION_FAILED -> "Verification Failed"
  NO_DOC_AVAILABLE -> "No document available"
  ISSUE_WITH_DL_IMAGE -> "There seems to be some issue with your DL image, Our support team will contact you soon."
  STILL_HAVE_SOME_DOUBT -> "Still have some doubt?"
  ISSUE_WITH_RC_IMAGE -> "There seems to be some issue with your RC image, Our support team will contact you soon."
  PLEASE_CHECK_FOR_IMAGE_IF_VALID_DOCUMENT_IMAGE_OR_NOT -> "Please check for image if valid Document image or not"
  OOPS_YOUR_APPLICATION_HAS_BEEN_REJECTED -> "Ooops! Your application has been rejected. Please try again"
  INVALID_DRIVING_LICENSE -> "Invalid Driving License"
  LIMIT_EXCEEDED_FOR_DL_UPLOAD -> "Limit exceed for DL upload"
  INVALID_VEHICLE_REGISTRATION_CERTIFICATE -> "Invalid Vehicle Registration Certificate"
  LIMIT_EXCEEDED_FOR_RC_UPLOAD -> "Limit exceed for RC upload"
  YOUR_DOCUMENTS_ARE_APPROVED -> "Your documents are approved. Support team will enable your account sooner. You can also call support team to enable your account"
  APPLICATION_STATUS -> "Application Status"
  FOR_SUPPORT -> "For Support"
  CONTACT_US -> " contact us"
  IMAGE_VALIDATION_FAILED -> "Validation of Image failed"
  IMAGE_NOT_READABLE -> "Image is not readable"
  IMAGE_LOW_QUALITY -> "Image quality is not good"
  IMAGE_INVALID_TYPE -> "Provided image type doesn't match with actual type"
  IMAGE_DOCUMENT_NUMBER_MISMATCH -> "Document number in this image is not matching with input"
  IMAGE_EXTRACTION_FAILED -> "Image extraction failed"
  IMAGE_NOT_FOUND -> "Image not found"
  IMAGE_NOT_VALID -> "Image not valid"
  DRIVER_ALREADY_LINKED -> "Other doc is already linked with driver"
  DL_ALREADY_UPDATED -> "No action required. Driver license is already linked to driver"
  RC_ALREADY_LINKED -> "Vehicle RC not available. Linked to other driver"
  RC_ALREADY_UPDATED -> "No action required. Vehicle RC is already linked to driver"
  DL_ALREADY_LINKED -> "Driver license not available. Linked to other driver"
  SOMETHING_WENT_WRONG -> "Something went wrong"
  PICKUP -> "Pickup"
  TRIP -> "Trip"
  CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER -> "Currently,We allow only Karnataka registered number"
  UPDATED_AT -> "Map updated at"
  TRIP_COUNT -> "Trip Count"
  SILENT_MODE_PROMPT -> "If you don't want to be disturbed, you may switch to silent mode instead"
  GO_SILENT -> "Go Silent"
  TRY_SILENT_MODE -> "Try silent mode?"
  TODAYS_EARNINGS -> "Earnings"
  DATE_OF_REGISTRATION -> "Date Of Registration"
  SELECT_DATE_OF_ISSUE -> "Select date of issue"
  DATE_OF_ISSUE -> "Date Of Issue"
  PROVIDE_DATE_OF_ISSUE_TEXT -> "Sorry we could't validate your detail, Please provide<b> Date of issue </b> to get your Driving License validated."
  PROVIDE_DATE_OF_REGISTRATION_TEXT -> "Sorry we could't validate your detail, Please provide<b> Date of Registration </b> to get your Vehicle details validated."
  SELECT_DATE_OF_REGISTRATION -> "Select date of registration"
  SAME_REENTERED_RC_MESSAGE -> "Please make sure re-entered RC number is same as RC number provided above"
  SAME_REENTERED_DL_MESSAGE -> "Re-entered DL number doesn't match with the DL number provided above"
  WHERE_IS_MY_ISSUE_DATE -> "Where is my Issue Date?"
  WHERE_IS_MY_REGISTRATION_DATE -> "Where is the Registration Date?"
  OTP_RESENT -> "OTP Resent"
  EARNINGS_CREDITED_IN_ACCOUNT -> "Your earnings will be credited in this account"
  INVALID_PARAMETERS -> "Invalid Parameters"
  UNAUTHORIZED -> "Unauthorized"
  INVALID_TOKEN -> "Invalid Token"
  SOME_ERROR_OCCURED_IN_OFFERRIDE -> "Some error occured in offerRide"
  SELECT_VEHICLE_TYPE -> "Select Vehicle Type"
  RIDE -> "Ride"
  NO_LOCATION_UPDATE -> "No location update"
  GOT_IT_TELL_US_MORE -> "Got it, Tell us more ?"
  WRITE_A_COMMENT -> "Write a comment"
  HOW_WAS_YOUR_RIDE_WITH -> "How was your ride with"
  RUDE_BEHAVIOUR -> "Rude Behaviour"
  LONG_WAITING_TIME -> "Longer Wait Time"
  DIDNT_COME_TO_PICUP_LOCATION -> "Didn’t come to pickup location"
  HELP_US_WITH_YOUR_REASON -> "Help us with your reason"
  MAX_CHAR_LIMIT_REACHED -> "Max character limit reached,"
  SHOW_ALL_OPTIONS -> "Show all options"
  UPDATE_REQUIRED -> "Update Required"
  PLEASE_UPDATE_APP_TO_CONTINUE_SERVICE -> "Please update app to continue service"
  NOT_NOW -> "Not now"
  OF -> "of"
  DROP -> "Drop"
  PLEASE_WAIT -> "Please wait"
  SETTING_YOU_OFFLINE -> "We are setting you offline"
  SETTING_YOU_ONLINE -> "We are setting you online"
  VIEW_BREAKDOWN -> "View Breakdown"
  APP_INFO -> "App Info"
  OTHER -> "Other"
  VEHICLE_ISSUE -> "Vehicle issue"
  FARE_UPDATED -> "Fare updated"
  FREQUENT_CANCELLATIONS_WILL_LEAD_TO_LESS_RIDES -> "Frequent cancellations will lead to less rides and lower rating"
  CONTINUE -> "Continue"
  CONFIRM_PASSWORD -> "Confirm Password"
  DEMO_MODE -> "Demo Mode"
  PASSWORD -> "Password"
  ENTER_DEMO_MODE_PASSWORD -> "Enter Demo Mode Password"
  DEMO_MODE_DISABLED -> "Demo mode disabled"
  ONLINE_VIA_DEMO_MODE -> "Online (Demo)"
  MORE -> "more"
  LESS -> "less"
  YOU_ARE_AT_PICKUP -> "You are at pickup location"
  WAITING_FOR_CUSTOMER -> "You are waiting for "
  CUSTOMER_NOTIFIED -> "Customer Notified"
  PICKUP_TOO_FAR -> "Pickup too far"
  CUSTOMER_NOT_PICKING_CALL -> "Customer not picking call"
  TRAFFIC_JAM -> "Traffic jam"
  CUSTOMER_WAS_RUDE -> "Customer was rude"
  ALERT ->  "Alert"
  ALL_ALERTS -> "All Alerts"
  ADD_A_COMMENT -> "Add a comment"
  POST_COMMENT -> "Post Comment"
  ENTER_YOUR_COMMENT -> "Enter your comment"
  NO_NOTIFICATIONS_RIGHT_NOW -> "No notifications right now!"
  NO_NOTIFICATIONS_RIGHT_NOW_DESC -> "We will let you know when there are new any notifications"
  ALERTS -> "Alerts"
  YOUR_COMMENT -> "Your comment"
  SHOW_MORE -> "Show More"
  LOAD_OLDER_ALERTS -> "Load Older Alerts"
  CONTEST -> "Contest"
  YOUR_REFERRAL_CODE_IS_LINKED -> "Your Referral Code is Linked!"
  YOU_CAN_NOW_EARN_REWARDS -> "You can now earn rewards for referring customers!"
  COMING_SOON -> "Coming Soon!"
  COMING_SOON_DESCRIPTION -> "We are working on getting you on board the referral program. Check out the alerts page for more info. "
  REFERRAL_CODE -> "Referral Code"
  REFERRAL_CODE_HINT ->"Enter 6-digit Referral Code"
  CONFIRM_REFERRAL_CODE -> "Confirm Referral Code"
  CONFIRM_REFERRAL_CODE_HINT -> "Re-enter Referral Code"
  YOUR_REFERRAL_CODE -> "Your Referral Code"
  FIRST_REFERRAL_SUCCESSFUL -> "First Referral Successful!\nReward Unlocked!"
  AWAITING_REFERRAL_RIDE -> "Awaiting referral ride"
  CHECK_THIS_SPACE_WHEN_YOU_GET_REFERRAL_ALERT -> "Check this space when you \nget a referral alert"
  REFERRED_CUSTOMERS -> "Referred Customers"
  ACTIVATED_CUSTOMERS -> "Activated Customers"
  REFERRAL_CODE_LINKING -> "Referral Code Linking"
  CONTACT_SUPPORT -> "Contact Support"
  CALL_SUPPORT -> "Call Support"
  YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT -> "You are about to place a call to the Namma Yatri Support Team. Do you want to proceed?"
  REFERRAL_ENROLMENT -> "Referral Enrolment"    
  REFERRALS -> "Referrals"
  LINK_REFERRAL_CODE -> "Link Referral Code"
  DRIVER_DETAILS -> "Driver Details"
  FOR_UPDATES_SEE_ALERTS -> "For updates, see Alerts"
  SHARE_OPTIONS -> "Share Options"
  ENTER_PASSWORD -> "Enter Password"
  RIDE_FARE -> "Ride Fare" 
  RIDE_DISTANCE -> "Ride Distance"
  FARE_UPDATED -> "Fare updated"
  START_YOUR_CHAT_USING_THESE_QUICK_CHAT_SUGGESTIONS -> "Start your chat using these quick chat suggestions"
  START_YOUR_CHAT_WITH_THE_DRIVER -> "Start your chat with the driver"
  MESSAGE -> "Message"
  I_AM_ON_MY_WAY -> "I'm on my way"
  GETTING_DELAYED_PLEASE_WAIT -> "Getting delayed, Please wait"
  UNREACHABLE_PLEASE_CALL_BACK -> "Unreachable, Please call back"
  ARE_YOU_STARING -> "Are you starting?"
  PLEASE_COME_SOON -> "Please come soon"
  OK_I_WILL_WAIT -> "Ok, I'll wait"
  I_HAVE_ARRIVED -> "I've arrived"
  PLEASE_COME_FAST_I_AM_WAITING -> "Please come fast, I'm waiting"
  PLEASE_WAIT_I_WILL_BE_THERE -> "Please wait, I'll be there"
  LOOKING_FOR_YOU_AT_PICKUP -> "Looking for you at pick-up"
  SILENT -> "Silent"
  GO_ONLINE -> "GO!"
  GO_ONLINE_PROMPT -> "You are currently offline.\nTo get ride requests, go online now!"
  LIVE_DASHBOARD -> "Live Stats Dashboard"
  CLICK_TO_ACCESS_YOUR_ACCOUNT -> "Click here to access your account"
  ADD_ALTERNATE_NUMBER -> "Add Alternate Number"
  ENTER_ALTERNATE_MOBILE_NUMBER -> "Enter Alternate Mobile Number"
  EDIT_ALTERNATE_MOBILE_NUMBER -> "Edit Alternate Mobile Number"
  PLEASE_ENTER_A_VALID_10_DIGIT_NUMBER -> "Please enter a valid 10-digit number"
  ALTERNATE_MOBILE_NUMBER -> "Alternate Mobile Number"
  REMOVE -> "Remove"
  REMOVE_ALTERNATE_NUMBER -> "Remove Alternate Number"
  ARE_YOU_SURE_YOU_WANT_TO_REMOVE_YOUR_ALTERNATE_MOBILE_NUMBER -> "Are you sure you want to remove your alternate mobile number?"
  YES_REMOVE_IT -> "Yes, Remove It"
  NUMBER_REMOVED_SUCCESSFULLY -> "Number Removed Successfully"
  NUMBER_ADDED_SUCCESSFULLY -> "Number Added Successfully"
  NUMBER_EDITED_SUCCESSFULLY -> "Number Updated Successfully"
  ALTERNATE_MOBILE_OTP_LIMIT_EXCEED -> "OTP Limit Exceeded , Enter Number And OTP Again"
  WRONG_OTP -> "Please Enter Valid OTP "
  ATTEMPTS_LEFT -> " attempts left"
  ATTEMPT_LEFT -> " attempt left"
  OTP_LIMIT_EXCEEDED -> "OTP Limit Exceeded"
  OTP_LIMIT_EXCEEDED_MESSAGE -> "You have reached your OTP limit. Please try again after 10 minutes."
  TRY_AGAIN_LATER -> "Try Again Later"
  NUMBER_ALREADY_EXIST_ERROR -> "Number linked to another account! Please use another number"
  OTP_RESEND_LIMIT_EXCEEDED -> "Resend OTP Limit Exceeded"
  LIMIT_EXCEEDED_FOR_ALTERNATE_NUMBER -> "Please try again after sometime"
  ALTERNATE_NUMBER_CANNOT_BE_ADDED -> "Alternate Number Cannot Be Added"
  ADD_ALTERNATE_NUMBER_IN_MEANTIME -> "This process can take upto 2 working days \nto be completed. In the meantime,  you \ncan add an alternate mobile number."
  VERIFICATION_IS_TAKING_A_BIT_LONGER -> "Looks like your verification is taking a bit \nlonger than expected.\nYou can contact support to help you out."
  COMPLETE_ONBOARDING -> "Complete Onboarding"
  PERSON_WITH_THIS_NUMBER_ALREADY_EXISTS -> "Person with this mobile number already exists."
  DEMO -> "DEMO"
  COMPLAINTS_GRIEVANCES -> "Complaints & Grievances"
  COMPLAINTS_DESCRIPTION -> "For any complaints, please reach out to us at <u>nammayatri.support@juspay.in</u>;"
  COMPLAINTS_DESCRIPTION_ADDITIONAL -> "For Redressal of grievances, please refer to our <u>Privacy Policy</u>"
  REGISTERED_ADDRESS -> "Registered Address"
  REGISTERED_ADDRESS_DESCRIPTION -> "Stallion Business Centre, No. 444, 3rd & 4th Floor, 18th Main, 6th Block, Koramangala, Bengaluru, Karnataka- 560095, India"