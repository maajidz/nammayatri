{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.DriverOnboarding.Error where

import Kernel.Prelude
import Kernel.Types.Error.BaseError.HTTPError

data DriverOnboardingError
  = ImageValidationExceedLimit Text
  | ImageValidationFailed
  | ImageNotReadable
  | ImageLowQuality
  | ImageInvalidType Text Text
  | ImageDocumentNumberMismatch Text Text
  | ImageExtractionFailed
  | ImageNotFound Text
  | ImageNotValid Text
  | DriverAlreadyLinked
  | DLAlreadyLinked
  | DLAlreadyUpdated
  | RCAlreadyLinked
  | RCAlreadyUpdated
  | RCLimitReached
  | InvalidOperatingCity Text
  | GenerateAadhaarOtpExceedLimit Text
  deriving (Generic, Eq, Show, Read, IsBecknAPIError, ToSchema, ToJSON, FromJSON)

instanceExceptionWithParent 'HTTPException ''DriverOnboardingError

instance IsBaseError DriverOnboardingError where
  toMessage = \case
    ImageValidationExceedLimit id_ -> Just $ "Number of validation try exceed for person \"" <> id_ <> "\"."
    ImageValidationFailed -> Just "Validation of Image failed."
    ImageNotReadable -> Just "Image is not readable."
    ImageLowQuality -> Just "Image quality is not good"
    ImageInvalidType provided actual -> Just $ "Provided image type \"" <> provided <> "\" doesn't match actual type \"" <> actual <> "\"."
    ImageDocumentNumberMismatch a b -> Just $ "Document number \"" <> a <> "\" in image is not matching with input \"" <> b <> "\"."
    ImageExtractionFailed -> Just "Image extraction failed"
    ImageNotFound id_ -> Just $ "Image with imageId \"" <> id_ <> "\" not found."
    ImageNotValid id_ -> Just $ "Image with imageId \"" <> id_ <> "\" is not valid."
    DriverAlreadyLinked -> Just "Other doc is already linked with driver."
    DLAlreadyLinked -> Just "Driver license not available."
    DLAlreadyUpdated -> Just "No action required. Driver license is already linked to driver."
    RCAlreadyLinked -> Just "Vehicle RC not available."
    RCAlreadyUpdated -> Just "No action required. Vehicle RC is already linked to driver."
    InvalidOperatingCity city -> Just $ "Operating city \"" <> city <> "\" is invalid."
    GenerateAadhaarOtpExceedLimit id_ -> Just $ "Generate Aadhaar otp  try limit exceeded for person \"" <> id_ <> "\"."
    RCLimitReached -> Just "Maximum Rc Limit Reached."

instance IsHTTPError DriverOnboardingError where
  toErrorCode = \case
    ImageValidationExceedLimit _ -> "IMAGE_VALIDATION_EXCEED_LIMIT"
    ImageValidationFailed -> "IMAGE_VALIDATION_FAILED"
    ImageNotReadable -> "IMAGE_NOT_READABLE"
    ImageLowQuality -> "IMAGE_LOW_QUALITY"
    ImageInvalidType _ _ -> "IMAGE_INVALID_TYPE"
    ImageDocumentNumberMismatch _ _ -> "IMAGE_DOCUMENT_NUMBER_MISMATCH"
    ImageExtractionFailed -> "IMAGE_EXTRACTION_FAILED"
    ImageNotFound _ -> "IMAGE_NOT_FOUND"
    ImageNotValid _ -> "IMAGE_NOT_VALID"
    DriverAlreadyLinked -> "DRIVER_ALREADY_LINKED"
    DLAlreadyLinked -> "DL_ALREADY_LINKED"
    DLAlreadyUpdated -> "DL_ALREADY_UPDATED"
    RCAlreadyLinked -> "RC_ALREADY_LINKED"
    RCAlreadyUpdated -> "RC_ALREADY_UPDATED"
    InvalidOperatingCity _ -> "OPERATING_CITY_INVALID"
    GenerateAadhaarOtpExceedLimit _ -> "GENERATE_AADHAAR_OTP_EXCEED_LIMIT"
    RCLimitReached -> "MAXIMUM_RC_LIMIT_REACHED"
  toHttpCode = \case
    ImageValidationExceedLimit _ -> E429
    ImageValidationFailed -> E400
    ImageNotReadable -> E400
    ImageLowQuality -> E400
    ImageInvalidType _ _ -> E400
    ImageDocumentNumberMismatch _ _ -> E400
    ImageExtractionFailed -> E400
    ImageNotFound _ -> E400
    ImageNotValid _ -> E400
    DriverAlreadyLinked -> E400
    DLAlreadyLinked -> E400
    DLAlreadyUpdated -> E400
    RCAlreadyLinked -> E400
    RCAlreadyUpdated -> E400
    InvalidOperatingCity _ -> E400
    GenerateAadhaarOtpExceedLimit _ -> E429
    RCLimitReached -> E400

instance IsAPIError DriverOnboardingError
