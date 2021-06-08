{-# LANGUAGE OverloadedLabels #-}

module Types.API.Transporter where

import App.Types
import Beckn.TypeClass.Transform
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Location as SL
import qualified Beckn.Types.Storage.Organization as SO
import qualified Beckn.Types.Storage.Person as SP
import Beckn.Utils.JSON
import EulerHS.Prelude
import qualified Storage.Queries.Location as QL

data TransporterReq = TransporterReq
  { name :: Text,
    description :: Maybe Text,
    mobileNumber :: Text,
    mobileCountryCode :: Text,
    gstin :: Maybe Text,
    headCount :: Maybe Int,
    locationType :: Maybe SL.LocationType,
    lat :: Maybe Double,
    long :: Maybe Double,
    ward :: Maybe Text,
    district :: Text,
    city :: Text,
    state :: Maybe Text,
    country :: Text,
    pincode :: Maybe Text,
    address :: Maybe Text
  }
  deriving (Generic)

instance FromJSON TransporterReq where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance CreateTransform TransporterReq SO.Organization Flow where
  createTransform req = do
    oid <- generateGUID
    let shortId = ShortId $ getId oid
    now <- getCurrentTime
    location <- transformToLocation req
    QL.createFlow location
    return $
      SO.Organization
        { SO.id = oid,
          SO.name = req ^. #name,
          SO.shortId = shortId,
          SO.description = req ^. #description,
          SO.mobileNumber = Just $ req ^. #mobileNumber,
          SO.mobileCountryCode = Just $ req ^. #mobileCountryCode,
          SO.gstin = req ^. #gstin,
          SO.locationId = Just $ SL.id location,
          SO._type = SO.PROVIDER,
          SO.domain = Just SO.MOBILITY,
          SO.fromTime = Nothing,
          SO.toTime = Nothing,
          SO.headCount = req ^. #headCount,
          SO.apiKey = Nothing,
          SO.callbackUrl = Nothing,
          SO.status = SO.PENDING_VERIFICATION,
          SO.verified = False,
          SO.enabled = True,
          SO.createdAt = now,
          SO.updatedAt = now,
          SO.callbackApiKey = Nothing,
          SO.info = Nothing
        }

transformToLocation :: TransporterReq -> Flow SL.Location
transformToLocation req = do
  locId <- generateGUID
  now <- getCurrentTime
  return $
    SL.Location
      { SL.id = locId,
        SL.locationType = fromMaybe SL.PINCODE $ req ^. #locationType,
        SL.lat = req ^. #lat,
        SL.long = req ^. #long,
        SL.ward = req ^. #ward,
        SL.district = Just $ req ^. #district,
        SL.city = Just $ req ^. #city,
        SL.state = req ^. #state,
        SL.country = Just $ req ^. #country,
        SL.pincode = req ^. #pincode,
        SL.address = req ^. #address,
        SL.bound = Nothing,
        SL.point = SL.Point,
        SL.createdAt = now,
        SL.updatedAt = now
      }

data TransporterRes = TransporterRes
  { user :: SP.Person,
    organization :: SO.Organization
  }
  deriving (Generic, ToJSON)

newtype TransporterRec = TransporterRec
  { organization :: SO.Organization
  }
  deriving (Generic, ToJSON)

data UpdateTransporterReq = UpdateTransporterReq
  { name :: Maybe Text,
    description :: Maybe Text,
    headCount :: Maybe Int,
    enabled :: Maybe Bool
  }
  deriving (Generic, Show, FromJSON)

instance ModifyTransform UpdateTransporterReq SO.Organization Flow where
  modifyTransform req org = do
    now <- getCurrentTime
    return $
      org{SO.name = fromMaybe (org ^. #name) (req ^. #name),
          SO.description = (req ^. #description) <|> (org ^. #description),
          SO.headCount = (req ^. #headCount) <|> (org ^. #headCount),
          SO.enabled = fromMaybe (org ^. #enabled) (req ^. #enabled),
          SO.updatedAt = now
         }
