{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Action.UI.Sos
  ( SosReq (..),
    SosRes (..),
    SosDetailsRes (..),
    SosUpdateReq (..),
    SOSVideoUploadReq (..),
    createSosDetails,
    updateSosDetails,
    markRideAsSafe,
    addSosVideo,
    getSosDetails,
  )
where

import AWS.S3 as S3
import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Message as Common
import qualified Data.ByteString as BS
import Data.Text as T
import Data.Time.Format.ISO8601 (iso8601Show)
import Domain.Action.UI.Profile (getDefaultEmergencyNumbers)
import Domain.Types.Booking.Type (BookingDetails (..))
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.Sos as DSos
import qualified Domain.Types.Sos as SosStatus
import Environment
import qualified EulerHS.Language as L
import EulerHS.Types (base64Encode)
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.External.Ticket.Interface.Types as Ticket
import Kernel.Prelude
import Kernel.ServantMultipart
import Kernel.Sms.Config
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.Sos as QSos
import qualified Storage.Queries.SosMedia as SQSM
import qualified Text.Read as Read
import Tools.Error
import Tools.SMS as Sms
import Tools.Ticket as Ticket

data SosReq = SosReq
  { flow :: DSos.SosType,
    rideId :: Id DRide.Ride
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data SOSVideoUploadReq = SOSVideoUploadReq
  { video :: FilePath,
    fileType :: Common.FileType,
    reqContentType :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance FromMultipart Tmp SOSVideoUploadReq where
  fromMultipart form = do
    SOSVideoUploadReq
      <$> fmap fdPayload (lookupFile "video" form)
      <*> (lookupInput "fileType" form >>= (Read.readEither . T.unpack))
      <*> fmap fdFileCType (lookupFile "video" form)

instance ToMultipart Tmp SOSVideoUploadReq where
  toMultipart sosVideoUploadReq =
    MultipartData
      [Input "fileType" (show sosVideoUploadReq.fileType)]
      [FileData "video" (T.pack sosVideoUploadReq.video) "" (sosVideoUploadReq.video)]

data SosUpdateReq = SosUpdateReq
  { status :: DSos.SosStatus,
    comment :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype SosRes = SosRes
  { sosId :: Id DSos.Sos
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

newtype SosDetailsRes = SosDetailsRes
  { sosId :: Maybe (Id DSos.Sos)
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

createSosDetails ::
  ( EsqDBReplicaFlow m r,
    EsqDBFlow m r,
    HasFlowEnv m r '["smsCfg" ::: SmsConfig],
    EncFlow m r,
    CacheFlow m r
  ) =>
  Id Person.Person ->
  Id Merchant.Merchant ->
  SosReq ->
  m SosRes
createSosDetails personId merchantId req = do
  smsCfg <- asks (.smsCfg)
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  mobileNumber <- mapM decrypt person.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
  countryCode <- person.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
  let phoneNumber = countryCode <> mobileNumber
  ride <- QRide.findById req.rideId >>= fromMaybeM (RideDoesNotExist req.rideId.getId)
  booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound req.rideId.getId)
  toLocation <- case booking.bookingDetails of
    OneWayDetails bDetails -> pure $ mkAddressEntity bDetails.toLocation.address
    RentalDetails _ -> pure ""
    DriverOfferDetails bDetails -> pure $ mkAddressEntity bDetails.toLocation.address
    OneWaySpecialZoneDetails bDetails -> pure $ mkAddressEntity bDetails.toLocation.address
  let rideInfo =
        Ticket.RideInfo
          { rideShortId = ride.shortId.getShortId,
            customerName = Just $ (fromMaybe "" person.firstName) <> " " <> (fromMaybe "" person.lastName),
            customerPhoneNo = Just $ countryCode <> mobileNumber,
            driverName = Just ride.driverName,
            driverPhoneNo = Just ride.driverMobileNumber,
            vehicleNo = ride.vehicleNumber,
            status = show booking.status,
            rideCreatedAt = ride.createdAt,
            pickupLocation = mkAddressEntity booking.fromLocation.address,
            dropLocation = Just toLocation,
            fare = Nothing
          }
      trackLink = "nammayatri.in/t/" <> ride.shortId.getShortId
  ticketRes <- createTicket person.merchantId (mkTicket person (Just phoneNumber) trackLink rideInfo)
  when (person.shareEmergencyContacts) $ do
    emergencyContacts <- getDefaultEmergencyNumbers (personId, merchantId)
    let sender = smsCfg.sender
    message <-
      MessageBuilder.buildSOSAlertMessage person.merchantId $
        MessageBuilder.BuildSOSAlertMessageReq
          { userName = (fromMaybe "" person.firstName) <> " " <> (fromMaybe "" person.lastName),
            rideLink = trackLink
          }
    for_ emergencyContacts.defaultEmergencyNumbers $ \emergencyContact -> do
      let emergencyPhoneNumber = emergencyContact.mobileCountryCode <> emergencyContact.mobileNumber
      Sms.sendSMS person.merchantId (Sms.SendSMSReq message emergencyPhoneNumber sender)
        >>= Sms.checkSmsResult
  sosDetails <- buildSosDetails personId req ticketRes.ticketId
  _ <- QSos.create sosDetails

  return $
    SosRes
      { sosId = sosDetails.id
      }
  where
    mkAddressEntity addr =
      let addField field label = case field of
            Just value -> T.concat [label, ": ", value, ", "]
            Nothing -> ""
       in T.concat
            [ addField addr.street "Street",
              addField addr.city "City",
              addField addr.state "State",
              addField addr.country "Country",
              addField addr.building "Building",
              addField addr.areaCode "Area Code",
              addField addr.area "Area"
            ]

getSosDetails :: (EsqDBFlow m r) => Id DRide.Ride -> Id Person.Person -> m SosDetailsRes
getSosDetails rideId_ personId_ = do
  sosDetails <- QSos.findByRideIdAndStatus rideId_ DSos.Resolved
  unless (personId_ == maybe "" (.personId) sosDetails) $ throwError $ InvalidRequest "PersonId not same"
  case sosDetails of
    Just DSos.Sos {..} ->
      return $
        SosDetailsRes
          { sosId = Just id
          }
    Nothing ->
      return $
        SosDetailsRes
          { sosId = Nothing
          }

updateSosDetails ::
  Id DSos.Sos -> (Id Person.Person, Id Merchant.Merchant) -> SosUpdateReq -> Flow APISuccess.APISuccess
updateSosDetails sosId (personId, merchantId) req = do
  sosDetails <- runInReplica $ QSos.findById sosId >>= fromMaybeM (SosIdDoesNotExist sosId.getId)
  unless (personId == sosDetails.personId) $ throwError $ InvalidRequest "PersonId not same"
  void $ QSos.updateStatus sosId (req.status)
  _ <- Ticket.updateTicket merchantId (Ticket.UpdateTicketReq req.comment sosDetails.ticketId (show req.status) "PS")
  pure APISuccess.Success

buildSosDetails :: (EncFlow m r) => Id Person.Person -> SosReq -> Text -> m DSos.Sos
buildSosDetails personId req ticketId = do
  pid <- generateGUID
  now <- getCurrentTime
  return
    DSos.Sos
      { id = pid,
        personId = personId,
        createdAt = now,
        updatedAt = now,
        status = DSos.Pending,
        flow = req.flow,
        rideId = req.rideId,
        ticketId = ticketId
      }

addSosVideo :: Id DSos.Sos -> Id Person.Person -> SOSVideoUploadReq -> Flow APISuccess.APISuccess
addSosVideo sosId personId SOSVideoUploadReq {..} = do
  sosDetails <- runInReplica $ QSos.findById sosId >>= fromMaybeM (SosIdDoesNotExist sosId.getId)
  person <- runInReplica $ QP.findById personId >>= fromMaybeM (PersonNotFound (getId personId))
  contentType <- validateContentType
  mediaFile <- L.runIO $ base64Encode <$> BS.readFile video
  filePath <- createFilePath (getId sosId) DSos.Video contentType
  fileConfig <- CQM.findById (person.merchantId) >>= fromMaybeM (InternalError "Error getting merchant configs")
  let fileUrl =
        fileConfig.mediaFileUrlPattern
          & T.replace "<DOMAIN>" "sos-video"
          & T.replace "<FILE_PATH>" filePath
  result <- try @_ @SomeException $ S3.put (T.unpack filePath) mediaFile
  case result of
    Left err -> throwError $ InternalError ("S3 Upload Failed: " <> show err)
    Right _ -> do
      _ <- Ticket.updateTicket person.merchantId (Ticket.UpdateTicketReq fileUrl sosDetails.ticketId "PENDING" "PS")
      createMediaEntry Common.AddLinkAsMedia {url = fileUrl, fileType}
  where
    validateContentType = do
      case fileType of
        Common.Video | reqContentType == "video/mp4" -> pure "mp4"
        _ -> throwError $ FileFormatNotSupported reqContentType

createFilePath ::
  Text ->
  DSos.MediaType ->
  Text ->
  Flow Text
createFilePath sosId fileType videoExtension = do
  pathPrefix <- asks (.s3Env.pathPrefix)
  now <- getCurrentTime
  let fileName = T.replace (T.singleton ':') (T.singleton '-') (T.pack $ iso8601Show now)
  return
    ( pathPrefix <> "/sos-video/" <> "sos-" <> sosId <> "/"
        <> show fileType
        <> "/"
        <> fileName
        <> videoExtension
    )

createMediaEntry :: Common.AddLinkAsMedia -> Flow APISuccess.APISuccess
createMediaEntry Common.AddLinkAsMedia {..} = do
  fileEntity <- mkFile url
  SQSM.create fileEntity
  return APISuccess.Success
  where
    mkFile fileUrl = do
      id <- generateGUID
      now <- getCurrentTime
      return $
        DSos.SosMedia
          { id,
            _type = DSos.Video,
            url = fileUrl,
            createdAt = now
          }

mkTicket :: Person.Person -> Maybe Text -> Text -> Ticket.RideInfo -> Ticket.CreateTicketReq
mkTicket person phoneNumber trackingUrl info = do
  Ticket.CreateTicketReq
    { category = "Code Red",
      subCategory = Just "SOS Alert (follow-back)",
      issueId = Nothing,
      issueDescription = "SOS called",
      mediaFiles = Just ["https://" <> trackingUrl],
      name = Just (fromMaybe "" person.firstName <> " " <> fromMaybe "" person.lastName),
      phoneNo = phoneNumber,
      personId = person.id.getId,
      classification = Ticket.CUSTOMER,
      rideDescription = Just info
    }

markRideAsSafe ::
  (Id Person.Person, Id Merchant.Merchant) ->
  Id DSos.Sos ->
  Flow APISuccess.APISuccess
markRideAsSafe (personId, merchantId) sosId = do
  person <- QP.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  sosDetails <- runInReplica $ QSos.findById sosId >>= fromMaybeM (SosIdDoesNotExist sosId.getId)
  smsCfg <- asks (.smsCfg)
  _ <- Ticket.updateTicket merchantId (Ticket.UpdateTicketReq "Mark Ride as Safe" sosDetails.ticketId "PENDING" "PS")
  void $ QSos.updateStatus sosId SosStatus.Resolved
  emergencyContacts <- getDefaultEmergencyNumbers (personId, merchantId)
  let sender = smsCfg.sender
  withLogTag ("perosnId" <> getId personId) $ do
    message <-
      MessageBuilder.buildMarkRideAsSafeMessage merchantId $
        MessageBuilder.BuildMarkRideAsSafeMessageReq
          { userName = (fromMaybe "" person.firstName) <> " " <> (fromMaybe "" person.lastName)
          }
    for_ emergencyContacts.defaultEmergencyNumbers $ \emergencyContact -> do
      let phoneNumber = emergencyContact.mobileCountryCode <> emergencyContact.mobileNumber
      Sms.sendSMS merchantId (Sms.SendSMSReq message phoneNumber sender)
        >>= Sms.checkSmsResult
  pure APISuccess.Success
