{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module External.Gateway.Flow where

import App.Types
import Beckn.Types.API.Cancel
import Beckn.Types.API.Confirm
import Beckn.Types.API.Search
import Beckn.Types.API.Status
import Beckn.Types.API.Track
import Beckn.Types.Common
import Beckn.Types.Core.Error
import Beckn.Types.Storage.Organization (Organization)
import Beckn.Utils.Common
import Beckn.Utils.Servant.Trail.Client (callAPIWithTrail)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified External.Gateway.Types as API
import Servant.Client
import Types.API.Location

search ::
  BaseUrl -> SearchReq -> Flow (Either Text ())
search url req = do
  apiKey <- xGatewayApiKey <$> ask
  mGatewaySelector <- xGatewaySelector <$> ask
  res <- case mGatewaySelector of
    Just "NSDL" -> do
      mNsdlUrl <- xGatewayNsdlUrl <$> ask
      case mNsdlUrl of
        Just nsdlBaseUrl -> do
          nsdlBapId <- nsdlUsername <$> ask
          nsdlBapPwd <- nsdlPassword <$> ask
          callAPIWithTrail nsdlBaseUrl (API.nsdlSearch nsdlBapId nsdlBapPwd req) "search"
        Nothing -> throwError500 "invalid nsdl gateway url"
    Just "JUSPAY" -> do
      callAPIWithTrail url (API.search (apiKey ?: "mobility-app-key") req) "search"
    _ -> throwError500 "gateway not configured"
  case res of
    Left err -> do
      L.logError @Text "Search" ("error occurred while search: " <> show err)
      return $ Left $ show err
    Right _ -> do
      L.logInfo @Text "Search" "Search successfully delivered"
      return $ Right ()

confirm :: BaseUrl -> Organization -> ConfirmReq -> Flow AckResponse
confirm url org req@ConfirmReq {context} = do
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  res <- callAPIWithTrail url (API.confirm cbApiKey req) "confirm"
  whenLeft res $ \err ->
    L.logError @Text "error occurred while confirm: " (show err)
  whenLeft res $ \err ->
    L.logError @Text "Confirm" ("error occurred while confirm: " <> show err)
  case res of
    Left err -> return $ AckResponse context (ack "ACK") $ Just (domainError (show err))
    Right _ -> return $ AckResponse context (ack "ACK") Nothing

location :: BaseUrl -> Text -> Flow (Either Text GetLocationRes)
location url req = do
  res <- callAPIWithTrail url (API.location req) "location"
  whenLeft res $ \err ->
    L.logError @Text "error occurred while confirm: " (show err)
  whenLeft res $ \err ->
    L.logError @Text "Location" ("error occurred while getting location: " <> show err)
  return $ first show res

track :: BaseUrl -> Organization -> TrackTripReq -> Flow AckResponse
track url org req@TrackTripReq {context} = do
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  res <- callAPIWithTrail url (API.trackTrip cbApiKey req) "track"
  case res of
    Left err -> L.logError @Text "error occurred while track trip: " (show err)
    Right _ -> L.logInfo @Text "Track" "Track successfully delivered"
  case res of
    Left err -> return $ AckResponse context (ack "ACK") $ Just (domainError (show err))
    Right _ -> return $ AckResponse context (ack "ACK") Nothing

cancel :: BaseUrl -> Organization -> CancelReq -> Flow (Either Text ())
cancel url org req = do
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  res <- callAPIWithTrail url (API.cancel cbApiKey req) "cancel"
  case res of
    Left err -> do
      L.logError @Text "error occurred while cancel trip: " (show err)
      return $ Left $ show err
    Right _ -> do
      L.logInfo @Text "Cancel" "Cancel successfully delivered"
      return $ Right ()

status :: BaseUrl -> Organization -> StatusReq -> Flow AckResponse
status url org req@StatusReq {context} = do
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  res <- callAPIWithTrail url (API.status cbApiKey req) "status"
  case res of
    Left err -> L.logError @Text "error occurred while getting status: " (show err)
    Right _ -> L.logInfo @Text "Status" "Status successfully delivered"
  case res of
    Left err -> return $ AckResponse context (ack "ACK") $ Just (domainError (show err))
    Right _ -> return $ AckResponse context (ack "ACK") Nothing
