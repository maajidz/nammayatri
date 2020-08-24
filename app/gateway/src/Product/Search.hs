{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Product.Search
  ( search,
    searchCb,
  )
where

import App.Types
import Beckn.Types.Common (AckResponse (..), ack)
import Beckn.Types.Core.Error
import qualified Beckn.Types.Storage.Organization as Org
import Beckn.Utils.Common (fork, fromMaybeM400, fromMaybeM500, withFlowHandler)
import Beckn.Utils.Servant.Trail.Client (callAPIWithTrail, withClientTracing)
import Data.Aeson (encode)
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified Product.AppLookup as BA
import qualified Product.ProviderRegistry as BP
import Servant.Client (BaseUrl, parseBaseUrl)
import Types.API.Search (OnSearchReq, SearchReq, onSearchAPI, searchAPI)

parseOrgUrl :: Text -> Flow BaseUrl
parseOrgUrl =
  fromMaybeM400 "INVALID_TOKEN"
    . parseBaseUrl
    . toString

search :: Org.Organization -> SearchReq -> FlowHandler AckResponse
search org req = withFlowHandler $ do
  let search' = ET.client $ withClientTracing searchAPI
      messageId = req ^. #context . #_transaction_id
  appUrl <- Org._callbackUrl org & fromMaybeM400 "INVALID_ORG"
  providers <- BP.lookup $ req ^. #context
  BA.insert messageId appUrl
  forM_ providers $ \provider -> fork "Provider search" $ do
    providerUrl <- provider ^. #_callbackUrl & fromMaybeM500 "PROVIDER_URL_NOT_FOUND" -- Already checked for existance
    let providerApiKey = fromMaybe "" $ provider ^. #_callbackApiKey
    baseUrl <- parseOrgUrl providerUrl
    eRes <- callAPIWithTrail baseUrl (search' providerApiKey req) "search"
    L.logDebug @Text "gateway" $
      "request_transaction_id: " <> messageId
        <> ", search: req: "
        <> decodeUtf8 (encode req)
        <> ", resp: "
        <> show eRes
  if null providers
    then return $ AckResponse (req ^. #context) (ack "NACK") (Just $ domainError "No providers")
    else return $ AckResponse (req ^. #context) (ack "ACK") Nothing

searchCb :: Org.Organization -> OnSearchReq -> FlowHandler AckResponse
searchCb org req = withFlowHandler $ do
  let onSearch = ET.client $ withClientTracing onSearchAPI
      messageId = req ^. #context . #_transaction_id
  appUrl <- BA.lookup messageId >>= fromMaybeM400 "INVALID_MESSAGE"
  baseUrl <- parseOrgUrl appUrl
  cbApiKey <- org ^. #_callbackApiKey & fromMaybeM500 "CB_API_KEY_NOT_CONFIGURED"
  eRes <- callAPIWithTrail baseUrl (onSearch cbApiKey req) "on_search"
  let resp = case eRes of
        Left err -> AckResponse (req ^. #context) (ack "NACK") (Just $ domainError $ show err)
        Right _ -> AckResponse (req ^. #context) (ack "ACK") Nothing
  L.logDebug @Text "gateway" $
    "request_transaction_id: " <> messageId
      <> ", search_cb: req: "
      <> decodeUtf8 (encode req)
      <> ", resp: "
      <> show resp
  return resp
