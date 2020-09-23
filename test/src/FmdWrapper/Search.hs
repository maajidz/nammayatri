{-# LANGUAGE OverloadedLabels #-}

module FmdWrapper.Search where

import Beckn.Types.API.Callback
import Beckn.Types.Core.Context
import qualified Beckn.Types.Core.Domain as Domain
import qualified Beckn.Types.Core.Error as Error
import Beckn.Types.Core.Item
import qualified Beckn.Types.Core.Location as Location
import Beckn.Types.Core.Price
import qualified Beckn.Types.FMD.API.Search as Search
import Beckn.Types.FMD.Catalog
import Beckn.Utils.Common
import Common
import Control.Lens (Setter', _Just)
import Control.Lens.At
import EulerHS.Prelude
import External.Dunzo.Types
import Fmd
import FmdWrapper.Common
import FmdWrapper.Server
import qualified Network.HTTP.Client as Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client
import Test.Hspec hiding (context, example)
import Utils

setPickupGps :: Setter' Search.SearchReq Location.GPS
setPickupGps = #message . #intent . #_pickups . ix 0 . #_location . #_gps . _Just

setDropGps :: Setter' Search.SearchReq Location.GPS
setDropGps = #message . #intent . #_drops . ix 0 . #_location . #_gps . _Just

gps1 :: Location.GPS
gps1 = Location.GPS "12.9729391" "77.6294794"

gps2 :: Location.GPS
gps2 = Location.GPS "12.9354504" "77.6146828"

gps3 :: Location.GPS
gps3 = Location.GPS "13.0827" "80.2707"

numberOfDunzoCategores :: Int
numberOfDunzoCategores = 6

verifyCallbackContext :: Bool -> Text -> Context -> IO ()
verifyCallbackContext expectBppUri transactionId context = do
  _country context `shouldBe` Just "IND"
  _domain context `shouldBe` Domain.FINAL_MILE_DELIVERY
  when expectBppUri $ _bpp_uri context `shouldSatisfy` isJust
  _transaction_id context `shouldBe` transactionId
  _action context `shouldBe` "on_search"
  _message_id context `shouldBe` transactionId
  _bap_uri context `shouldSatisfy` isJust
  _domain_version context `shouldBe` Just "0.8.2"
  _core_version context `shouldBe` Just "0.8.0"

verifyDunzoCatalog :: Search.OnSearchServices -> IO ()
verifyDunzoCatalog onSearchServices = do
  let catalog = Search.catalog onSearchServices
  let items = _items catalog
  let packageCategories = _package_categories catalog
  map extractIndices items `shouldBe` expectedItemIndices
  map extractCategoryData packageCategories `shouldBe` expectedCategoryData
  forM_ items $ \item -> do
    let price = _price item
    _currency price `shouldBe` "INR"
    _estimated_value price `shouldSatisfy` isJust
  where
    extractCategoryData category = (category ^. #_id, category ^. #_descriptor . #_name)
    extractIndices item = (item ^. #_id, item ^. #_package_category_id)
    expectedItemIndices =
      take numberOfDunzoCategores $
        zip stringNumbers maybeStringNumbers
    expectedCategoryData = zip stringNumbers $ map (Just . content) dzPackageContentList
    stringNumbers = map show numbers
    maybeStringNumbers = map Just stringNumbers
    numbers = [1 ..] :: [Int]

verifyContexts :: Text -> [Search.OnSearchReq] -> IO ()
verifyContexts transactionId searchResults =
  forM_ searchResults \searchResult ->
    verifyCallbackContext True transactionId $ searchResult ^. #context

dunzoLocationError ::
  Location.GPS ->
  Location.GPS ->
  Error.Error ->
  ClientEnv ->
  CallbackData ->
  IO ()
dunzoLocationError pickupGps dropGps expectedError clientEnv callbackData =
  withNewUUID $ \transactionId -> do
    ctx <- buildContext "search" transactionId (Just fmdTestAppBaseUrl) Nothing

    let searchReq =
          buildFMDSearchReq ctx
            & setPickupGps .~ pickupGps
            & setDropGps .~ dropGps

    gatewayResponse <- runClient clientEnv $ client Search.searchAPI "fmd-test-app-key" searchReq
    assertAck gatewayResponse

    _ <- waitForCallback (onSearchEndMVar callbackData)
    callbackResults <- readTVarIO (onSearchTVar callbackData)
    let apiKeys = map apiKey callbackResults
    let searchResults = map result callbackResults

    traverse_ verifyApiKey apiKeys
    verifyContexts transactionId searchResults

    let errorResults = filter isLeft $ map contents searchResults
    case errorResults of
      [Left err] -> err `shouldBe` expectedError
      _ -> expectationFailure "Exactly one error result expected."

successfulSearch :: ClientEnv -> CallbackData -> IO ()
successfulSearch clientEnv callbackData =
  withNewUUID $ \transactionId -> do
    ctx <- buildContext "search" transactionId (Just fmdTestAppBaseUrl) Nothing

    let searchReq =
          buildFMDSearchReq ctx
            & setPickupGps .~ gps1
            & setDropGps .~ gps2

    gatewayResponse <- runClient clientEnv $ client Search.searchAPI "fmd-test-app-key" searchReq
    assertAck gatewayResponse

    searchEndResult <- waitForCallback (onSearchEndMVar callbackData)

    case searchEndResult of
      Just res -> do
        verifyApiKey $ apiKey res
        verifyCallbackContext False transactionId $ result res ^. #context
      Nothing -> expectationFailure "No search end callback received."

    callbackResults <- readTVarIO (onSearchTVar callbackData)
    let apiKeys = map apiKey callbackResults
    let searchResults = map result callbackResults

    traverse_ verifyApiKey apiKeys
    verifyContexts transactionId searchResults

    let dunzoResults = filter isDunzoResult searchResults

    case rights (map contents dunzoResults) of
      [message] ->
        verifyDunzoCatalog message
      _ -> expectationFailure "Expected one search result from Dunzo."
  where
    isDunzoResult result =
      _bpp_uri (context result) == Just fmdWrapperBaseUrl

dunzoUnserviceableLocation :: ClientEnv -> CallbackData -> IO ()
dunzoUnserviceableLocation =
  dunzoLocationError gps1 example $
    Error.Error "DOMAIN-ERROR" "FMD001" Nothing $
      Just "Location is not serviceable."

dunzoNearByLocation :: ClientEnv -> CallbackData -> IO ()
dunzoNearByLocation =
  dunzoLocationError gps1 gps1 $
    Error.Error "DOMAIN-ERROR" "FMD001" Nothing $
      Just "The pickup and drop addresses are near by."

dunzoDifferentCity :: ClientEnv -> CallbackData -> IO ()
dunzoDifferentCity =
  dunzoLocationError gps1 gps3 $
    Error.Error "DOMAIN-ERROR" "FMD001" Nothing $
      Just "Apologies, our services are limited to serviceable areas with in the city only."

spec :: Spec
spec = do
  around withCallbackApp $ do
    appManager <- runIO $ Client.newManager tlsManagerSettings
    let appClientEnv = mkClientEnv appManager gatewayBaseUrl
    describe "Search API" do
      it "Successful search" $ successfulSearch appClientEnv
      it "Dunzo: unserviceable location" $ dunzoUnserviceableLocation appClientEnv
      it "Dunzo: nearby location" $ dunzoNearByLocation appClientEnv
      it "Dunzo: different city" $ dunzoDifferentCity appClientEnv
