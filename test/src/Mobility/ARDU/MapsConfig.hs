{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Mobility.ARDU.MapsConfig where

import qualified Beckn.External.Maps as Maps
import Beckn.Prelude
import Beckn.Types.Id
import "driver-offer-bpp" Domain.Types.Merchant
import "driver-offer-bpp" Domain.Types.Merchant.MerchantServiceConfig
import qualified Mobility.ARDU.Fixtures as Fixtures
import qualified "driver-offer-bpp" Storage.CachedQueries.Merchant.MerchantServiceConfig as QOMSC
import Test.Hspec
import "driver-offer-bpp" Tools.Maps
import Utils

spec :: Spec
spec = describe "Merchant maps configs" $ do
  it
    "Fetch google config"
    fetchGoogleConfig
  it
    "Fetch OSRM config"
    fetchOSRMConfig

fetchConfig :: forall b. (Show b, Eq b) => Id Merchant -> Maps.MapsService -> (ServiceConfig -> b) -> b -> IO ()
fetchConfig merchantId serviceProvider getterFunc resultExpected = do
  Just cfg <-
    runARDUFlow "" $
      QOMSC.findByMerchantIdAndService merchantId (MapsService serviceProvider)
  getterFunc cfg.serviceConfig `shouldBe` resultExpected

fetchGoogleConfig :: IO ()
fetchGoogleConfig = do
  fetchConfig Fixtures.nammaYatriPartnerMerchantId Google func (fromJust $ parseBaseUrl "https://maps.googleapis.com/maps/api/")
  where
    func (MapsServiceConfig (GoogleConfig cfg)) = cfg.googleMapsUrl

fetchOSRMConfig :: IO ()
fetchOSRMConfig = do
  fetchConfig Fixtures.nammaYatriPartnerMerchantId OSRM func (fromJust $ parseBaseUrl "localhost:5000")
  where
    func (MapsServiceConfig (OSRMConfig cfg)) = cfg.osrmUrl
