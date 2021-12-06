{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Quote where

import Beckn.Prelude
import Beckn.Types.Amount
import Beckn.Types.Id
import Domain.ParkingLocation
import Domain.Search

data Quote = Quote
  { id :: Id Quote,
    searchId :: Id Search,
    bppId :: Text,
    bppUrl :: BaseUrl,
    parkingSpaceName :: Text,
    parkingLocationId :: Id ParkingLocation,
    parkingLocationIdFromBpp :: Text,
    fare :: Amount,
    availableSpaces :: Int,
    createdAt :: UTCTime
  }
  deriving (Generic)

data QuoteAPIEntity = QuoteAPIEntity
  { id :: Id Quote,
    searchId :: Id Search,
    bppId :: Text,
    bppUrl :: BaseUrl,
    parkingSpaceName :: Text,
    parkingLocationId :: Id ParkingLocation,
    parkingLocationIdFromBpp :: Text,
    fare :: Amount,
    availableSpaces :: Int,
    createdAt :: UTCTime
  }
  deriving (Generic, ToJSON)

makeQuoteAPIEntity :: Quote -> QuoteAPIEntity
makeQuoteAPIEntity Quote {..} =
  QuoteAPIEntity {..}