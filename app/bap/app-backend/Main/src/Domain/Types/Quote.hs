{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.Quote where

import Data.OpenApi (ToSchema (..), genericDeclareNamedSchema)
import qualified Domain.Types.DriverOffer as DDriverOffer
import qualified Domain.Types.RentalSlab as DRentalSlab
import qualified Domain.Types.SearchRequest as DSearchRequest
import qualified Domain.Types.TripTerms as DTripTerms
import Domain.Types.VehicleVariant (VehicleVariant)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.GenericPretty
import qualified Tools.JSON as J
import qualified Tools.Schema as S

data Quote = Quote
  { id :: Id Quote,
    requestId :: Id DSearchRequest.SearchRequest,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    providerId :: Text,
    providerUrl :: BaseUrl,
    providerName :: Text,
    providerMobileNumber :: Text,
    providerCompletedRidesCount :: Int,
    vehicleVariant :: VehicleVariant,
    tripTerms :: Maybe DTripTerms.TripTerms,
    quoteDetails :: QuoteDetails,
    createdAt :: UTCTime
  }
  deriving (Generic, Show, PrettyShow)

data QuoteDetails
  = OneWayDetails OneWayQuoteDetails
  | RentalDetails DRentalSlab.RentalSlab
  | DriverOfferDetails DDriverOffer.DriverOffer
  deriving (Generic, Show)
  deriving (PrettyShow) via Showable QuoteDetails

newtype OneWayQuoteDetails = OneWayQuoteDetails
  { distanceToNearestDriver :: HighPrecMeters
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema, PrettyShow)

data QuoteAPIEntity = QuoteAPIEntity
  { id :: Id Quote,
    vehicleVariant :: VehicleVariant,
    estimatedFare :: Money,
    estimatedTotalFare :: Money,
    discount :: Maybe Money,
    agencyName :: Text,
    agencyNumber :: Text,
    agencyCompletedRidesCount :: Int,
    tripTerms :: [Text],
    quoteDetails :: QuoteAPIDetails,
    createdAt :: UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- do not change constructor names without changing fareProductConstructorModifier
data QuoteAPIDetails
  = OneWayAPIDetails OneWayQuoteAPIDetails
  | RentalAPIDetails DRentalSlab.RentalSlabAPIEntity
  | DriverOfferAPIDetails DDriverOffer.DriverOfferAPIEntity
  deriving (Show, Generic)

instance ToJSON QuoteAPIDetails where
  toJSON = genericToJSON J.fareProductOptions

instance FromJSON QuoteAPIDetails where
  parseJSON = genericParseJSON J.fareProductOptions

instance ToSchema QuoteAPIDetails where
  declareNamedSchema = genericDeclareNamedSchema S.fareProductSchemaOptions

newtype OneWayQuoteAPIDetails = OneWayQuoteAPIDetails
  { distanceToNearestDriver :: HighPrecMeters
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

mkQuoteAPIDetails :: QuoteDetails -> QuoteAPIDetails
mkQuoteAPIDetails = \case
  RentalDetails DRentalSlab.RentalSlab {..} -> RentalAPIDetails DRentalSlab.RentalSlabAPIEntity {..}
  OneWayDetails OneWayQuoteDetails {..} -> OneWayAPIDetails OneWayQuoteAPIDetails {..}
  DriverOfferDetails DDriverOffer.DriverOffer {..} -> DriverOfferAPIDetails DDriverOffer.DriverOfferAPIEntity {..}

makeQuoteAPIEntity :: Quote -> QuoteAPIEntity
makeQuoteAPIEntity Quote {..} = do
  QuoteAPIEntity
    { agencyName = providerName,
      agencyNumber = providerMobileNumber,
      agencyCompletedRidesCount = providerCompletedRidesCount,
      tripTerms = fromMaybe [] $ tripTerms <&> (.descriptions),
      quoteDetails = mkQuoteAPIDetails quoteDetails,
      ..
    }
