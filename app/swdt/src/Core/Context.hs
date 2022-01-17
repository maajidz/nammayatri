module Core.Context where

import Beckn.Prelude
import Beckn.Types.Core.Migration.Domain (Domain)
import Beckn.Types.Time (Iso8601Time)
import Beckn.Utils.Example
import Beckn.Utils.JSON
import Data.Aeson
import Data.Maybe (fromJust)
import Data.OpenApi (ToSchema)
import Servant.Client.Core

data Context = Context
  { domain :: Domain,
    country :: Text,
    city :: Text,
    action :: Action,
    core_version :: Text,
    bap_id :: Text,
    bap_uri :: BaseUrl,
    bpp_id :: Maybe Text,
    bpp_uri :: Maybe BaseUrl,
    transaction_id :: Text,
    message_id :: Text,
    timestamp :: UTCTime,
    key :: Maybe Text,
    ttl :: Maybe Iso8601Time
  }
  deriving (Generic, FromJSON, Show)

-- instance ToSchema Servant.BaseUrl where
--   declareNamedSchema _ = do
-- aSchema <- declareSchema (Proxy :: Proxy Text)
-- return $
--   NamedSchema (Just "BaseUrl") aSchema
--

instance ToJSON Context where
  toJSON = genericToJSON $ defaultOptions {omitNothingFields = True}

instance Example Context where
  example =
    Context
      { domain = example,
        action = example,
        core_version = "0.9.3",
        bap_id = "API.DOMAIN",
        bap_uri = fromJust $ parseBaseUrl "https://api.domain.com/",
        bpp_id = Just "API.DOMAIN",
        bpp_uri = parseBaseUrl "https://api.domain.com/",
        transaction_id = idExample,
        message_id = idExample,
        timestamp = example,
        country = "IND",
        city = "Kochi",
        ttl = Nothing,
        key = Nothing
      }

example_context :: Context
example_context = example

data Action
  = SEARCH
  | SELECT
  | INIT
  | CONFIRM
  | UPDATE
  | STATUS
  | TRACK
  | CANCEL
  | RATING
  | SUPPORT
  | ON_SEARCH
  | ON_SELECT
  | ON_INIT
  | ON_CONFIRM
  | ON_UPDATE
  | ON_STATUS
  | ON_TRACK
  | ON_CANCEL
  | ON_RATING
  | ON_SUPPORT
  deriving (Generic, Show, Eq, ToSchema)

instance FromJSON Action where
  parseJSON = genericParseJSON constructorsToLowerOptions

instance ToJSON Action where
  toJSON = genericToJSON constructorsToLowerOptions

instance Example Action where
  example = SEARCH

mapToCbAction :: Action -> Maybe Action
mapToCbAction = \case
  SEARCH -> Just ON_SEARCH
  SELECT -> Just ON_SELECT
  INIT -> Just ON_INIT
  CONFIRM -> Just ON_CONFIRM
  UPDATE -> Just ON_UPDATE
  STATUS -> Just ON_STATUS
  TRACK -> Just ON_TRACK
  CANCEL -> Just ON_CANCEL
  RATING -> Just ON_RATING
  SUPPORT -> Just ON_SUPPORT
  _ -> Nothing
