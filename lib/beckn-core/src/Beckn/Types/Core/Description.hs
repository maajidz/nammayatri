module Beckn.Types.Core.Description
  ( Description (..),
  )
where

import Beckn.Utils.JSON
import qualified Beckn.Utils.Schema as Schema
import Data.OpenApi (ToSchema (declareNamedSchema), genericDeclareNamedSchema)
import EulerHS.Prelude
  ( FromJSON (..),
    Generic,
    Maybe,
    Show,
    Text,
    ToJSON (..),
    genericParseJSON,
    genericToJSON,
  )

data Description = Description
  { name :: Text,
    code :: Text,
    symbol :: Maybe Text,
    short_desc :: Maybe Text,
    long_desc :: Maybe Text,
    images :: [Text],
    audio :: Maybe Text,
    _3d_render :: Maybe Text
  }
  deriving (Show, Generic)

instance ToSchema Description where
  declareNamedSchema = genericDeclareNamedSchema Schema.stripPrefixUnderscoreIfAny

instance FromJSON Description where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Description where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny
