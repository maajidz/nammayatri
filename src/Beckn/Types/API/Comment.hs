module Beckn.Types.API.Comment where

import           Beckn.Types.Storage.Comment
import           Data.Default
import           Data.Swagger
import           EulerHS.Prelude

data CreateReq =
  CreateReq
  { _CommentedOnEntityId   :: Text
  , _commentedOnEntityType :: Text
  , _comment               :: Text
  , _info                  :: Maybe Text
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON CreateReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON CreateReq where
  toJSON = genericToJSON stripAllLensPrefixOptions

data CreateRes =
  CreateRes
  { _comment :: Comment
  }
  deriving (Show, Generic, Default, ToSchema)

instance ToJSON CreateRes where
  toJSON = genericToJSON stripLensPrefixOptions

instance FromJSON CreateRes where
  parseJSON = genericParseJSON stripLensPrefixOptions

data ListRes =
  ListRes
  { _comments :: [Comment]
  }
  deriving (Show, Generic, Default, ToSchema)

instance ToJSON ListRes where
  toJSON = genericToJSON stripLensPrefixOptions

instance FromJSON ListRes where
  parseJSON = genericParseJSON stripLensPrefixOptions
