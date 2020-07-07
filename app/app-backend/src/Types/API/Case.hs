module Types.API.Case where

import Beckn.Types.Core.Currency
import Beckn.Types.Storage.Case
import Beckn.Types.Storage.Location
import Beckn.Types.Storage.Products
import Data.Default
import Data.Swagger
import EulerHS.Prelude

data StatusRes = StatusRes
  { _case :: Case,
    _product :: [Products],
    _fromLocation :: Location,
    _toLocation :: Location
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON StatusRes where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON StatusRes where
  toJSON = genericToJSON stripAllLensPrefixOptions

data UpdateCaseReq = UpdateCaseReq
  { _quote :: Maybe Money,
    _transporterChoice :: Text
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON UpdateCaseReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON UpdateCaseReq where
  toJSON = genericToJSON stripAllLensPrefixOptions

data ProductInstance = ProductInstance
  { _case :: Case,
    _products :: [Products],
    _fromLocation :: Maybe Location,
    _toLocation :: Maybe Location
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON ProductInstance where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON ProductInstance where
  toJSON = genericToJSON stripAllLensPrefixOptions

newtype ListRes = ListRes
  { _productInstances :: [ProductInstance]
  }
  deriving (Show, Generic, ToSchema)

instance FromJSON ListRes where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON ListRes where
  toJSON = genericToJSON stripAllLensPrefixOptions
