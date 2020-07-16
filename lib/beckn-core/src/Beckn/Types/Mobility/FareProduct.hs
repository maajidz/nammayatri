module Beckn.Types.Mobility.FareProduct where

import Beckn.Types.App
import Beckn.Types.Core.Descriptor
import Beckn.Types.Core.Item
import Beckn.Types.Core.Policy
import Beckn.Utils.Common
import Data.Text
import EulerHS.Prelude

data FareProduct = FareProduct
  { _id :: Text,
    _descriptor :: Descriptor,
    _policy_id :: Text
  }
  deriving (Generic, Show)

instance FromJSON FareProduct where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON FareProduct where
  toJSON = genericToJSON stripLensPrefixOptions

instance Example FareProduct where
  example =
    FareProduct
      { _id = idExample,
        _descriptor = example,
        _policy_id = idExample
      }
