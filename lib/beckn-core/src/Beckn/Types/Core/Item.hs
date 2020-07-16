module Beckn.Types.Core.Item where

import Beckn.Types.App
import Beckn.Types.Core.Descriptor
import Beckn.Types.Core.Image
import Beckn.Types.Core.Policy
import Beckn.Types.Core.Price
import Beckn.Types.Core.Tag
import Beckn.Utils.Common
import Data.Text
import EulerHS.Prelude

data Item = Item
  { _id :: Text,
    _parent_item_id :: Maybe Text,
    _descriptor :: Descriptor,
    _price :: Price,
    _model_id :: Maybe Text,
    _category_id :: Maybe Text,
    _brand_id :: Maybe Text,
    _promotional :: Bool,
    _ttl :: Maybe Integer,
    _tags :: [Tag]
  }
  deriving (Generic, Show)

instance FromJSON Item where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Item where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Item where
  example =
    Item
      { _id = idExample,
        _parent_item_id = Just idExample,
        _descriptor = example,
        _price = example,
        _model_id = Just idExample,
        _brand_id = Just idExample,
        _category_id = Just idExample,
        _tags = example,
        _promotional = False,
        _ttl = Just 86400
      }
