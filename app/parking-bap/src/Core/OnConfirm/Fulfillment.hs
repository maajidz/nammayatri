module Core.OnConfirm.Fulfillment where

import Beckn.Prelude
import Beckn.Utils.JSON (stripPrefixUnderscoreIfAny)
import Core.OnConfirm.End
import Core.OnConfirm.Start
import Core.OnConfirm.Vehicle (Vehicle)

data Fulfillment = Fulfillment
  { _type :: Text,
    tracking :: Bool,
    start :: Start,
    end :: End,
    vehicle :: Vehicle
  }
  deriving (Generic)

instance FromJSON Fulfillment where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny
