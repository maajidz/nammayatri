module Beckn.Types.Core.Payment where

import Beckn.Types.Core.MonetaryValue
import Beckn.Types.Core.PaymentEndpoint
import Beckn.Types.Core.PaymentPolicy
import Beckn.Types.Core.State
import Beckn.Utils.Common
import Data.Time
import EulerHS.Prelude hiding (State)

data Payment = Payment
  { _transaction_id :: Maybe Text,
    _type :: Maybe Text, -- ON-ORDER, PRE-FULFILLMENT, ON-FULFILLMENT, POST-FULFILLMENT
    _payer :: Maybe PaymentEndpoint,
    _payee :: Maybe PaymentEndpoint,
    _method :: Text, -- CASH, CHEQUE, DEMAND-DRAFT, UPI, RTGS, NEFT, IMPS
    _amount :: MonetaryValue,
    _state :: Maybe State,
    _due_date :: Maybe UTCTime,
    _duration :: Maybe MonetaryValue,
    _terms :: Maybe PaymentPolicy
  }
  deriving (Generic, Show)

instance FromJSON Payment where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Payment where
  toJSON = genericToJSON stripAllLensPrefixOptions

instance Example Payment where
  example =
    Payment
      { _transaction_id = Just idExample,
        _type = Just "ON-ORDER",
        _payer = example,
        _payee = example,
        _method = "CASH",
        _amount = example,
        _state = Nothing,
        _due_date = Just example,
        _duration = Nothing,
        _terms = example
      }
