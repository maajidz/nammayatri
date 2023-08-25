{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Merchant.MerchantPaymentMethod where

import Data.Aeson.Types
import qualified Data.List as List
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import Domain.Types.Common (UsageSafety (..))
import Domain.Types.Merchant (Merchant)
import Kernel.Prelude
import Kernel.Types.Common (fromFieldEnum)
import Kernel.Types.Id
import qualified Text.Show

data MerchantPaymentMethodD (s :: UsageSafety) = MerchantPaymentMethod
  { id :: Id MerchantPaymentMethod,
    merchantId :: Id Merchant,
    paymentType :: PaymentType,
    paymentInstrument :: PaymentInstrument,
    collectedBy :: PaymentCollector,
    priority :: Int,
    updatedAt :: UTCTime,
    createdAt :: UTCTime
  }
  deriving (Generic)

type MerchantPaymentMethod = MerchantPaymentMethodD 'Safe

instance FromJSON (MerchantPaymentMethodD 'Unsafe)

instance ToJSON (MerchantPaymentMethodD 'Unsafe)

data PaymentType = PREPAID | POSTPAID
  deriving (Generic, FromJSON, ToJSON, Show, Read, Eq, Ord)

instance FromField PaymentType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be PaymentType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be PaymentType

instance FromBackendRow Postgres PaymentType

instance IsString PaymentType where
  fromString = show

data PaymentInstrument = Card CardType | Wallet WalletType | UPI | NetBanking | Cash
  deriving (Generic, Eq, Ord)

instance FromField PaymentInstrument where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be PaymentInstrument where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be PaymentInstrument

instance FromBackendRow Postgres PaymentInstrument

instance IsString PaymentInstrument where
  fromString = show

instance ToJSON PaymentInstrument where
  toJSON = genericToJSON paymentInstrumentOptions

instance FromJSON PaymentInstrument where
  parseJSON = genericParseJSON paymentInstrumentOptions

paymentInstrumentOptions :: Options
paymentInstrumentOptions =
  defaultOptions
    { sumEncoding = paymentInstrumentTaggedObject
    }

paymentInstrumentTaggedObject :: SumEncoding
paymentInstrumentTaggedObject =
  defaultTaggedObject
    { tagFieldName = "instrumentType",
      contentsFieldName = "instrumentName"
    }

instance Show PaymentInstrument where
  show (Card p) = "Card_" <> show p
  show (Wallet p) = "Wallet_" <> show p
  show UPI = "UPI"
  show NetBanking = "NetBanking"
  show Cash = "Cash"

instance Read PaymentInstrument where
  readsPrec d' =
    readParen
      (d' > app_prec)
      ( \r ->
          [ (Card v1, r2)
            | r1 <- stripPrefix "Card_" r,
              (v1, r2) <- readsPrec (app_prec + 1) r1
          ]
            ++ [ (Wallet v1, r2)
                 | r1 <- stripPrefix "Wallet_" r,
                   (v1, r2) <- readsPrec (app_prec + 1) r1
               ]
            ++ [ (UPI, r1)
                 | r1 <- stripPrefix "UPI" r
               ]
            ++ [ (NetBanking, r1)
                 | r1 <- stripPrefix "NetBanking" r
               ]
            ++ [ (Cash, r1)
                 | r1 <- stripPrefix "Cash" r
               ]
      )
    where
      app_prec = 10
      stripPrefix pref r = bool [] [List.drop (length pref) r] $ List.isPrefixOf pref r

data CardType = DefaultCardType
  deriving (Show, Read, Eq, Ord)

-- Generic instances for type with single value will not work
instance FromJSON CardType where
  parseJSON (String "DefaultCardType") = pure DefaultCardType
  parseJSON (String _) = parseFail "Expected \"DefaultCardType\""
  parseJSON e = typeMismatch "String" e

instance ToJSON CardType where
  toJSON = String . show

data WalletType = DefaultWalletType
  deriving (Show, Read, Eq, Ord)

-- Generic instances for type with single value will not work
instance FromJSON WalletType where
  parseJSON (String "DefaultWalletType") = pure DefaultWalletType
  parseJSON (String _) = parseFail "Expected \"DefaultWalletType\""
  parseJSON e = typeMismatch "String" e

instance ToJSON WalletType where
  toJSON = String . show

data PaymentCollector = BAP | BPP
  deriving (Generic, FromJSON, ToJSON, Show, Read, Eq, Ord)

instance FromField PaymentCollector where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be PaymentCollector where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be PaymentCollector

instance FromBackendRow Postgres PaymentCollector

instance IsString PaymentCollector where
  fromString = show

data PaymentMethodInfo = PaymentMethodInfo
  { paymentType :: PaymentType,
    paymentInstrument :: PaymentInstrument,
    collectedBy :: PaymentCollector
  }
  deriving (Show, Eq)

mkPaymentMethodInfo :: MerchantPaymentMethod -> PaymentMethodInfo
mkPaymentMethodInfo MerchantPaymentMethod {..} = PaymentMethodInfo {..}

getPrepaidPaymentUrl :: MerchantPaymentMethod -> Maybe Text
getPrepaidPaymentUrl mpm = do
  if mpm.paymentType == PREPAID && mpm.collectedBy == BPP && mpm.paymentInstrument /= Cash
    then Just $ mkDummyPaymentUrl mpm
    else Nothing

getPostpaidPaymentUrl :: MerchantPaymentMethod -> Maybe Text
getPostpaidPaymentUrl mpm = do
  if mpm.paymentType == POSTPAID && mpm.collectedBy == BPP && mpm.paymentInstrument /= Cash
    then Just $ mkDummyPaymentUrl mpm
    else Nothing

mkDummyPaymentUrl :: MerchantPaymentMethod -> Text
mkDummyPaymentUrl MerchantPaymentMethod {..} = do
  "payment_link_for_paymentInstrument="
    <> show paymentInstrument
    <> ";collectedBy="
    <> show collectedBy
    <> ";paymentType="
    <> show paymentType
