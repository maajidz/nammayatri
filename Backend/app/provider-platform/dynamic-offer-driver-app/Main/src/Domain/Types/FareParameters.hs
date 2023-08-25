{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Domain.Types.FareParameters where

import qualified Data.Aeson as A
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.GenericPretty (PrettyShow (..))

data FareParameters = FareParameters
  { id :: Id FareParameters,
    driverSelectedFare :: Maybe Money,
    customerExtraFee :: Maybe Money,
    serviceCharge :: Maybe Money,
    govtCharges :: Maybe Money,
    baseFare :: Money,
    waitingCharge :: Maybe Money,
    nightShiftCharge :: Maybe Money,
    nightShiftRateIfApplies :: Maybe Double,
    fareParametersDetails :: FareParametersDetails
  }
  deriving (Generic, Show, Eq, PrettyShow)

data FareParametersDetails = ProgressiveDetails FParamsProgressiveDetails | SlabDetails FParamsSlabDetails
  deriving (Generic, Show, Eq, PrettyShow)

data FParamsProgressiveDetails = FParamsProgressiveDetails
  { deadKmFare :: Money,
    extraKmFare :: Maybe Money
  }
  deriving (Generic, Show, Eq, PrettyShow)

data FParamsSlabDetails = FParamsSlabDetails
  { platformFee :: Maybe Money,
    sgst :: Maybe HighPrecMoney,
    cgst :: Maybe HighPrecMoney
  }
  deriving (Generic, Show, Eq, PrettyShow)

type FullFareParametersProgressiveDetails = (Id FareParameters, FParamsProgressiveDetails)

data FareParametersType = Progressive | Slab deriving (Show, Read, Generic)

instance FromBackendRow Postgres FareParametersType

instance FromField FareParametersType where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be FareParametersType where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be FareParametersType

instance FromJSON FareParametersType where
  parseJSON = A.genericParseJSON A.defaultOptions

instance ToJSON FareParametersType where
  toJSON = A.genericToJSON A.defaultOptions

deriving stock instance Ord FareParametersType

deriving stock instance Eq FareParametersType

getFareParametersType :: FareParameters -> FareParametersType
getFareParametersType fareParams = case fareParams.fareParametersDetails of
  ProgressiveDetails _ -> Progressive
  SlabDetails _ -> Slab
