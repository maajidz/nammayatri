{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.FareProduct where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Vehicle as Vehicle
import Kernel.Prelude
import Kernel.Types.Id (Id)

data FarePolicyType = NORMAL | SLAB deriving (Generic, Eq, Show, Read, FromJSON, ToJSON)

data FlowType = NORMAL_RIDE_FLOW | RIDE_OTP_FLOW deriving (Generic, Eq, Show, Read, FromJSON, ToJSON)

data FareProduct = FareProduct
  { id :: Id FareProduct,
    merchantId :: Id DM.Merchant,
    vehicleVariant :: Vehicle.Variant,
    farePolicyType :: FarePolicyType,
    flowType :: FlowType
  }
  deriving (Generic, Show, FromJSON, ToJSON)
