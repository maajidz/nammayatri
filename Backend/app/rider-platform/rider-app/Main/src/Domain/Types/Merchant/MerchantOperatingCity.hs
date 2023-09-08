{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.Merchant.MerchantOperatingCity where

-- import qualified Database.Beam as B
-- import Database.Beam.Backend
import Database.Beam.MySQL ()
import qualified Domain.Types.Merchant as DM
import Kernel.Prelude
-- import Database.Beam.Postgres
--   ( Postgres,
--   )
-- import Database.PostgreSQL.Simple.FromField (FromField, fromField)
-- import Kernel.Types.Common hiding (id)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id

-- import Database.Beam.Backend
--     ( HasSqlValueSyntax, autoSqlValueSyntax, BeamSqlBackend, FromBackendRow )
-- import Database.Beam.Backend.SQL (HasSqlValueSyntax(sqlValueSyntax))
-- import qualified Database.Beam as B
-- import Database.Beam.Postgres (Postgres)
-- import Database.PostgreSQL.Simple.FromField (FromField (fromField))
-- import Kernel.Utils.Common (fromFieldEnum)

data MerchantOperatingCity = MerchantOperatingCity
  { id :: Id MerchantOperatingCity,
    merchantId :: Id DM.Merchant,
    city :: Context.City
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, ToSchema)

-- data City = BANGALORE | KOLKATA | KOCHI | CHENNAI | DELHI | MUMBAI | DEFAULT
--   deriving (Generic, FromJSON, ToJSON, Show, Read, Eq, ToSchema, Ord)

-- instance FromField Context.City where
--   fromField = fromFieldEnum

-- -- type City = Context.City

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be Context.City where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance BeamSqlBackend be => B.HasSqlEqualityCheck be Context.City

-- instance FromBackendRow Postgres Context.City

-- instance IsString Context.City where
--   fromString = show
