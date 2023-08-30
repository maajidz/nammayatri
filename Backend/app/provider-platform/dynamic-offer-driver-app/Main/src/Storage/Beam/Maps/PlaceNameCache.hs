{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Storage.Beam.Maps.PlaceNameCache where

import Data.Serialize
import qualified Data.Time as Time
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.Maps.PlaceNameCache as Domain
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common ()
import Lib.Utils ()
import Sequelize

data PlaceNameCacheT f = PlaceNameCacheT
  { id :: B.C f Text,
    formattedAddress :: B.C f (Maybe Text),
    plusCode :: B.C f (Maybe Text),
    lat :: B.C f Double,
    lon :: B.C f Double,
    placeId :: B.C f (Maybe Text),
    geoHash :: B.C f (Maybe Text),
    addressComponents :: B.C f [Domain.AddressResp],
    createdAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PlaceNameCacheT where
  data PrimaryKey PlaceNameCacheT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type PlaceNameCache = PlaceNameCacheT Identity

placeNameCacheTMod :: PlaceNameCacheT (B.FieldModification (B.TableField PlaceNameCacheT))
placeNameCacheTMod =
  B.tableModification
    { id = B.fieldNamed "id",
      formattedAddress = B.fieldNamed "formatted_address",
      plusCode = B.fieldNamed "plus_code",
      lat = B.fieldNamed "lat",
      lon = B.fieldNamed "lon",
      placeId = B.fieldNamed "place_id",
      geoHash = B.fieldNamed "geo_hash",
      addressComponents = B.fieldNamed "address_components",
      createdAt = B.fieldNamed "created_at"
    }

$(enableKVPG ''PlaceNameCacheT ['id] [['placeId], ['geoHash]])

$(mkTableInstances ''PlaceNameCacheT "place_name_cache" "atlas_driver_offer_bpp")
