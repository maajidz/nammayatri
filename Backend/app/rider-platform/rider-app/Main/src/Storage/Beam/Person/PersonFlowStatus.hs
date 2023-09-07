{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage.Beam.Person.PersonFlowStatus where

import Data.Aeson
import qualified Database.Beam as B
import qualified Domain.Types.Person.PersonFlowStatus as Domain
import Tools.Beam.UtilsTH
import Kernel.Prelude

data PersonFlowStatusT f = PersonFlowStatusT
  { personId :: B.C f Text,
    flowStatus :: B.C f Domain.FlowStatus,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table PersonFlowStatusT where
  data PrimaryKey PersonFlowStatusT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . personId

type PersonFlowStatus = PersonFlowStatusT Identity

$(enableKVPG ''PersonFlowStatusT ['personId] [])

$(mkTableInstances ''PersonFlowStatusT "person_flow_status")
