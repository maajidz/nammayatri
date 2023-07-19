{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.CancellationReason where

import Domain.Types.CancellationReason
import qualified EulerHS.Language as L
import Kernel.Prelude hiding (isNothing)
import Kernel.Types.Logging (Log)
import Lib.Utils (FromTType' (fromTType'), ToTType' (toTType'), findAllWithOptionsKV)
import qualified Sequelize as Se
import qualified Storage.Beam.CancellationReason as BeamCR

findAll :: (L.MonadFlow m, Log m) => m [CancellationReason]
findAll = findAllWithOptionsKV [Se.Is BeamCR.enabled $ Se.Eq True] (Se.Desc BeamCR.priority) Nothing Nothing

instance FromTType' BeamCR.CancellationReason CancellationReason where
  fromTType' BeamCR.CancellationReasonT {..} = do
    pure $
      Just
        CancellationReason
          { reasonCode = CancellationReasonCode reasonCode,
            description = description,
            enabled = enabled,
            priority = priority
          }

instance ToTType' BeamCR.CancellationReason CancellationReason where
  toTType' CancellationReason {..} = do
    BeamCR.CancellationReasonT
      { BeamCR.reasonCode = (\(CancellationReasonCode x) -> x) reasonCode,
        BeamCR.description = description,
        BeamCR.enabled = enabled,
        BeamCR.priority = priority
      }
