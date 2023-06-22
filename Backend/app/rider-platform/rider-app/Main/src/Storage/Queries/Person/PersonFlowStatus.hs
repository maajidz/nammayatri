{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Person.PersonFlowStatus
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Person
import Domain.Types.Person.PersonFlowStatus
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Lib.Utils
import qualified Sequelize as Se
import qualified Storage.Beam.Person.PersonFlowStatus as BeamPFS
import Storage.Tabular.Person.PersonFlowStatus

create :: L.MonadFlow m => DPFS.PersonFlowStatus -> m (MeshResult ())
create personFlowStatus = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamPFS.PersonFlowStatusT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' updatedMeshConfig (transformDomainPersonFlowStatusToBeam personFlowStatus)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

getStatus ::
  (Transactionable m) =>
  Id Person ->
  m (Maybe DPFS.FlowStatus)
getStatus personId = do
  findOne $ do
    personFlowStatus <- from $ table @PersonFlowStatusT
    where_ $
      personFlowStatus ^. PersonFlowStatusTId ==. val (toKey personId)
    return $ personFlowStatus ^. PersonFlowStatusFlowStatus

updateStatus :: Id Person -> DPFS.FlowStatus -> SqlDB ()
updateStatus personId flowStatus = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonFlowStatusUpdatedAt =. val now,
        PersonFlowStatusFlowStatus =. val flowStatus
      ]
    where_ $ tbl ^. PersonFlowStatusTId ==. val (toKey personId)

deleteByPersonId :: Id Person -> SqlDB ()
deleteByPersonId personId = do
  Esq.delete $ do
    personFlowStatus <- from $ table @PersonFlowStatusT
    where_ (personFlowStatus ^. PersonFlowStatusTId ==. val (toKey personId))

updateToIdleMultiple :: [Id Person] -> UTCTime -> SqlDB ()
updateToIdleMultiple personIds now = do
  Esq.update $ \tbl -> do
    set
      tbl
      [ PersonFlowStatusUpdatedAt =. val now,
        PersonFlowStatusFlowStatus =. val DPFS.IDLE
      ]
    where_ $ tbl ^. PersonFlowStatusTId `in_` valList (toKey <$> personIds)

transformBeamPersonFlowStatusToDomain :: BeamPFS.PersonFlowStatus -> PersonFlowStatus
transformBeamPersonFlowStatusToDomain BeamPFS.PersonFlowStatusT {..} = do
  PersonFlowStatus
    { personId = Id personId,
      flowStatus = flowStatus,
      updatedAt = updatedAt
    }

transformDomainPersonFlowStatusToBeam :: PersonFlowStatus -> BeamPFS.PersonFlowStatus
transformDomainPersonFlowStatusToBeam PersonFlowStatus {..} =
  BeamPFS.defaultPersonFlowStatus
    { BeamPFS.personId = getId personId,
      BeamPFS.flowStatus = flowStatus,
      BeamPFS.updatedAt = updatedAt
    }
