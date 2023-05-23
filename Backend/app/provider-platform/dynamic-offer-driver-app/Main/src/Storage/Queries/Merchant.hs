{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Merchant
  {-# WARNING
    "This module contains direct calls to the table. \
  \ But most likely you need a version from CachedQueries with caching results feature."
    #-}
where

import Domain.Types.Merchant as DM
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Geofencing
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.Merchant as BeamM
import Storage.Tabular.Merchant
import qualified Storage.Tabular.VechileNew as VN

findById :: Transactionable m => Id Merchant -> m (Maybe Merchant)
findById = Esq.findById

findById' :: L.MonadFlow m => Id Merchant -> m (Maybe Merchant)
findById' (Id merchantId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamMerchantToDomain <$>) <$> KV.findWithKVConnector dbCOnf' VN.meshConfig [Se.Is BeamM.id $ Se.Eq merchantId]
    Nothing -> pure Nothing

findBySubscriberId :: Transactionable m => ShortId Subscriber -> m (Maybe Merchant)
findBySubscriberId subscriberId = Esq.findOne $ do
  org <- from $ table @MerchantT
  where_ $
    org ^. MerchantSubscriberId ==. val (subscriberId.getShortId)
  return org

findBySubscriberId' :: L.MonadFlow m => ShortId Subscriber -> m (Maybe Merchant)
findBySubscriberId' (ShortId subscriberId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamMerchantToDomain <$>) <$> KV.findWithKVConnector dbCOnf' VN.meshConfig [Se.Is BeamM.subscriberId $ Se.Eq subscriberId]
    Nothing -> pure Nothing

findByShortId :: Transactionable m => ShortId Merchant -> m (Maybe Merchant)
findByShortId shortId = Esq.findOne $ do
  org <- from $ table @MerchantT
  where_ $
    org ^. MerchantShortId ==. val (shortId.getShortId)
  return org

findByShortId' :: L.MonadFlow m => ShortId Subscriber -> m (Maybe Merchant)
findByShortId' (ShortId shortId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamMerchantToDomain <$>) <$> KV.findWithKVConnector dbCOnf' VN.meshConfig [Se.Is BeamM.shortId $ Se.Eq shortId]
    Nothing -> pure Nothing

loadAllProviders :: Transactionable m => m [Merchant]
loadAllProviders =
  Esq.findAll $ do
    org <- from $ table @MerchantT
    where_ $
      org ^. MerchantStatus ==. val DM.APPROVED
        &&. org ^. MerchantEnabled
    return org

loadAllProviders' :: L.MonadFlow m => m [Merchant]
loadAllProviders' = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure []) (transformBeamMerchantToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' Mesh.meshConfig [Se.And [Se.Is BeamM.status $ Se.Eq DM.APPROVED, Se.Is BeamM.enabled $ Se.Eq True]]
    Nothing -> pure []

findAll :: Transactionable m => m [Merchant]
findAll =
  Esq.findAll $ do from $ table @MerchantT

findAll' :: L.MonadFlow m => m [Merchant]
findAll' = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure []) (transformBeamMerchantToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' Mesh.meshConfig []
    Nothing -> pure []

update :: Merchant -> SqlDB ()
update org = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ MerchantName =. val org.name,
        MerchantDescription =. val org.description,
        MerchantHeadCount =. val org.headCount,
        MerchantEnabled =. val org.enabled,
        MerchantUpdatedAt =. val now,
        MerchantFromTime =. val org.fromTime
      ]
    where_ $ tbl ^. MerchantTId ==. val (toKey org.id)

update' :: (L.MonadFlow m, MonadTime m) => Merchant -> m (MeshResult ())
update' org = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  now <- getCurrentTime
  case dbConf of
    Just dbConf' ->
      KV.updateWoReturningWithKVConnector
        dbConf'
        VN.meshConfig
        [ Se.Set BeamM.name org.name,
          Se.Set BeamM.description org.description,
          Se.Set BeamM.headCount org.headCount,
          Se.Set BeamM.enabled org.enabled,
          Se.Set BeamM.updatedAt now,
          Se.Set BeamM.fromTime org.fromTime
        ]
        [Se.Is BeamM.id (Se.Eq (getId org.id))]
    Nothing -> pure (Left (MKeyNotFound "DB Config not found"))

transformBeamMerchantToDomain :: BeamM.Merchant -> Merchant
transformBeamMerchantToDomain BeamM.MerchantT {..} = do
  Merchant
    { id = Id id,
      name = name,
      description = description,
      subscriberId = ShortId subscriberId,
      uniqueKeyId = uniqueKeyId,
      shortId = ShortId shortId,
      city = city,
      mobileNumber = mobileNumber,
      mobileCountryCode = mobileCountryCode,
      gstin = gstin,
      fromTime = fromTime,
      toTime = toTime,
      headCount = headCount,
      status = status,
      verified = verified,
      enabled = enabled,
      internalApiKey = internalApiKey,
      createdAt = createdAt,
      updatedAt = updatedAt,
      geofencingConfig = GeofencingConfig originRestriction destinationRestriction,
      info = info
    }

transformDomainMerchantToBeam :: Merchant -> BeamM.Merchant
transformDomainMerchantToBeam Merchant {..} =
  BeamM.MerchantT
    { BeamM.id = getId id,
      BeamM.name = name,
      BeamM.description = description,
      BeamM.subscriberId = getShortId subscriberId,
      BeamM.uniqueKeyId = uniqueKeyId,
      BeamM.shortId = getShortId shortId,
      BeamM.city = city,
      BeamM.mobileNumber = mobileNumber,
      BeamM.mobileCountryCode = mobileCountryCode,
      BeamM.gstin = gstin,
      BeamM.fromTime = fromTime,
      BeamM.toTime = toTime,
      BeamM.headCount = headCount,
      BeamM.status = status,
      BeamM.verified = verified,
      BeamM.enabled = enabled,
      BeamM.internalApiKey = internalApiKey,
      BeamM.createdAt = createdAt,
      BeamM.updatedAt = updatedAt,
      BeamM.originRestriction = origin geofencingConfig,
      BeamM.destinationRestriction = destination geofencingConfig,
      BeamM.info = info
    }
