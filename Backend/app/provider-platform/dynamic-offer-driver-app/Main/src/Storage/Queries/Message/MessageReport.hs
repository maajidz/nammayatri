{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-identities #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Storage.Queries.Message.MessageReport where

import qualified Database.Beam as B
import Database.Beam.Postgres
import Domain.Types.Message.Message
import qualified Domain.Types.Message.Message as Msg (Message (id))
import Domain.Types.Message.MessageReport as DTMR
import qualified Domain.Types.Message.MessageTranslation as MTD
import qualified Domain.Types.Person as P
import EulerHS.KVConnector.Utils (meshModelTableEntity)
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.External.Types
import Kernel.Prelude
import Kernel.Types.Common (MonadTime (getCurrentTime))
import Kernel.Types.Id
import Kernel.Types.Logging
import Lib.Utils
  ( FromTType' (fromTType'),
    ToTType' (toTType'),
    createWithKV,
    deleteWithKV,
    findAllWithKV,
    findAllWithOptionsKV,
    findOneWithKV,
    getMasterDBConfig,
    updateWithKV,
  )
import Sequelize
import qualified Sequelize as Se
import qualified Storage.Beam.Message.Message as BeamM
import qualified Storage.Beam.Message.MessageReport as BeamMR
import qualified Storage.Beam.Message.MessageTranslation as BeamMT
import Storage.Beam.Person as BeamP (PersonT (createdAt, id))
import Storage.Queries.Message.Message as QMM hiding (create)
import Storage.Queries.Message.MessageTranslation as QMMT hiding (create)
import qualified Storage.Queries.Person ()

createMany :: (L.MonadFlow m, Log m) => [MessageReport] -> m ()
createMany = traverse_ create

create :: (L.MonadFlow m, Log m) => MessageReport -> m ()
create = createWithKV

-- fullMessage ::
--   Language ->
--   From
--     ( Table MessageReportT
--         :& Table M.MessageT
--         :& MbTable MT.MessageTranslationT
--     )
-- fullMessage lang =
--   table
--     @MessageReportT
--     `innerJoin` table @M.MessageT
--       `Esq.on` ( \(messageReport :& message) ->
--                    messageReport ^. MessageReportMessageId ==. message ^. M.MessageTId
--                )
--     `leftJoin` table @MT.MessageTranslationT
--       `Esq.on` ( \(_ :& message :& messageTranslation) ->
--                    just (message ^. M.MessageTId) ==. messageTranslation ?. MT.MessageTranslationMessageId
--                      &&. messageTranslation ?. MT.MessageTranslationLanguage ==. val (Just lang)
--                )

-- findByDriverIdAndLanguage :: Transactionable m => Id P.Driver -> Language -> Maybe Int -> Maybe Int -> m [(MessageReport, RawMessage, Maybe MTD.MessageTranslation)]
-- findByDriverIdAndLanguage driverId language mbLimit mbOffset = do
--   let limitVal = min (fromMaybe 10 mbLimit) 10
--       offsetVal = fromMaybe 0 mbOffset
--   Esq.findAll $ do
--     (messageReport :& message :& mbMessageTranslation) <- from (fullMessage language)
--     where_ $
--       messageReport ^. MessageReportDriverId ==. val (toKey $ cast driverId)
--     orderBy [desc $ messageReport ^. MessageReportCreatedAt]
--     limit $ fromIntegral limitVal
--     offset $ fromIntegral offsetVal
--     return (messageReport, message, mbMessageTranslation)

findAllMessageWithSeConditionCreatedAtdesc :: (L.MonadFlow m, Log m) => [Se.Clause Postgres BeamM.MessageT] -> m [Message]
findAllMessageWithSeConditionCreatedAtdesc conditions = findAllWithOptionsKV conditions (Se.Desc BeamM.createdAt) Nothing Nothing

findAllMessageTranslationWithSeConditionCreatedAtdesc :: (L.MonadFlow m, Log m) => [Se.Clause Postgres BeamMT.MessageTranslationT] -> m [MTD.MessageTranslation]
findAllMessageTranslationWithSeConditionCreatedAtdesc conditions = findAllWithOptionsKV conditions (Se.Desc BeamMT.createdAt) Nothing Nothing

findAllMessageReportWithSeCondition :: (L.MonadFlow m, Log m) => [Se.Clause Postgres BeamMR.MessageReportT] -> m [MessageReport]
findAllMessageReportWithSeCondition = findAllWithKV

findByDriverIdAndLanguage :: (L.MonadFlow m, Log m) => Id P.Driver -> Language -> Maybe Int -> Maybe Int -> m [(MessageReport, RawMessage, Maybe MTD.MessageTranslation)]
findByDriverIdAndLanguage driverId language mbLimit mbOffset = do
  let limitVal = min (fromMaybe 10 mbLimit) 10
      offsetVal = fromMaybe 0 mbOffset
  messageReport <- findAllMessageReportWithSeCondition [Se.Is BeamMR.driverId $ Se.Eq $ getId driverId]
  message <- findAllMessageWithSeConditionCreatedAtdesc [Se.Is BeamM.id $ Se.In $ getId . DTMR.messageId <$> messageReport]
  let rawMessageFromMessage Message {..} =
        RawMessage
          { id = id,
            _type = _type,
            title = title,
            description = description,
            shortDescription = shortDescription,
            label = label,
            likeCount = likeCount,
            viewCount = viewCount,
            mediaFiles = mediaFiles,
            merchantId = merchantId,
            createdAt = createdAt
          }
  let mtSeCondition =
        [ Se.And
            [Se.Is BeamMT.messageId $ Se.In $ getId . Msg.id <$> message, Se.Is BeamMT.language $ Se.Eq language]
        ]
  messageTranslation <- findAllMessageTranslationWithSeConditionCreatedAtdesc mtSeCondition

  let messageReportAndMessage = foldl' (getMessageReportAndMessage messageReport) [] message
  let finalResult' = foldl' (getMessageTranslationAndMessage messageTranslation rawMessageFromMessage) [] messageReportAndMessage
  let finalResult = map (\(mr, rm, mt) -> (mr, rm, Just mt)) finalResult'
  pure $ take limitVal (drop offsetVal finalResult)
  where
    getMessageReportAndMessage messageReports acc message' =
      let messageeReports' = filter (\messageReport -> messageReport.messageId == message'.id) messageReports
       in acc <> ((\messageReport' -> (messageReport', message')) <$> messageeReports')

    getMessageTranslationAndMessage messageTranslations rawMessageFromMessage acc (msgRep, msg) =
      let messageTranslations' = filter (\messageTranslation' -> messageTranslation'.messageId == msg.id) messageTranslations
       in acc <> ((\messageTranslation' -> (msgRep, rawMessageFromMessage msg, messageTranslation')) <$> messageTranslations')

findByDriverIdMessageIdAndLanguage :: (L.MonadFlow m, Log m) => Id P.Driver -> Id Msg.Message -> Language -> m (Maybe (MessageReport, RawMessage, Maybe MTD.MessageTranslation))
findByDriverIdMessageIdAndLanguage driverId messageId language = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  case dbConf of
    Just _ -> do
      messageReport <- findByMessageIdAndDriverId messageId driverId
      case messageReport of
        Just report -> do
          rawMessage <- QMM.findById messageId
          case rawMessage of
            Just message -> do
              messageTranslation <- QMMT.findByMessageIdAndLanguage messageId language
              pure $ Just (report, message, messageTranslation)
            Nothing -> pure Nothing
        Nothing -> pure Nothing
    Nothing -> pure Nothing

findByMessageIdAndDriverId :: (L.MonadFlow m, Log m) => Id Msg.Message -> Id P.Driver -> m (Maybe MessageReport)
findByMessageIdAndDriverId (Id messageId) (Id driverId) = do
  findOneWithKV [Se.And [Se.Is BeamMR.messageId $ Se.Eq messageId, Se.Is BeamMR.driverId $ Se.Eq driverId]]

findByMessageIdAndStatusWithLimitAndOffset ::
  (L.MonadFlow m, Log m) =>
  Maybe Int ->
  Maybe Int ->
  Id Msg.Message ->
  Maybe DeliveryStatus ->
  m [(MessageReport, P.Person)]
findByMessageIdAndStatusWithLimitAndOffset mbLimit mbOffset (Id messageID) mbDeliveryStatus = do
  let limitVal = min (maybe 10 fromIntegral mbLimit) 20
      offsetVal = maybe 0 fromIntegral mbOffset
  messageReport <-
    findAllWithOptionsKV
      [ Se.And
          ( [Se.Is BeamMR.messageId $ Se.Eq messageID]
              <> ([Se.Is BeamMR.deliveryStatus $ Se.Eq (fromJust mbDeliveryStatus) | isJust mbDeliveryStatus])
          )
      ]
      (Se.Desc BeamMR.createdAt)
      Nothing
      Nothing
  person <-
    findAllWithOptionsKV
      [Se.Is BeamP.id $ Se.In $ getId . DTMR.driverId <$> messageReport]
      (Se.Desc BeamP.createdAt)
      Nothing
      Nothing

  let messageOfPerson = foldl' (getMessageOfPerson messageReport) [] person
  pure $ take limitVal (drop offsetVal messageOfPerson)
  where
    getMessageOfPerson messageReports acc person' =
      let messageReports' = filter (\messageReport -> getId messageReport.driverId == getId person'.id) messageReports
       in acc <> ((\messageReport -> (messageReport, person')) <$> messageReports')

getMessageCountByStatus :: L.MonadFlow m => Id Msg.Message -> DeliveryStatus -> m Int
getMessageCountByStatus (Id messageID) status = do
  dbConf <- getMasterDBConfig
  conn <- L.getOrInitSqlConn dbConf
  case conn of
    Right c -> do
      resp <-
        L.runDB c $
          L.findRow $
            B.select $
              B.aggregate_ (\_ -> B.as_ @Int B.countAll_) $
                B.filter_' (\(BeamMR.MessageReportT {..}) -> messageId B.==?. B.val_ messageID B.&&?. (deliveryStatus B.==?. B.val_ status)) $
                  B.all_ (meshModelTableEntity @BeamMR.MessageReportT @Postgres @(DatabaseWith BeamMR.MessageReportT))
      pure (either (const 0) (fromMaybe 0) resp)
    Left _ -> pure 0

getMessageCountByReadStatus :: L.MonadFlow m => Id Msg.Message -> m Int
getMessageCountByReadStatus (Id messageID) = do
  dbConf <- getMasterDBConfig
  conn <- L.getOrInitSqlConn dbConf
  case conn of
    Right c -> do
      resp <-
        L.runDB c $
          L.findRow $
            B.select $
              B.aggregate_ (\_ -> B.as_ @Int B.countAll_) $
                B.filter_' (\(BeamMR.MessageReportT {..}) -> messageId B.==?. B.val_ messageID B.&&?. readStatus B.==?. B.val_ True) $
                  B.all_ (meshModelTableEntity @BeamMR.MessageReportT @Postgres @(DatabaseWith BeamMR.MessageReportT))
      pure (either (const 0) (fromMaybe 0) resp)
    Left _ -> pure 0

updateSeenAndReplyByMessageIdAndDriverId :: (L.MonadFlow m, MonadTime m, Log m) => Id Msg.Message -> Id P.Driver -> Bool -> Maybe Text -> m ()
updateSeenAndReplyByMessageIdAndDriverId messageId driverId readStatus reply = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamMR.readStatus readStatus,
      Se.Set BeamMR.reply reply,
      Se.Set BeamMR.updatedAt now
    ]
    [Se.Is BeamMR.messageId $ Se.Eq $ getId messageId, Se.Is BeamMR.driverId $ Se.Eq $ getId driverId]

updateMessageLikeByMessageIdAndDriverIdAndReadStatus :: (L.MonadFlow m, MonadTime m, Log m) => Id Msg.Message -> Id P.Driver -> m ()
updateMessageLikeByMessageIdAndDriverIdAndReadStatus messageId driverId = do
  findByMessageIdAndDriverId messageId driverId >>= \case
    Just report -> do
      let likeStatus = not report.likeStatus
      now <- getCurrentTime
      updateWithKV
        [ Se.Set BeamMR.likeStatus likeStatus,
          Se.Set BeamMR.updatedAt now
        ]
        [Se.And [Se.Is BeamMR.messageId $ Se.Eq $ getId messageId, Se.Is BeamMR.driverId $ Se.Eq $ getId driverId, Se.Is BeamMR.readStatus $ Se.Eq True]]
    Nothing -> pure ()

updateDeliveryStatusByMessageIdAndDriverId :: (L.MonadFlow m, MonadTime m, Log m) => Id Msg.Message -> Id P.Driver -> DeliveryStatus -> m ()
updateDeliveryStatusByMessageIdAndDriverId messageId driverId deliveryStatus = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BeamMR.deliveryStatus deliveryStatus,
      Se.Set BeamMR.updatedAt now
    ]
    [Se.Is BeamMR.messageId $ Se.Eq $ getId messageId, Se.Is BeamMR.driverId $ Se.Eq $ getId driverId]

deleteByPersonId :: (L.MonadFlow m, Log m) => Id P.Person -> m ()
deleteByPersonId (Id personId) = deleteWithKV [Se.Is BeamMR.driverId (Se.Eq personId)]

instance FromTType' BeamMR.MessageReport MessageReport where
  fromTType' BeamMR.MessageReportT {..} = do
    pure $
      Just
        MessageReport
          { messageId = Id messageId,
            driverId = Id driverId,
            deliveryStatus = deliveryStatus,
            readStatus = readStatus,
            likeStatus = likeStatus,
            reply = reply,
            messageDynamicFields = messageDynamicFields,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamMR.MessageReport MessageReport where
  toTType' MessageReport {..} = do
    BeamMR.MessageReportT
      { BeamMR.messageId = getId messageId,
        BeamMR.driverId = getId driverId,
        BeamMR.deliveryStatus = deliveryStatus,
        BeamMR.readStatus = readStatus,
        BeamMR.likeStatus = likeStatus,
        BeamMR.reply = reply,
        BeamMR.messageDynamicFields = messageDynamicFields,
        BeamMR.createdAt = createdAt,
        BeamMR.updatedAt = updatedAt
      }
