module Storage.Queries.Case where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Schema
import qualified Beckn.Types.Storage.Case as Storage
import Beckn.Types.Storage.Organization
import Beckn.Utils.Common
import Data.Time (UTCTime)
import Database.Beam ((&&.), (<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB

getDbTable :: (HasSchemaName m, Functor m) => m (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.CaseT))
getDbTable =
  DB._case . DB.transporterDb <$> getSchemaName

createFlow :: Storage.Case -> Flow ()
createFlow = DB.runSqlDB . create

create :: Storage.Case -> DB.SqlDB ()
create case_ = do
  dbTable <- getDbTable
  DB.createOne' dbTable (Storage.insertExpression case_)

findAllByIds :: [Id Storage.Case] -> Flow [Storage.Case]
findAllByIds ids = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Case {..} =
      B.in_ id (B.val_ <$> ids)

findById :: Id Storage.Case -> Flow (Maybe Storage.Case)
findById caseId = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Case {..} = id ==. B.val_ caseId

findByParentCaseIdAndType :: Id Storage.Case -> Storage.CaseType -> Flow (Maybe Storage.Case)
findByParentCaseIdAndType pCaseId cType = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Case {..} =
      parentCaseId ==. B.val_ (Just pCaseId)
        &&. _type ==. B.val_ cType

findBySid :: Text -> Flow (Maybe Storage.Case)
findBySid sid = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Case {..} = shortId ==. B.val_ (ShortId sid)

updateStatusFlow ::
  Id Storage.Case ->
  Storage.CaseStatus ->
  Flow ()
updateStatusFlow id newStatus = DB.runSqlDB (updateStatus id newStatus)

updateStatus ::
  Id Storage.Case ->
  Storage.CaseStatus ->
  DB.SqlDB ()
updateStatus caseId newStatus = do
  dbTable <- getDbTable
  currTime <- asks DB.currentTime
  DB.update'
    dbTable
    (setClause newStatus currTime)
    (predicate caseId)
  where
    predicate cid Storage.Case {..} = id ==. B.val_ cid
    setClause status_ currTime Storage.Case {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          status <-. B.val_ status_
        ]

updateStatusByIdsFlow ::
  [Id Storage.Case] ->
  Storage.CaseStatus ->
  Flow ()
updateStatusByIdsFlow ids newStatus = do
  dbTable <- getDbTable
  (currTime :: UTCTime) <- getCurrentTime
  DB.update
    dbTable
    (setClause newStatus currTime)
    (predicate ids)
  where
    predicate cids Storage.Case {..} = B.in_ id (B.val_ <$> cids)
    setClause status_ currTime Storage.Case {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          status <-. B.val_ status_
        ]

updateStatusByIds ::
  [Id Storage.Case] ->
  Storage.CaseStatus ->
  DB.SqlDB ()
updateStatusByIds ids newStatus = do
  dbTable <- getDbTable
  currTime <- asks DB.currentTime
  DB.update'
    dbTable
    (setClause newStatus currTime)
    (predicate ids)
  where
    predicate cids Storage.Case {..} = B.in_ id (B.val_ <$> cids)
    setClause status_ currTime Storage.Case {..} =
      mconcat
        [ updatedAt <-. B.val_ currTime,
          status <-. B.val_ status_
        ]

findByIdType :: [Id Storage.Case] -> Storage.CaseType -> Flow (Maybe Storage.Case)
findByIdType ids type_ = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
  where
    predicate Storage.Case {..} =
      _type ==. B.val_ type_
        &&. B.in_ id (B.val_ <$> ids)

findAllByIdType :: [Id Storage.Case] -> Storage.CaseType -> Flow [Storage.Case]
findAllByIdType ids type_ = do
  dbTable <- getDbTable
  DB.findAll dbTable identity predicate
  where
    predicate Storage.Case {..} =
      _type ==. B.val_ type_
        &&. B.in_ id (B.val_ <$> ids)

findAllByTypeStatuses :: Integer -> Integer -> Storage.CaseType -> [Storage.CaseStatus] -> Id Organization -> UTCTime -> Flow [Storage.Case]
findAllByTypeStatuses limit offset csType statuses orgId now = do
  dbTable <- getDbTable
  DB.findAll dbTable (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc) predicate
  where
    orderByDesc Storage.Case {..} = B.desc_ createdAt
    predicate Storage.Case {..} =
      _type ==. B.val_ csType
        &&. provider ==. B.val_ (Just $ getId orgId)
        &&. B.in_ status (B.val_ <$> statuses)
        &&. validTill B.>. B.val_ now

findAllByTypeStatusTime :: Integer -> Integer -> Storage.CaseType -> [Storage.CaseStatus] -> Id Organization -> UTCTime -> UTCTime -> Flow [Storage.Case]
findAllByTypeStatusTime limit offset csType statuses orgId now fromTime = do
  dbTable <- getDbTable
  DB.findAll dbTable (B.limit_ limit . B.offset_ offset . B.orderBy_ orderByDesc) predicate
  where
    orderByDesc Storage.Case {..} = B.desc_ createdAt
    predicate Storage.Case {..} =
      _type ==. B.val_ csType
        &&. provider ==. B.val_ (Just $ getId orgId)
        &&. B.in_ status (B.val_ <$> statuses)
        &&. validTill B.>. B.val_ now
        &&. createdAt B.<. B.val_ fromTime

findAllExpiredByStatus :: [Storage.CaseStatus] -> Storage.CaseType -> UTCTime -> UTCTime -> Flow [Storage.Case]
findAllExpiredByStatus statuses csType from to = do
  dbTable <- getDbTable
  (now :: UTCTime) <- getCurrentTime
  DB.findAll dbTable identity (predicate now)
  where
    predicate now Storage.Case {..} =
      _type ==. B.val_ csType
        &&. B.in_ status (B.val_ <$> statuses)
        &&. validTill B.<=. B.val_ now
        &&. createdAt B.>=. B.val_ from
        &&. createdAt B.<=. B.val_ to
