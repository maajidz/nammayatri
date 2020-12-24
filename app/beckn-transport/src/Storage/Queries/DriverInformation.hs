module Storage.Queries.DriverInformation where

import App.Types (AppEnv (dbCfg), Flow)
import qualified Beckn.Storage.Common as Storage.Common
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Amount (Amount)
import Beckn.Utils.Common (getCurrTime, getSchemaName)
import Database.Beam ((<-.), (==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.App (DriverId)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.DriverInformation as Storage

getDbTable :: Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.DriverInformationT))
getDbTable =
  DB._driverInformation . DB.transporterDb <$> getSchemaName

create :: Storage.DriverInformation -> Flow ()
create Storage.DriverInformation {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.Common.insertExpression Storage.DriverInformation {..})
    >>= either DB.throwDBError pure

findById :: DriverId -> Flow (Maybe Storage.DriverInformation)
findById id = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.DriverInformation {..} = _driverId ==. B.val_ id

findByIds :: [DriverId] -> Flow [Storage.DriverInformation]
findByIds ids = do
  dbTable <- getDbTable
  DB.findAll dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.DriverInformation {..} = _driverId `B.in_` (B.val_ <$> ids)

update :: DriverId -> Int -> Amount -> Flow ()
update driverId completedRides earnings = do
  dbTable <- getDbTable
  now <- getCurrTime
  DB.update dbTable (setClause completedRides earnings now) (predicate driverId)
    >>= either DB.throwDBError pure
  where
    setClause cr e now Storage.DriverInformation {..} =
      mconcat
        [ _completedRidesNumber <-. B.val_ cr,
          _earnings <-. B.val_ e,
          _updatedAt <-. B.val_ now
        ]
    predicate id Storage.DriverInformation {..} = _driverId ==. B.val_ id

fetchMostOutdatedDriversInfo :: Integer -> Flow [Storage.DriverInformation]
fetchMostOutdatedDriversInfo limit = do
  dbTable <- getDbTable
  let offset = 0
  let orderByDesc Storage.DriverInformation {..} = B.desc_ _updatedAt
  DB.findAllWithLimitOffset dbTable limit offset orderByDesc
    >>= either DB.throwDBError pure
