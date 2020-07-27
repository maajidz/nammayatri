module Storage.Queries.Tracker where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Database.Beam ((==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import Types.App
import qualified Types.Storage.DB as DB
import qualified Types.Storage.Tracker as Storage

dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.TrackerT)
dbTable = DB._tracker DB.transporterDb

create :: Storage.Tracker -> Flow ()
create Storage.Tracker {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Tracker {..})
    >>= either DB.throwDBError pure

findTrackerById ::
  TrackerId -> Flow (Maybe Storage.Tracker)
findTrackerById id =
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Tracker {..} = _id ==. B.val_ id
