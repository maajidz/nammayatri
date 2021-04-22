module Storage.Queries.Leads where

import App.Types
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as DB
import Beckn.Types.Id
import Beckn.Types.Schema
import Beckn.Utils.Common
import Database.Beam ((==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Types.Storage.DB as DB
import qualified Types.Storage.Leads as Storage

getDbTable :: Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.LeadsT))
getDbTable =
  DB._leads . DB.transporterDb <$> getSchemaName

create :: Storage.Leads -> Flow ()
create Storage.Leads {..} = do
  dbTable <- getDbTable
  DB.createOne dbTable (Storage.insertExpression Storage.Leads {..})
    >>= checkDBError

findLeadsById ::
  Id Storage.Leads -> Flow (Maybe Storage.Leads)
findLeadsById id = do
  dbTable <- getDbTable
  DB.findOne dbTable predicate
    >>= checkDBError
  where
    predicate Storage.Leads {..} = _id ==. B.val_ id
