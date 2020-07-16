module Storage.Queries.Leads where

import App.Types
import Database.Beam ((==.))
import qualified Database.Beam as B
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries as DB
import Types.App
import qualified Types.Storage.DB as DB
import qualified Types.Storage.Leads as Storage

dbTable :: B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.LeadsT)
dbTable = DB._leads DB.transporterDb

create :: Storage.Leads -> Flow ()
create Storage.Leads {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Leads {..})
    >>= either DB.throwDBError pure

findLeadsById ::
  LeadsId -> Flow (Maybe Storage.Leads)
findLeadsById id =
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Leads {..} = _id ==. B.val_ id
