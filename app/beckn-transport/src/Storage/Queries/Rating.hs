module Storage.Queries.Rating where

import App.Types (AppEnv (dbCfg), Flow)
import qualified Beckn.Storage.Common as Storage
import qualified Beckn.Storage.Queries as Query
import Beckn.Types.App (ProductInstanceId, RatingId)
import qualified Beckn.Types.Storage.Rating as Storage
import Beckn.Utils.Common (getCurrTime, getSchemaName)
import Database.Beam (SqlEq ((==.)), (<-.))
import qualified Database.Beam as B
import EulerHS.Prelude
import qualified Types.Storage.DB as DB

getDbTable :: Flow (B.DatabaseEntity be DB.TransporterDb (B.TableEntity Storage.RatingT))
getDbTable =
  DB._rating . DB.transporterDb <$> getSchemaName

create :: Storage.Rating -> Flow ()
create rating = do
  dbTable <- getDbTable
  Query.createOne dbTable (Storage.insertExpression rating)
    >>= either Query.throwDBError pure

updateRatingValue :: RatingId -> Int -> Flow ()
updateRatingValue ratingId newRatingValue = do
  dbTable <- getDbTable
  now <- getCurrTime
  Query.update
    dbTable
    ( \Storage.Rating {..} ->
        mconcat
          [ _ratingValue <-. B.val_ newRatingValue,
            _updatedAt <-. B.val_ now
          ]
    )
    (\Storage.Rating {..} -> _id ==. B.val_ ratingId)
    >>= either Query.throwDBError pure

findByProductInstanceId :: ProductInstanceId -> Flow (Maybe Storage.Rating)
findByProductInstanceId productInsId = do
  dbTable <- getDbTable
  Query.findOne dbTable predicate
    >>= either Query.throwDBError pure
  where
    predicate Storage.Rating {..} = _productInstanceId ==. B.val_ productInsId
