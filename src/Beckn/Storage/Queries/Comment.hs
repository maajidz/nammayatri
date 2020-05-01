{-# LANGUAGE RecordWildCards #-}

module Beckn.Storage.Queries.Comment where

import           Database.Beam               ((&&.), (<-.), (==.))
import           EulerHS.Prelude             hiding (id)

import qualified Beckn.Storage.Queries       as DB
import           Beckn.Types.App
import           Beckn.Types.Common
import qualified Beckn.Types.Storage.Comment as Storage
import qualified Beckn.Types.Storage.DB      as DB

import           Beckn.Utils.Common
import qualified Database.Beam               as B
import qualified EulerHS.Language            as L
import qualified EulerHS.Types               as T

dbTable :: B.DatabaseEntity be DB.BecknDb (B.TableEntity Storage.CommentT)
dbTable = DB._comment DB.becknDb

create :: Storage.Comment -> L.Flow ()
create Storage.Comment {..} =
  DB.createOne dbTable (Storage.insertExpression Storage.Comment {..}) >>=
  either DB.throwDBError pure

findById :: CommentId -> L.Flow (Maybe Storage.Comment)
findById id = do
  DB.findOne dbTable predicate
    >>= either DB.throwDBError pure
  where
    predicate Storage.Comment {..} = (_id ==. B.val_ id)

findAllByCommentedOnEntity :: Text -> Text -> L.Flow [Storage.Comment]
findAllByCommentedOnEntity commentedOnEntityType commentedOnEntityId =
  DB.findAll dbTable (predicate commentedOnEntityType commentedOnEntityId)
    >>= either DB.throwDBError pure
  where
    predicate commentedOnEntityType commentedOnEntityId Storage.Comment{..} =
      _commentedOnEntityType ==. B.val_ commentedOnEntityType &&.
      _CommentedOnEntityId ==. B.val_ commentedOnEntityId
