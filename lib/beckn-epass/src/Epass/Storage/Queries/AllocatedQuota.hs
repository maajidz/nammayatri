{-# LANGUAGE RecordWildCards #-}

module Epass.Storage.Queries.AllocatedQuota where

import Database.Beam ((&&.), (<-.), (==.))
import qualified Database.Beam as B
import qualified Epass.Storage.Queries as DB
import qualified Epass.Types.Storage.AllocatedQuota as Storage
import qualified Epass.Types.Storage.DB as DB
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types as T

dbTable ::
  B.DatabaseEntity be DB.EpassDb (B.TableEntity Storage.AllocatedQuotaT)
dbTable = DB._allocatedQuota DB.becknDb

findAllocatedQuota :: Text -> L.Flow (T.DBResult (Maybe Storage.AllocatedQuota))
findAllocatedQuota id = do
  DB.findOne dbTable predicate
  where
    predicate Storage.AllocatedQuota {..} = (_id ==. B.val_ id)
