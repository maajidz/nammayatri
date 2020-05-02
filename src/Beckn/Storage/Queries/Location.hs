{-# LANGUAGE RecordWildCards #-}

module Beckn.Storage.Queries.Location where

import           Database.Beam                ((&&.), (<-.), (==.))
import           EulerHS.Prelude              hiding (id)

import qualified Beckn.Storage.Queries        as DB
import           Beckn.Types.Common
import qualified Beckn.Types.Common           as Common
import qualified Beckn.Types.Storage.DB       as DB
import qualified Beckn.Types.Storage.DB       as DB
import qualified Beckn.Types.Storage.Location as Storage
import qualified Beckn.Types.Storage.Location as Storage
import qualified Data.Text                    as T
import qualified Database.Beam                as B
import qualified EulerHS.Language             as L
import qualified EulerHS.Types                as T

dbTable :: B.DatabaseEntity be DB.BecknDb (B.TableEntity Storage.LocationT)
dbTable = DB._location DB.becknDb

findLocation :: Text -> L.Flow (T.DBResult (Maybe Storage.Location))
findLocation id = do
  DB.findOne dbTable predicate
  where
    predicate Storage.Location {..} = (_id ==. B.val_ id)


findByLocation :: Common.LocationType -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int ->  L.Flow (Maybe Storage.Location)
findByLocation locType (Just dist) (Just city) (Just state) (Just country) (Just ward) (Just pin) = do
  DB.findOne dbTable (predicate locType dist city state country ward pin) >>=
   either DB.throwDBError pure
  where
    predicate locType dist city state country ward pin Storage.Location {..} = -- B.val_ True
        (  (_type ==. B.val_  (show $ locType))
        &&. (_district ==. B.val_ dist))

findByLocation locType _ _ _ _ _ _ = pure Nothing
findLocationWithErr :: Text -> L.Flow Storage.Location
findLocationWithErr id = do
  DB.findOneWithErr dbTable predicate
  where
    predicate Storage.Location {..} = (_id ==. B.val_ id)

findByStOrDistrict ::
  Maybe Int
  -> Maybe Int
  -> LocateBy
  -> Text
  -> L.Flow [Storage.Location]
findByStOrDistrict moffset mlimit filterByT filterBy = do
  (DB.run
     $ L.findRows
     $ B.select
     $ B.filter_ (predicate filterByT filterBy)
     $ B.offset_ offset
     $ B.limit_ limit
     $ B.orderBy_ orderByDesc
     $ B.all_ dbTable)
     >>= either DB.throwDBError pure
  where
    predicate LDISTRICT filterBy Storage.Location {..} =
      _district ==. B.val_ filterBy
    predicate LSTATE filterBy Storage.Location {..} =
      _state ==. B.val_ filterBy
    predicate LCITY filterBy Storage.Location {..} =
      _city ==. B.val_ filterBy
    predicate LWARD filterBy Storage.Location {..} =
      _ward ==. B.val_ filterBy
    predicate LPINCODE filterBy Storage.Location {..} =
      _pincode ==. B.val_ (read $ T.unpack filterBy)

    limit = (toInteger $ fromMaybe 10 mlimit)
    offset = (toInteger $ fromMaybe 0 moffset)

    orderByDesc Storage.Location {..} = B.desc_ _createdAt
