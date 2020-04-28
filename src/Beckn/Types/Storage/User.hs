{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Beckn.Types.Storage.User where

import qualified Beckn.Utils.Defaults      as Defaults
import           Data.Aeson
import           Data.Default
import qualified Data.Text                 as T
import           Data.Time
import qualified Database.Beam             as B
import           Database.Beam.Backend.SQL
import           Database.Beam.MySQL
import           EulerHS.Prelude

data Status = ACTIVE | INACTIVE
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Status where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow MySQL Status where
  fromBackendRow = read . T.unpack <$> fromBackendRow

data Role = ADMIN | MANAGER | VIEWER
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Role where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow MySQL Role where
  fromBackendRow = read . T.unpack <$> fromBackendRow


data UserT f =
  User
    { _id             :: B.C f Text
    , _organizationId :: B.C f Text
    , _name           :: B.C f Text
    , _username       :: B.C f Text
    , _password       :: B.C f Text
    , _email          :: B.C f Text
    , _role           :: B.C f Role
    , _verified       :: B.C f Bool
    , _status         :: B.C f Status
    , _info           :: B.C f (Maybe Text)
    , _createdAt      :: B.C f LocalTime
    , _updatedAt      :: B.C f LocalTime
    }
  deriving (Generic, B.Beamable)

type User = UserT Identity

type UserPrimaryKey = B.PrimaryKey UserT Identity

instance B.Table UserT where
  data PrimaryKey UserT f = UserPrimaryKey (B.C f Text)
                               deriving (Generic, B.Beamable)
  primaryKey = UserPrimaryKey . _id

instance Default User where
  def = User
    { _id             = Defaults.id
    , _organizationId = Defaults.orgId
    , _name           = Defaults.user
    , _username       = Defaults.user
    , _password       = ""
    , _email          = Defaults.email
    , _role           = VIEWER
    , _verified       = False
    , _status         = ACTIVE
    , _info           = Nothing
    , _createdAt      = Defaults.localTime
    , _updatedAt      = Defaults.localTime
    }

deriving instance Show User

deriving instance Eq User

instance ToJSON User where
  toJSON = genericToJSON stripLensPrefixOptions

deriving instance FromJSON User

fieldEMod ::
     B.EntityModification (B.DatabaseEntity be db) be (B.TableEntity UserT)
fieldEMod =
  B.modifyTableFields
    B.tableModification
      { _createdAt = "created_at"
      , _updatedAt = "updated_at"
      , _organizationId = "organization_id"
      }


