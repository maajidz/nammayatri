module Domain.Action.UI.OrgAdmin
  ( OrgAdminProfileRes (..),
    UpdateOrgAdminProfileReq (..),
    UpdateOrgAdminProfileRes,
    getProfile,
    updateProfile,
  )
where

import Beckn.External.Encryption (decrypt)
import Beckn.External.FCM.Types
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Utils.Common
import Control.Applicative
import qualified Domain.Types.Organization as Org
import qualified Domain.Types.Person as SP
import qualified Storage.Queries.Organization as QOrg
import qualified Storage.Queries.Person as QPerson
import Tools.Error

data OrgAdminProfileRes = OrgAdminProfileRes
  { id :: Id SP.Person,
    firstName :: Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    maskedMobileNumber :: Maybe Text,
    maskedDeviceToken :: Maybe FCMRecipientToken,
    organization :: Org.OrganizationAPIEntity
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data UpdateOrgAdminProfileReq = UpdateOrgAdminProfileReq
  { firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    deviceToken :: Maybe FCMRecipientToken
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

type UpdateOrgAdminProfileRes = OrgAdminProfileRes

getProfile :: (EsqDBFlow m r, EncFlow m r) => SP.Person -> m OrgAdminProfileRes
getProfile admin = do
  let Just orgId = admin.organizationId
  org <- QOrg.findById orgId >>= fromMaybeM (OrgNotFound orgId.getId)
  decAdmin <- decrypt admin
  let personAPIEntity = SP.makePersonAPIEntity decAdmin
  return $ makeOrgAdminProfileRes personAPIEntity (Org.makeOrganizationAPIEntity org)

updateProfile :: (EsqDBFlow m r, EncFlow m r) => SP.Person -> UpdateOrgAdminProfileReq -> m UpdateOrgAdminProfileRes
updateProfile admin req = do
  let Just orgId = admin.organizationId
      updAdmin =
        admin{firstName = fromMaybe admin.firstName req.firstName,
              middleName = req.middleName <|> admin.middleName,
              lastName = req.lastName <|> admin.lastName,
              deviceToken = req.deviceToken <|> admin.deviceToken
             }
  Esq.runTransaction $
    QPerson.updatePersonRec updAdmin.id updAdmin
  org <- QOrg.findById orgId >>= fromMaybeM (OrgNotFound orgId.getId)
  decUpdAdmin <- decrypt updAdmin
  let personAPIEntity = SP.makePersonAPIEntity decUpdAdmin
  return $ makeOrgAdminProfileRes personAPIEntity (Org.makeOrganizationAPIEntity org)

makeOrgAdminProfileRes :: SP.PersonAPIEntity -> Org.OrganizationAPIEntity -> OrgAdminProfileRes
makeOrgAdminProfileRes SP.PersonAPIEntity {..} org =
  OrgAdminProfileRes
    { organization = org,
      ..
    }
