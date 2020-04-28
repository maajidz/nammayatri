module Beckn.Product.Organization where

import qualified Beckn.Data.Accessor          as Accessor
import           Beckn.Types.API.Organization
import           Beckn.Types.App
import           Beckn.Types.Common
import           Data.Aeson
import           EulerHS.Prelude

createOrganization ::
  Maybe Text -> CreateOrganizationReq -> FlowHandler OrganizationRes
createOrganization regToken req = undefined

getOrganization :: Maybe Text -> Text -> FlowHandler OrganizationRes
getOrganization regToken orgId = undefined

listOrganization ::
  Maybe Text
  -> Maybe Int
  -> Maybe Int
  -> Maybe Text
  -> FlowHandler ListOrganizationRes
listOrganization regToken limitM offsetM typeM = undefined

updateOrganization ::
  Maybe Text -> Text -> UpdateOrganizationReq -> FlowHandler OrganizationRes
updateOrganization regToken orgId req = undefined
