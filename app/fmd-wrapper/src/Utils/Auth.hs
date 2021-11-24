module Utils.Auth where

import Beckn.Storage.DB.Config
import Beckn.Utils.Common
import Beckn.Utils.Servant.HeaderAuth
import Control.Monad ()
import EulerHS.Prelude
import qualified Storage.Queries.Organization as Org
import Types.Error
import Types.Storage.Organization (Organization)

-- | TODO: Perform some API key verification.
data VerifyAPIKey = VerifyAPIKey

instance VerificationMethod VerifyAPIKey where
  type VerificationResult VerifyAPIKey = Organization
  verificationDescription =
    "Checks whether app/gateway is registered.\
    \If you don't have an API key, register the app/gateway."

verifyApiKey :: DBFlow m r => VerificationAction VerifyAPIKey m
verifyApiKey = VerificationAction (Org.findOrgByApiKey >=> fromMaybeM OrgNotFound)
