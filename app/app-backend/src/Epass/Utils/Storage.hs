module Epass.Utils.Storage where

import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common
import Beckn.Utils.Extra
import qualified Data.Time as DT
import Data.Time.Clock
import Data.Time.LocalTime
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified EulerHS.Types as T
import Servant
import qualified Storage.Queries.RegistrationToken as QR

data AppException
  = SqlDBConnectionFailedException Text
  | KVDBConnectionFailedException Text
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, Exception)

throwOnFailedWithLog ::
  Show e => Either e a -> (Text -> AppException) -> Text -> L.Flow ()
throwOnFailedWithLog (Left err) mkException msg = do
  L.logError ("" :: Text) $ msg <> " " <> show err <> ""
  L.throwException $ mkException $ msg <> " " <> show err <> ""
throwOnFailedWithLog _ _ _ = pure ()

throwFailedWithLog :: (Text -> AppException) -> Text -> L.Flow ()
throwFailedWithLog mkException msg = do
  L.logError ("" :: Text) $ msg <> ""
  L.throwException $ mkException $ msg <> ""

verifyToken :: Maybe Text -> L.Flow SR.RegistrationToken
verifyToken (Just token) =
  QR.findByToken token
    >>= fromMaybeM400 "INVALID_TOKEN"
    >>= validateToken
verifyToken _ = L.throwException $ err400 {errBody = "NO_TOKEN_FOUND"}

validateToken :: SR.RegistrationToken -> L.Flow SR.RegistrationToken
validateToken sr@SR.RegistrationToken {..} = do
  let nominal = realToFrac $ _tokenExpiry * 24 * 60 * 60
  expired <- isExpired nominal _updatedAt
  when expired (L.throwException $ err400 {errBody = "TOKEN_EXPIRED"})
  return sr
