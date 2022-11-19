module Domain.Action.UI.Transporter
  ( TransporterRec (..),
    UpdateTransporterReq (..),
    UpdateTransporterRes,
    updateTransporter,
    getTransporter,
  )
where

import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Hedis
import Beckn.Types.Id (Id (..))
import Beckn.Types.Predicate
import Beckn.Utils.Common
import qualified Beckn.Utils.Predicates as P
import Beckn.Utils.Validation
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as SP
import EulerHS.Prelude hiding (id)
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Person as QP
import Tools.Error

newtype TransporterRec = TransporterRec
  { organization :: DM.MerchantAPIEntity
  }
  deriving (Generic, ToJSON, ToSchema)

data UpdateTransporterReq = UpdateTransporterReq
  { name :: Maybe Text,
    description :: Maybe Text,
    enabled :: Maybe Bool
  }
  deriving (Generic, Show, FromJSON, ToSchema)

type UpdateTransporterRes = DM.MerchantAPIEntity

validateUpdateTransporterReq :: Validate UpdateTransporterReq
validateUpdateTransporterReq UpdateTransporterReq {..} =
  sequenceA_
    [ validateField "name" name $ InMaybe $ MinLength 3 `And` P.name,
      validateField "description" description $ InMaybe $ MinLength 3 `And` P.name
    ]

updateTransporter :: (HasCacheConfig r, HedisFlow m r, EsqDBFlow m r) => SP.Person -> Id DM.Merchant -> UpdateTransporterReq -> m UpdateTransporterRes
updateTransporter admin merchantId req = do
  unless (Just merchantId == admin.merchantId) $ throwError AccessDenied
  runRequestValidation validateUpdateTransporterReq req
  merchant <-
    CQM.findById merchantId
      >>= fromMaybeM (MerchantDoesNotExist merchantId.getId)
  let updMerchant =
        merchant{DM.name = fromMaybe (merchant.name) (req.name),
                 DM.description = (req.description) <|> (merchant.description),
                 DM.enabled = fromMaybe (merchant.enabled) (req.enabled)
                }
  Esq.runTransaction $ CQM.update updMerchant
  CQM.clearCache updMerchant
  logTagInfo ("merchantAdmin-" <> getId admin.id <> " -> updateTransporter : ") (show updMerchant)
  return $ DM.makeMerchantAPIEntity updMerchant

getTransporter :: (HasCacheConfig r, HedisFlow m r, EsqDBFlow m r) => Id SP.Person -> m TransporterRec
getTransporter personId = do
  person <-
    QP.findById personId
      >>= fromMaybeM (PersonNotFound personId.getId)
  case person.merchantId of
    Just merchantId -> TransporterRec . DM.makeMerchantAPIEntity <$> (CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId))
    Nothing -> throwError (PersonFieldNotPresent "merchant_id")
