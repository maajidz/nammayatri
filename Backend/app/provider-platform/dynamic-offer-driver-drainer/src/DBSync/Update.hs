module DBSync.Update where

import Config.Config as Config
import Config.Env
import Data.Aeson as A
import Data.Either.Extra (mapLeft)
import Data.Text as T
import Database.Beam as B hiding (runUpdate)
import EulerHS.CachedSqlDBQuery as CDB
import EulerHS.KVConnector.Types
import EulerHS.KVConnector.Utils as EKU
import qualified EulerHS.Language as EL
import EulerHS.Prelude hiding (id)
import EulerHS.Types as ET
-- import           Utils.Logging

import Sequelize (Model, Set, Where)
import System.CPUTime
import Types.DBSync
import Types.Event as Event
import Utils.Utils

updateDB ::
  forall beM be table m.
  ( HasCallStack,
    ET.BeamRuntime be beM,
    ET.BeamRunner beM,
    Model be table,
    MeshMeta be table,
    B.HasQBuilder be,
    EL.MonadFlow m,
    ToJSON (table Identity),
    FromJSON (table Identity)
  ) =>
  ET.DBConfig beM ->
  Maybe Text ->
  [Set be table] ->
  Where be table ->
  ByteString ->
  m (Either MeshError [A.Value])
updateDB dbConf _ setClause whereClause bts = do
  either (pure . Left) ((Right <$>) . (sequence . (updateModel' <$>))) . mapLeft MDBError
    =<< runExceptT
      ( do
          updateObj <- ExceptT $ CDB.findAll dbConf Nothing whereClause
          ExceptT $ CDB.updateOneWoReturning dbConf Nothing setClause whereClause
          pure updateObj
      )
  where
    updateModel' model = do
      let val = (EKU.updateModel @be @table) model (EKU.jsonKeyValueUpdates setClause)
      case val of
        Right obj -> pure obj
        Left err -> do
          EL.logError (("Model Update failed: " :: Text) <> T.pack (show err)) (show [("command" :: String, bts)] :: Text)
          pure A.Null

runUpdateCommands :: (UpdateDBCommand, ByteString) -> Flow (Either (MeshError, EL.KVDBStreamEntryID) EL.KVDBStreamEntryID)
runUpdateCommands (cmd, val) = do
  case cmd of
    -- UpdateDBCommand id _ _ _ _ (TxnOfferInfoOptions                  _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("TxnOfferInfo"                  :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (JuspayEventOptions                   _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("JuspayEvent"                   :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (HdfcHashedNumOptions                 _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("HdfcHashedNum"                 :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (MerchantGatewayAccountOptions        _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("MerchantGatewayAccount"        :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (MerchantGatewayAccountSubInfoOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("MerchantGatewayAccountSubInfo" :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (OrderReferenceOptions                _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("OrderReference"                :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (MandateOptions                       _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("Mandate"                       :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (GatewayTxnDataOptions                _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("GatewayTxnData"                :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (MerchantGatewayCardInfoOptions       _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("MerchantGatewayCardInfo"       :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (TxnRiskCheckOptions                  _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("TxnRiskCheck"                  :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (PaymentMethodOptions                 _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("PaymentMethod"                 :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (OfferBenefitInfoOptions              _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("OfferBenefitInfo"              :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (MerchantIframePreferencesOptions     _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("MerchantIframePreferences"     :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (ResellerAccountOptions               _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("ResellerAccount"               :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (UnifiedGatewayResponseOptions        _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("UnifiedGatewayResponse"        :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (WalletTopUpTxnOptions                _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("WalletTopUpTxn"                :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (IsinRoutesOptions                    _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("IsinRoutes"                    :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (TxnCardInfoOptions                   _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("TxnCardInfo"                   :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (OrderAddressOptions                  _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("OrderAddress"                  :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (LockerAccountOptions                 _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("LockerAccount"                 :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (JuspayBankCodeOptions                _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("JuspayBankCode"                :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (IssuerRoutesOptions                  _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("IssuerRoutes"                  :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (RefundOptions                        _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("Refund"                        :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (SecondFactorOptions                  _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("SecondFactor"                  :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (GatewayOutageOptions                 _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("GatewayOutage"                 :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (TxnDetailOptions                     _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("TxnDetail"                     :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (EmiPlanOptions                       _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("EmiPlan"                       :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (MerchantKeyOptions                   _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("MerchantKey"                   :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (NetworkCardFingerprintOptions        _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("NetworkCardFingerprint"        :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (TokenRequestorOptions                _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("TokenRequestor"                :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (TxnOfferDetailOptions                _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("TxnOfferDetail"                :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (SecondFactorResponseOptions          _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("SecondFactorResponse"          :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (CardBrandRoutesOptions               _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("CardBrandRoutes"               :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (CustomerOptions                      _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("Customer"                      :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (MerchantAccountOptions               _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("MerchantAccount"               :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (TokenBinInfoOptions                  _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("TokenBinInfo"                  :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (MerchantGatewayPaymentMethodOptions  _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("MerchantGatewayPaymentMethod"  :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (PromotionOptions                     _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("Promotion"                     :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (LockerTokenRequestorOptions          _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("LockerTokenRequestor"          :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (BankAccountOptions                   _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("BankAccount"                   :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (ProviderOptions                      _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("Provider"                      :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (GatewayCardInfoOptions               _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("GatewayCardInfo"               :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (PaymentGatewayResponseOptions        _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("PaymentGatewayResponse"        :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (MetadataOptions                      _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("Metadata"                      :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (ChargebackOptions                    _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("Chargeback"                    :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (WalletAccountOptions                 _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("WalletAccount"                 :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (GatewayStatusMapOptions              _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("GatewayStatusMap"              :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (TokenOptions                         _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("Token"                         :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (MerchantLockerAccountOptions         _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("MerchantLockerAccount"         :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (TempCardOptions                      _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("TempCard"                      :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (MerchantRiskSettingsOptions          _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("MerchantRiskSettings"          :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (UserOptions                          _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("User"                          :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (CofDetailsOptions                    _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("CofDetails"                    :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (OrderMetadataV2Options               _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("OrderMetadataV2"               :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (StoredCardOptions                    _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("StoredCard"                    :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (TokenCustomerOptions                 _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("TokenCustomer"                 :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (EnrolledPanOptions                   _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("EnrolledPan"                   :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (RoleOptions                          _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("Role"                          :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (FeatureOptions                       _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("Feature"                       :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (GatewayBankEmiSupportOptions         _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("GatewayBankEmiSupport"         :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (AuthenticationAccountOptions         _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("AuthenticationAccount"         :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (PaymentGatewayResponseV1Options      _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("PaymentGatewayResponseV1"      :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (MerchantProviderDetailsOptions       _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("MerchantProviderDetails"       :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (TxnOfferOptions                      _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("TxnOffer"                      :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (GatewayHealthOptions                 _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("GatewayHealth"                 :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (RiskManagementAccountOptions         _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("RiskManagementAccount"         :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (CardInfoOptions                      _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("CardInfo"                      :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (DeviceBindingOptions                 _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("DeviceBinding"                 :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (NotificationOptions                  _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("Notification"                  :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (OrderBasketOptions                   _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("OrderBasket"                   :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (GatewayPaymentMethodOptions          _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("GatewayPaymentMethod"          :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (PaymentLinksOptions                  _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("PaymentLinks"                  :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (CustomerAccountOptions               _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("CustomerAccount"               :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (AuthMappingOptions                   _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("AuthMapping"                   :: Text) =<< Config.getEulerDbConf
    -- UpdateDBCommand id _ _ _ _ (RuleOptions                          _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("Rule"                          :: Text) =<< Config.getEulerPgDbConf
    -- UpdateDBCommand id _ _ _ _ (OfferRedemptionOptions               _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("OfferRedemption"               :: Text) =<< Config.getEulerPgDbConf
    -- UpdateDBCommand id _ _ _ _ (ExternalMerchantCustomerOptions      _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("ExternalMerchantCustomer"      :: Text) =<< Config.getEulerPgDbConf
    -- UpdateDBCommand id _ _ _ _ (AgencyOptions                        _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("Agency"                        :: Text) =<< Config.getEulerPgDbConf
    -- UpdateDBCommand id _ _ _ _ (OffersOptions                        _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("Offers"                        :: Text) =<< Config.getEulerPgDbConf
    -- UpdateDBCommand id _ _ _ _ (SavedPaymentMethodOptions            _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("SavedPaymentMethod"            :: Text) =<< Config.getEulerPgDbConf
    UpdateDBCommand id _ _ _ _ (RegistrationTokenOptions _ setClauses whereClause) -> runUpdate id val setClauses whereClause ("RegistrationToken" :: Text) =<< Config.getEulerPgDbConf
  where
    runUpdate id value setClause whereClause model dbConf = do
      maxRetries <- EL.runIO getMaxRetries
      runUpdateWithRetries id value setClause whereClause model dbConf 0 maxRetries

    runUpdateWithRetries id value setClause whereClause model dbConf retryIndex maxRetries = do
      --   t1    <- EL.getCurrentDateInMillis
      --   cpuT1 <- EL.runIO getCPUTime
      res <- updateDB dbConf Nothing setClause whereClause value
      --   t2    <- EL.getCurrentDateInMillis
      --   cpuT2 <- EL.runIO getCPUTime
      case (res, retryIndex) of
        (Left _, y) | y < maxRetries -> do
          void $ publishDBSyncMetric $ Event.QueryExecutionFailure "Update" model
          EL.runIO $ delay =<< getRetryDelay
          runUpdateWithRetries id value setClause whereClause model dbConf (retryIndex + 1) maxRetries
        (Left x, _) -> do
          void $ publishDBSyncMetric $ Event.QueryExecutionFailure "Update" model
          EL.logError (("Update failed: " :: Text) <> T.pack (show x)) (show [("command" :: String, value)] :: Text)
          pure $ Left (x, id)
        (Right _, _) -> do
          --   EL.logInfoV ("Drainer Info" :: Text) $ createDBLogEntry model "UPDATE" (t2 -t1) (cpuT2 - cpuT1) rVals
          pure $ Right id
