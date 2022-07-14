module Product.BecknProvider.Confirm (confirm) where

import Beckn.Prelude
import Beckn.Types.Core.Ack
import qualified Beckn.Types.Core.Context as Context
import qualified Beckn.Types.Core.Taxi.API.Confirm as Confirm
import Beckn.Types.Core.Taxi.API.OnConfirm as OnConfirm
import Beckn.Types.Id
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Core.ACL.Confirm as ACL
import qualified Core.ACL.OnConfirm as ACL
import qualified Domain.Action.Beckn.Confirm as DConfirm
import qualified Domain.Types.Organization as Org
import Environment
import ExternalAPI.Flow as ExternalAPI
import qualified Product.BecknProvider.BP as BP
import Utils.Common

confirm ::
  Id Org.Organization ->
  SignatureAuthResult ->
  Confirm.ConfirmReq ->
  FlowHandler AckResponse
confirm transporterId (SignatureAuthResult _ subscriber) req =
  withFlowHandlerAPI . withTransactionIdLogTag req $ do
    -- log beckn request
    dConfirmReq <- ACL.buildConfirmReq req
    let context = req.context
    dConfirmRes <- DConfirm.handler subscriber transporterId dConfirmReq
    now <- getCurrentTime
    fork "on_confirm/on_update" $ do
      liftIO $ threadDelaySec 2
      _ <-
        ExternalAPI.withCallback dConfirmRes.transporter Context.CONFIRM OnConfirm.onConfirmAPI context context.bap_uri $
          -- there should be DOnConfirm.onConfirm, but it is empty anyway

          pure $ ACL.mkOnConfirmMessage now dConfirmRes
      BP.sendRideAssignedUpdateToBAP dConfirmRes.booking dConfirmRes.ride
    pure Ack
