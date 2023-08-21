{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.Update (API, handler) where

import qualified Beckn.ACL.Update as ACL
import qualified Beckn.Types.Core.Taxi.API.Update as Update
import qualified Domain.Action.Beckn.Update as DUpdate
import Domain.Types.Merchant (Merchant)
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Hedis as Redis
-- import Kernel.Types.Beckn.Ack
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Servant

type API =
  Capture "merchantId" (Id Merchant)
    :> SignatureAuth "Authorization"
    :> Update.UpdateAPI

handler :: FlowServer API
handler = update

update ::
  Id Merchant ->
  SignatureAuthResult ->
  Update.UpdateReq ->
  FlowHandler BecknAPIResponse
update _ (SignatureAuthResult _ subscriber) req = withFlowHandlerBecknAPI $
  withTransactionIdLogTag req $ do
    -- now <- getCurrentTime
    -- isExp <- maybe (pure False) (\ttl -> isTtlExpired (Just ttl) req.context.timestamp now) req.context.ttl
    -- if isExp then getTtlExpiredRes
    -- else do -- req is diff. no need here - check prash
    logTagInfo "updateAPI" "Received update API call."
    dUpdateReq <- ACL.buildUpdateReq subscriber req
    Redis.whenWithLockRedis (updateLockKey dUpdateReq.bookingId.getId) 60 $ do
      fork "update request processing" $
        Redis.whenWithLockRedis (updateProcessingLockKey dUpdateReq.bookingId.getId) 60 $
          DUpdate.handler dUpdateReq
    pure getSuccessRes

updateLockKey :: Text -> Text
updateLockKey id = "Driver:Update:BookingId-" <> id

updateProcessingLockKey :: Text -> Text
updateProcessingLockKey id = "Driver:Update:Processing:BookingId-" <> id
