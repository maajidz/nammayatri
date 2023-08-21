{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.Beckn.Search (API, handler) where

import qualified Beckn.ACL.OnSearch as ACL
import qualified Beckn.ACL.Search as ACL
import qualified Beckn.Core as CallBAP
import qualified Beckn.Types.Core.Taxi.API.OnSearch as OnSearch
import qualified Beckn.Types.Core.Taxi.API.Search as Search
import qualified Domain.Action.Beckn.Search as DSearch
import qualified Domain.Types.Merchant as DM
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
-- import Kernel.Types.Beckn.Ack
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth
import Servant

type API =
  Capture "merchantId" (Id DM.Merchant)
    :> SignatureAuth "Authorization"
    :> SignatureAuth "X-Gateway-Authorization"
    :> Search.SearchAPI

handler :: FlowServer API
handler = search

search ::
  Id DM.Merchant ->
  SignatureAuthResult ->
  SignatureAuthResult ->
  Search.SearchReq ->
  FlowHandler BecknAPIResponse
search transporterId (SignatureAuthResult _ subscriber) (SignatureAuthResult _ gateway) req =
  withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
    now <- getCurrentTime
    isExp <- maybe (pure False) (\ttl -> isTtlExpired (Just ttl) req.context.timestamp now) req.context.ttl
    if isExp
      then getTtlExpiredRes
      else do
        logTagInfo "Search API Flow" "Reached"
        dSearchReq <- ACL.buildSearchReq subscriber req
        Redis.whenWithLockRedis (searchLockKey dSearchReq.messageId transporterId.getId) 60 $ do
          merchant <- DSearch.validateRequest transporterId dSearchReq
          fork "search request processing" $
            Redis.whenWithLockRedis (searchProcessingLockKey dSearchReq.messageId transporterId.getId) 60 $ do
              dSearchRes <- DSearch.handler merchant dSearchReq
              let context = req.context
              let callbackUrl = gateway.subscriber_url
              void $
                CallBAP.withCallback dSearchRes.provider Context.SEARCH OnSearch.onSearchAPI context callbackUrl $ do
                  pure $ ACL.mkOnSearchMessage dSearchRes
        pure getSuccessRes

searchLockKey :: Text -> Text -> Text
searchLockKey id mId = "Driver:Search:MessageId-" <> id <> ":" <> mId

searchProcessingLockKey :: Text -> Text -> Text
searchProcessingLockKey id mId = "Driver:Search:Processing:MessageId-" <> id <> ":" <> mId
