module Beckn.Types.Core.Cabs.API.OnUpdate where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.Cabs.OnUpdate (OnUpdateMessage)
import Beckn.Types.Core.ReqTypes (BecknCallbackReq)
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type OnUpdateReq = BecknCallbackReq OnUpdateMessage

type OnUpdateAPI =
  "on_update"
    :> ReqBody '[JSON] OnUpdateReq
    :> Post '[JSON] AckResponse

onUpdateAPI :: Proxy OnUpdateAPI
onUpdateAPI = Proxy
