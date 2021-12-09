module Beckn.Types.Core.Cabs.API.OnCancel where

import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.Cabs.OnCancel
import Beckn.Types.Core.ReqTypes (BecknCallbackReq)
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type OnCancelReq = BecknCallbackReq OnCancelMessage

type OnCancelAPI =
  "on_cancel"
    :> ReqBody '[JSON] OnCancelReq
    :> Post '[JSON] AckResponse

onCancelAPI :: Proxy OnCancelAPI
onCancelAPI = Proxy
