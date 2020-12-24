{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.FMD.API.Status where

import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Ack (AckResponse (..))
import Beckn.Types.Core.Context
import Beckn.Types.FMD.Order
import Data.Generics.Labels ()
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type StatusAPI =
  "status"
    :> ReqBody '[JSON] StatusReq
    :> Post '[JSON] StatusRes

statusAPI :: Proxy StatusAPI
statusAPI = Proxy

type OnStatusAPI =
  "on_status"
    :> ReqBody '[JSON] OnStatusReq
    :> Post '[JSON] OnStatusRes

onStatusAPI :: Proxy OnStatusAPI
onStatusAPI = Proxy

data StatusReq = StatusReq
  { context :: Context,
    message :: StatusReqMessage
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type StatusRes = AckResponse

type OnStatusReq = CallbackReq StatusResMessage

newtype StatusReqMessage = StatusReqMessage
  { order_id :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type OnStatusRes = AckResponse

newtype StatusResMessage = StatusResMessage
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)
