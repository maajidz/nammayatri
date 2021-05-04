{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.API.Update
  ( module Beckn.Types.Core.API.Update,
    module Beckn.Types.Core.API.Callback,
  )
where

import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.Context
import Beckn.Types.Mobility.Order
import EulerHS.Prelude

data UpdateReq = UpdateReq
  { context :: Context,
    message :: UpdateReq
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data UpdateOrder = UpdateOrder
  { update_action :: Maybe String, -- UPDATE-PICKUP | UPDATE-DROP | ADD-STOP
    service :: Maybe UpdateOrderService,
    order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)

newtype UpdateOrderService = UpdateOrderService
  { id :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type UpdateRes = AckResponse

type OnUpdateReq = CallbackReq OnUpdateOrder

newtype OnUpdateOrder = OnUpdateOrder
  { order :: Order
  }
  deriving (Generic, Show, ToJSON, FromJSON)

type OnUpdateRes = AckResponse
