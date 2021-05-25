{-# LANGUAGE DuplicateRecordFields #-}

module Types.API.Call where

import qualified Beckn.External.Exotel.Types as Call
import Beckn.Types.Core.Ack
import Beckn.Types.Id
import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import qualified Types.Storage.CallStatus as SCS

newtype CallRes = CallRes
  { callId :: Id SCS.CallStatus
  }
  deriving (Generic, Eq, Show, FromJSON, ToJSON, ToSchema)

type CallCallbackReq = Call.ExotelCallCallback

type CallCallbackRes = AckResponse

type GetCallStatusRes = SCS.CallStatusAPIEntity
