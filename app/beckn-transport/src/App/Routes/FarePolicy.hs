module App.Routes.FarePolicy where

import App.Types (FlowServer)
import Beckn.Types.ID (ID)
import Product.FarePolicy (listFarePolicies, updateFarePolicy)
import Servant
import Types.API.FarePolicy
  ( ListFarePolicyResponse,
    UpdateFarePolicyRequest,
  )
import Types.Domain.FarePolicy (FarePolicy)
import Utils.Common

type FarePolicyAPI =
  "farePolicy"
    :> ( TokenAuth :> Get '[JSON] ListFarePolicyResponse
           :<|> TokenAuth
             :> Capture "farePolicyId" (ID FarePolicy)
             :> ReqBody '[JSON] UpdateFarePolicyRequest
             :> Post '[JSON] ()
       )

farePolicyFlow :: FlowServer FarePolicyAPI
farePolicyFlow = listFarePolicies :<|> updateFarePolicy
