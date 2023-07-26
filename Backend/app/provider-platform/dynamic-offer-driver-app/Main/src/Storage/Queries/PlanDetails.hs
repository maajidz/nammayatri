{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.PlanDetails where

import Domain.Types.Merchant
import Domain.Types.PlanDetails
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.PlanDetails

create :: PlanDetails -> SqlDB ()
create = Esq.create

findByPlanIdAndPlanType :: Transactionable m => Id PlanDetails -> PaymentType -> m (Maybe PlanDetails)
findByPlanIdAndPlanType planId paymentType = Esq.findOne $ do
  planDetails <- from $ table @PlanDetailsT
  where_ $
    planDetails ^. PlanDetailsTId ==. val (toKey (planId, paymentType))
  return planDetails

findByMerchantId :: Transactionable m => Id Merchant -> m [PlanDetails]
findByMerchantId merchantId = do
  findAll $ do
    planDetails <- from $ table @PlanDetailsT
    where_ $
      planDetails ^. PlanDetailsMerchantId ==. val (toKey merchantId)
    return planDetails
