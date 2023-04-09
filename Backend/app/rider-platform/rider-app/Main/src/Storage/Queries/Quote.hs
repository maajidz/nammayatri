{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Quote where

import Data.Tuple.Extra
import Domain.Types.DriverOffer
import Domain.Types.Estimate
import Domain.Types.FarePolicy.FareProductType
import Domain.Types.Quote
import Domain.Types.SearchRequest
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Queries.FullEntityBuilders (buildFullQuote)
import Storage.Tabular.DriverOffer as DriverOffer
import Storage.Tabular.Quote
import Storage.Tabular.Quote.Instances
import Storage.Tabular.RentalSlab
import Storage.Tabular.SpecialZoneQuote
import Storage.Tabular.TripTerms

createDetails :: QuoteDetailsT -> FullEntitySqlDB ()
createDetails = \case
  OneWayDetailsT -> pure ()
  RentalDetailsT rentalSlabT -> do
    Esq.create' rentalSlabT
  DriverOfferDetailsT driverOfferT -> do
    Esq.create' driverOfferT
  OneWaySpecialZoneDetailsT specialZoneQuoteT -> do
    Esq.create' specialZoneQuoteT

-- order of creating entites make sense!
create :: Quote -> SqlDB ()
create quote =
  Esq.withFullEntity quote $ \(quoteT, mbTripTermsT, quoteDetailsT) -> do
    traverse_ Esq.create' mbTripTermsT
    createDetails quoteDetailsT
    Esq.create' quoteT

createMany :: [Quote] -> SqlDB ()
createMany quotes =
  Esq.withFullEntities quotes $ \list -> do
    let quoteTs = map fst3 list
        mbTripTermsTs = mapMaybe snd3 list
        quoteDetailsTs = map thd3 list
    Esq.createMany' mbTripTermsTs
    traverse_ createDetails quoteDetailsTs
    Esq.createMany' quoteTs

fullQuoteTable ::
  From
    ( Table QuoteT
        :& MbTable TripTermsT
        :& MbTable RentalSlabT
        :& MbTable DriverOffer.DriverOfferT
        :& MbTable SpecialZoneQuoteT
    )
fullQuoteTable =
  table @QuoteT
    `leftJoin` table @TripTermsT
      `Esq.on` ( \(quote :& mbTripTerms) ->
                   quote ^. QuoteTripTermsId ==. mbTripTerms ?. TripTermsTId
               )
    `leftJoin` table @RentalSlabT
      `Esq.on` ( \(quote :& _ :& mbRentalSlab) ->
                   quote ^. QuoteRentalSlabId ==. mbRentalSlab ?. RentalSlabTId
               )
    `leftJoin` table @DriverOfferT
      `Esq.on` ( \(quote :& _ :& _ :& mbDriverOffer) ->
                   quote ^. QuoteDriverOfferId ==. mbDriverOffer ?. DriverOfferTId
               )
    `leftJoin` table @SpecialZoneQuoteT
      `Esq.on` ( \(quote :& _ :& _ :& _ :& mbspecialZoneQuote) ->
                   quote ^. QuoteSpecialZoneQuoteId ==. mbspecialZoneQuote ?. SpecialZoneQuoteTId
               )

findAllByRequestId :: Transactionable m => Id SearchRequest -> m [Quote]
findAllByRequestId searchRequestId = Esq.buildDType $ do
  fullQuoteTs <- Esq.findAll' $ do
    (quote :& mbTripTerms :& mbRentalSlab :& mbDriverOffer :& mbspecialZoneQuote) <- from fullQuoteTable
    where_ $ quote ^. QuoteRequestId ==. val (toKey searchRequestId)
    pure (quote, mbTripTerms, mbRentalSlab, mbDriverOffer, mbspecialZoneQuote)
  catMaybes <$> mapM buildFullQuote fullQuoteTs

findAllByEstimateId' :: Transactionable m => Id Estimate -> m [Quote]
findAllByEstimateId' estimateId = buildDType $ do
  driverOfferTs <- findDOfferByEstimateId' estimateId
  (catMaybes <$>) $ mapM buildFullQuote' driverOfferTs

findDOfferByEstimateId' :: Transactionable m => Id Estimate -> DTypeBuilder m [DriverOfferT]
findDOfferByEstimateId' estimateId =
  Esq.findAll' $ do
    driverOffer <- from $ table @DriverOfferT
    where_ $ driverOffer ^. DriverOfferEstimateId ==. val (toKey estimateId)
    return driverOffer

findQuotesByDriverOfferId' :: Transactionable m => Id DriverOffer -> DTypeBuilder m (Maybe QuoteT)
findQuotesByDriverOfferId' driverOfferId = Esq.findOne' $ do
  quote <- from $ table @QuoteT
  where_ $
    quote ^. QuoteDriverOfferId ==. just (val (toKey driverOfferId))
  return quote

buildFullQuote' :: Transactionable m => DriverOfferT -> DTypeBuilder m (Maybe (SolidType FullQuoteT))
buildFullQuote' driverOfferT = runMaybeT $ do
  quoteT <- MaybeT $ findQuotesByDriverOfferId' (Id $ DriverOffer.id driverOfferT)
  quoteDetailsT <- case fareProductType quoteT of
    ONE_WAY -> pure OneWayDetailsT
    RENTAL -> do
      rentalSlabId <- MaybeT $ pure (fromKey <$> rentalSlabId quoteT)
      rentalSlab <- MaybeT $ Esq.findById' @RentalSlabT rentalSlabId
      pure $ RentalDetailsT rentalSlab
    DRIVER_OFFER -> do
      pure (DriverOfferDetailsT driverOfferT)
    ONE_WAY_SPECIAL_ZONE -> do
      specialZoneQuoteId <- MaybeT $ pure (fromKey <$> specialZoneQuoteId quoteT)
      specialZoneQuoteT <- MaybeT $ Esq.findById' @SpecialZoneQuoteT specialZoneQuoteId
      pure (OneWaySpecialZoneDetailsT specialZoneQuoteT)
  mbTripTermsT <- forM (fromKey <$> tripTermsId quoteT) $ \tripTermsId -> do
    MaybeT $ Esq.findById' @TripTermsT tripTermsId
  return $ extractSolidType @Quote (quoteT, mbTripTermsT, quoteDetailsT)

-- queries fetching only one entity should avoid join for performance reason

findById :: Transactionable m => Id Quote -> m (Maybe Quote)
findById quoteId = Esq.buildDType . runMaybeT $ do
  quote <- findByIdM @QuoteT (toKey quoteId)
  quoteDetails <- case fareProductType quote of
    ONE_WAY -> pure OneWayDetailsT
    RENTAL -> do
      rentalSlabTId <- hoistMaybe (quote & rentalSlabId)
      rentalSlab <- Esq.findByIdM @RentalSlabT rentalSlabTId
      pure $ RentalDetailsT rentalSlab
    DRIVER_OFFER -> do
      driverOfferTId <- hoistMaybe (quote & driverOfferId)
      driverOffer <- Esq.findByIdM @DriverOfferT driverOfferTId
      pure (DriverOfferDetailsT driverOffer)
    ONE_WAY_SPECIAL_ZONE -> do
      specialZoneQuoteTId <- hoistMaybe (quote & specialZoneQuoteId)
      specialZoneQuoteT <- Esq.findByIdM @SpecialZoneQuoteT specialZoneQuoteTId
      pure (OneWaySpecialZoneDetailsT specialZoneQuoteT)
  mbTripTerms <- forM (quote & tripTermsId) $ Esq.findByIdM @TripTermsT
  return $ extractSolidType @Quote (quote, mbTripTerms, quoteDetails)

findByBppIdAndBPPQuoteId :: Transactionable m => Text -> Id BPPQuote -> m (Maybe (Id Quote))
findByBppIdAndBPPQuoteId bppId bppQuoteId = Esq.findOne $ do
  quote :& driverOffer <-
    from $
      table @QuoteT
        `innerJoin` table @DriverOfferT
          `Esq.on` ( \(quote :& driverOffer) ->
                       quote ^. QuoteDriverOfferId ==. just (driverOffer ^. DriverOfferTId)
                   )
  where_ $
    quote ^. QuoteProviderId ==. val bppId
      &&. driverOffer ^. DriverOfferBppQuoteId ==. (val bppQuoteId.getId)
  pure $ quote ^. QuoteTId
