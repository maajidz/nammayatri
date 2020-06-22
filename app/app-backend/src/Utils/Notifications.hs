{-# LANGUAGE OverloadedLabels #-}

module Utils.Notifications where

import Beckn.External.FCM.Flow
import Beckn.External.FCM.Types as FCM
import Beckn.Types.API.Track
import Beckn.Types.App
import Beckn.Types.Core.Person (full_name)
import Beckn.Types.Mobility.Driver as Driver
import Beckn.Types.Mobility.Tracking
import Beckn.Types.Mobility.Trip
import Beckn.Types.Mobility.Vehicle as Vehicle
import Beckn.Types.Storage.Case as Case
import Beckn.Types.Storage.CaseProduct as CaseProduct
import Beckn.Types.Storage.Person as Person hiding (full_name)
import Beckn.Types.Storage.Products as Products
import Beckn.Types.Storage.RegistrationToken as RegToken
import Control.Lens.Prism (_Just)
import qualified Data.Text as T
import Data.Time
import Data.Time.LocalTime
import qualified EulerHS.Language as L
import EulerHS.Prelude
import qualified Storage.Queries.Case as Case
import qualified Storage.Queries.CaseProduct as CaseProduct
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Products as Products

-- @boazjohn:
-- When customer searches case is created in the BA, and search request is
-- sent to BP, which creates a case in the BP also. When someone responds to
-- that saying, they can offer this ride, onSearch is called to BP for each
-- of these and product/caseProduct is created. Then the customer would
-- confirm one of these product. Which would be basically choosing on of
-- the offers. Only these are cancellable in the BP, which needs to send
-- a notification in BA as a part of onCancel. Similary, when BA is cancelling,
-- cancel should send a notification to the provider who had the ride.
-- This will be basically cancelling the product/caseProduct. Here, when the
-- onCancel comes, it would be ideal not to send a notification in BA. But
-- it's also okay to send this in the first cut. The BA also has a case
-- cancellation flow, which basically cancels the case with or without products.
-- If the case with product is being cancelled, you have to send notification
-- in the BP for each product. Here it would be mostly one product again.
-- When case doesn't have any product, there is no notification.
notifyOnProductCancelCb :: Maybe Text -> Case -> ProductsId -> L.Flow ()
notifyOnProductCancelCb personId c productId =
  if isJust personId
    then do
      person <- Person.findById $ PersonId (fromJust personId)
      case person of
        Just p -> do
          let notificationData =
                FCMData CANCELLED_PRODUCT SHOW FCM.Product $
                  show (_getProductsId productId)
              title = FCMNotificationTitle $ T.pack "Ride cancelled!"
              body =
                FCMNotificationBody $ T.pack $
                  " Cancelled the ride scheduled for "
                    <> formatTime defaultTimeLocale "%T, %F" (Case._startTime c)
                    <> ". Check the app for more details."
          notifyPerson title body notificationData p
        _ -> pure ()
    else pure ()

notifyOnConfirmCb :: Maybe Text -> Case -> Maybe Tracker -> L.Flow ()
notifyOnConfirmCb personId c tracker =
  if isJust personId
    then do
      person <- Person.findById $ PersonId (fromJust personId)
      case person of
        Just p -> do
          let notificationData =
                FCMData CONFIRM_CALLBACK SHOW FCM.Case $
                  show (_getCaseId $ c ^. #_id)
              vehicle_category = case tracker of
                Nothing -> "unknown"
                Just t ->
                  fromMaybe "unknown" $ t ^. #trip ^. #vehicle . _Just . #category
              title = FCMNotificationTitle $ T.pack "Your ride is now confirmed!"
              body =
                FCMNotificationBody $
                  "Your booking for " <> vehicle_category
                    <> T.pack
                      ( " is confirmed for "
                          <> formatTime defaultTimeLocale "%T, %F" (Case._startTime c)
                          <> "."
                      )
          notifyPerson title body notificationData p
        _ -> pure ()
    else pure ()

notifyOnExpiration :: Case -> L.Flow ()
notifyOnExpiration caseObj = do
  let caseId = Case._id caseObj
  let personId = Case._requestor caseObj
  let startTime = Case._startTime caseObj
  if isJust personId
    then do
      person <- Person.findById $ PersonId (fromJust personId)
      case person of
        Just p -> do
          let notificationData =
                FCMData EXPIRED_CASE SHOW FCM.Case $
                  show (_getCaseId caseId)
              title = FCMNotificationTitle $ T.pack "Ride expired!"
              body =
                FCMNotificationBody $ T.pack $
                  "Your ride for "
                    <> formatTime defaultTimeLocale "%T, %F" startTime
                    <> " has expired as there were no replies."
                    <> " You can place a new request to get started again!"
          notifyPerson title body notificationData p
        _ -> pure ()
    else pure ()

notifyOnRegistration :: RegistrationToken -> Person -> L.Flow ()
notifyOnRegistration regToken person =
  let tokenId = RegToken._id regToken
      notificationData =
        FCMData REGISTRATION_APPROVED SHOW FCM.Organization $
          show tokenId
      title = FCMNotificationTitle $ T.pack "Registration Completed!"
      body =
        FCMNotificationBody $
          T.pack "You can now book rides for travel or apply "
            <> "for a travel pass for yourself, family, or for work."
   in notifyPerson title body notificationData person

notifyOnTrackCb :: Maybe Text -> Tracker -> Case -> L.Flow ()
notifyOnTrackCb personId tracker c =
  if isJust personId
    then do
      let caseId = Case._id c
      person <- Person.findById $ PersonId (fromJust personId)
      case person of
        Just p -> do
          let notificationData =
                FCMData TRACKING_CALLBACK SHOW FCM.Case $
                  show caseId
              trip = tracker ^. #trip
              reg_number =
                trip ^. #vehicle . _Just . #registration . _Just . #number
              model =
                fromMaybe reg_number $ trip ^. #vehicle . _Just . #model
              driver_name =
                trip ^. #driver . #persona . _Just . #descriptor . #full_name
              title = FCMNotificationTitle $ T.pack "Ride details updated!"
              body =
                FCMNotificationBody $
                  "Your ride with " <> driver_name <> ", " <> model
                    <> T.pack
                      ( " is scheduled for "
                          <> formatTime defaultTimeLocale "%T, %F" (Case._startTime c)
                          <> ".  You can see more details in the app."
                      )
          notifyPerson title body notificationData p
        _ -> pure ()
    else pure ()

notifyOnSearchCb :: PersonId -> CaseId -> [Products] -> L.Flow ()
notifyOnSearchCb personId caseId products = do
  person <- Person.findById personId
  case person of
    Just p -> do
      let notificationData =
            FCMData SEARCH_CALLBACK SHOW FCM.Case $
              show (_getCaseId caseId)
          title = FCMNotificationTitle $ T.pack "New ride options available!"
          body =
            FCMNotificationBody $ T.pack $
              if length products == 1
                then
                  "You have a new reply for your ride request!"
                    <> " Head to the beckn app for details."
                else
                  "You have " <> show (length products) <> "new ride offers!"
                    <> " Check your options in the beckn app."
      notifyPerson title body notificationData p
    _ -> pure ()
