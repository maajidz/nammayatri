{-# LANGUAGE OverloadedLabels #-}

module Utils.Notifications where

import App.Types
import Beckn.External.FCM.Flow
import Beckn.External.FCM.Types as FCM
import Beckn.Types.App
import Beckn.Types.Mobility.Intent as Intent
import Beckn.Types.Storage.Case as Case
import Beckn.Types.Storage.Person as Person
import Beckn.Types.Storage.ProductInstance as ProductInstance
import Beckn.Types.Storage.RegistrationToken as RegToken
import Beckn.Utils.Common (showTimeIst)
import qualified Data.Text as T
import EulerHS.Prelude

-- | Send FCM "search" notification to provider admins
notifyTransportersOnSearch :: Case -> Intent -> [Person] -> Flow ()
notifyTransportersOnSearch c intent =
  traverse_ (notifyPerson title body notificationData)
  where
    notificationData =
      FCMData SEARCH_REQUEST SHOW FCM.Organization $
        show (_getCaseId $ c ^. #_id)
    title = FCMNotificationTitle $ T.pack "New ride request!"
    variant =
      fromMaybe "Unknown" $ Just $ intent ^. #_vehicle . #variant
    body =
      FCMNotificationBody $
        unwords
          [ "You have a new ride request (" <> variant <> ")",
            "for",
            showTimeIst (Case._startTime c) <> ".",
            "Visit the app to accept or decline the request."
          ]

-- | Send FCM "confirm" notification to provider admins
notifyTransportersOnConfirm :: Case -> ProductInstance -> [Person] -> Flow ()
notifyTransportersOnConfirm c _prodInst admins = do
  let model = fromMaybe "Unknown" $ c ^. #_udf1
  traverse_
    (notifyPerson title (body model) notificationData)
    admins
  where
    notificationData =
      FCMData CONFIRM_REQUEST SHOW FCM.Organization $
        show (_getCaseId $ c ^. #_id)
    title = FCMNotificationTitle $ T.pack "Customer has confirmed the ride!"
    body model =
      FCMNotificationBody $
        unwords
          [ "Customer has accepted your offer for",
            model,
            "dated",
            showTimeIst (Case._startTime c) <> ".",
            "Visit the app to assign a driver."
          ]

-- | Send FCM "cancel" notification to provider admins
notifyTransportersOnCancel :: Case -> [Person] -> Flow ()
notifyTransportersOnCancel c =
  traverse_ (notifyPerson title body notificationData)
  where
    caseId = Case._id c
    notificationData =
      FCMData CANCELLED_PRODUCT SHOW FCM.Product $
        show (_getCaseId caseId)
    title = FCMNotificationTitle $ T.pack "Ride cancelled!"
    body =
      FCMNotificationBody $
        unwords
          [ "Customer had to cancel your ride scehduled for",
            showTimeIst (Case._startTime c) <> ".",
            "Check the app for more details."
          ]

notifyOnRegistration :: RegistrationToken -> Person -> Flow ()
notifyOnRegistration regToken =
  notifyPerson title body notificationData
  where
    tokenId = RegToken._id regToken
    notificationData =
      FCMData REGISTRATION_APPROVED SHOW FCM.Organization $
        show tokenId
    title = FCMNotificationTitle $ T.pack "Registration Completed!"
    body =
      FCMNotificationBody $
        unwords
          [ "Welcome to Beckn Mobility! Click here to view all the open ride",
            "requests. You will be notified whenever a new request comes in."
          ]

notifyTransporterOnExpiration :: Case -> [Person] -> Flow ()
notifyTransporterOnExpiration c =
  traverse_ (notifyPerson title body notificationData)
  where
    notificationData =
      FCMData EXPIRED_CASE SHOW FCM.Case $
        show (_getCaseId $ c ^. #_id)
    title = FCMNotificationTitle $ T.pack "Ride expired!"
    body =
      FCMNotificationBody $
        unwords
          [ "The ride request for",
            showTimeIst (Case._startTime c),
            "has expired as the customer failed to confirm.",
            "You can view more details in the app."
          ]

notifyCancelReqByBP :: ProductInstance -> [Person] -> Flow ()
notifyCancelReqByBP p =
  traverse_ (notifyPerson title body notificationData)
  where
    notificationData =
      FCM.FCMData
        { _fcmNotificationType = FCM.CANCELLED_PRODUCT,
          _fcmShowNotification = FCM.SHOW,
          _fcmEntityIds = show $ _getProductInstanceId $ p ^. #_id,
          _fcmEntityType = FCM.Organization
        }
    title = FCM.FCMNotificationTitle $ T.pack "Driver has cancelled the ride!"
    body =
      FCMNotificationBody $
        unwords
          [ "The ride scheduled for",
            showTimeIst (ProductInstance._startTime p) <> ",",
            "has been cancelled. Check the app for more details."
          ]
