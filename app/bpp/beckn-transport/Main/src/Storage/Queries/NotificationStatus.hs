module Storage.Queries.NotificationStatus where

import Domain.Types.Booking
import Domain.Types.NotificationStatus as NotificationStatus
import Domain.Types.Person (Driver, Person)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.NotificationStatus

createMany :: [NotificationStatus] -> SqlDB ()
createMany = Esq.createMany

updateStatus :: Id Booking -> AnswerStatus -> [Id Driver] -> SqlDB ()
updateStatus bookingId status driverIds =
  Esq.update $ \tbl -> do
    set
      tbl
      [ NotificationStatusStatus =. val status
      ]
    where_ $
      tbl ^. NotificationStatusBookingId ==. val (toKey bookingId)
        &&. tbl ^. NotificationStatusDriverId `in_` valList (toKey . cast <$> driverIds)

fetchActiveNotifications :: Transactionable m => m [NotificationStatus]
fetchActiveNotifications =
  Esq.findAll $ do
    notificationStatus <- from $ table @NotificationStatusT
    where_ $
      notificationStatus ^. NotificationStatusStatus ==. val NotificationStatus.NOTIFIED
    return notificationStatus

findActiveNotificationByRBId :: Transactionable m => Id Booking -> m [NotificationStatus.NotificationStatus]
findActiveNotificationByRBId bookingId =
  Esq.findAll $ do
    notificationStatus <- from $ table @NotificationStatusT
    where_ $
      notificationStatus ^. NotificationStatusBookingId ==. val (toKey bookingId)
        &&. notificationStatus ^. NotificationStatusStatus ==. val NotificationStatus.NOTIFIED
    return notificationStatus

findActiveNotificationByDriverId :: Transactionable m => Id Driver -> Id Booking -> m (Maybe NotificationStatus)
findActiveNotificationByDriverId driverId bookingId =
  Esq.findOne $ do
    notificationStatus <- from $ table @NotificationStatusT
    where_ $
      notificationStatus ^. NotificationStatusBookingId ==. val (toKey bookingId)
        &&. notificationStatus ^. NotificationStatusStatus ==. val NotificationStatus.NOTIFIED
        &&. notificationStatus ^. NotificationStatusDriverId ==. val (toKey $ cast driverId)
    return notificationStatus

cleanupNotifications :: Id Booking -> SqlDB ()
cleanupNotifications bookingId =
  Esq.delete $ do
    notificationStatus <- from $ table @NotificationStatusT
    where_ (notificationStatus ^. NotificationStatusBookingId ==. val (toKey bookingId))

cleanupOldNotifications :: SqlDB Int
cleanupOldNotifications = do
  compareTime <- getCurrentTime <&> addUTCTime (-300) -- We only remove very old notifications (older than 5 minutes) as a fail-safe
  res <- Esq.deleteReturningCount $ do
    notificationStatus <- from $ table @NotificationStatusT
    where_ (notificationStatus ^. NotificationStatusExpiresAt ==. val compareTime)
  return $ fromIntegral res

deleteByPersonId :: Id Driver -> SqlDB ()
deleteByPersonId personId =
  Esq.delete $ do
    notificationStatuses <- from $ table @NotificationStatusT
    where_ $ notificationStatuses ^. NotificationStatusDriverId ==. val (toKey . cast @Driver @Person $ personId)
